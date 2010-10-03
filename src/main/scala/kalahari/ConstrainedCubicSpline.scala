package kalahari

class Period(val t0 : Double, val t1 : Double){
	def contains (t : Double) = t0 <= t && t <= t1
	def toList = List(t0, t1)
}
class BasisSpread(t0 : Double, t1 : Double, val rate : Double)
	extends Period(t0, t1)
{
	val T = t1 - t0
}
class SpotBasisSpread(val t : Double, rate : Double) 
	extends BasisSpread(0, t, rate)

trait ConstrainedCurve{
	val eps = ConstrainedCurve.eps
	def canAddPoint(spread : BasisSpread) : Boolean
	def addPoint(spread : BasisSpread) : ConstrainedCurve
	def apply(x : Double) : Double
	def apply(t0 : Double, T : Double) : Double = {
		val t1 = t0 + T
		val y0 = apply(t0)
		val y1 = apply(t1)
		ConstrainedCurve.impliedForwardFromFrontAndBack(
			new SpotBasisSpread(t0, y0), 
			new SpotBasisSpread(t1, y1)
		).rate
	}
	def almostEqual(x0 : Double, x1 : Double) = (x0 - x1).abs < eps
}

object ConstrainedCurve{
	val eps = 1e-6

	def impliedFrontFromForwardAndBack(fwd : BasisSpread, back : SpotBasisSpread) : SpotBasisSpread = {
		new SpotBasisSpread(fwd.t0, (back.rate * back.t - fwd.rate * fwd.T) / fwd.t0)
	}

	def impliedBackFromForwardAndFront(fwd : BasisSpread, front : SpotBasisSpread) : SpotBasisSpread = {
		new SpotBasisSpread(fwd.t1, (front.rate * front.t + fwd.rate * fwd.T) / fwd.t1)
	}

	def impliedForwardFromFrontAndBack(
		front : SpotBasisSpread,
		back : SpotBasisSpread
	) : BasisSpread = {
		new BasisSpread(
			front.t, back.t, (back.rate * back.t - front.rate * front.t) / (back.t - front.t)
		)
	}
}

class ConstrainedBasisCurve(val ts : Array[Double], val zs : Array[Double])
	extends ConstrainedCurve
{
	require(ts.size == zs.size, "Axes need to be the same size")
	require(ts.toList == ts.toList.sortWith(_<_), "time axis must be sorted")

	def this(map : Map[Double, Double]) = this(
		map.keys.toArray.sortWith(_<_),
		map.keys.toArray.sortWith(_<_).map(map)
	)

	def spotRate(t : Double) = new SpotBasisSpread(t, apply(t))
	def forwardRate(period : Period) : BasisSpread = {
		ConstrainedCurve.impliedForwardFromFrontAndBack(
			spotRate(period.t0),
			spotRate(period.t1)
		)
	}

	def maxMinSpreads(period : Period) : (BasisSpread, BasisSpread)  = {
		def boundingPeriod(t : Double) : Period = {
			val t0 = ts.lastIndexWhere(_ <= t) match {
				case -1 => ts(0)
				case i => ts(i)
			}
			val t1 = ts.find(_ >= t) match {
				case Some(t_) => t_
				case None => ts.last
			}
			new Period(t0, t1)
		}
		val (za, zb) = apply(boundingPeriod(period.t0))
		val (zc, zd) = apply(boundingPeriod(period.t1))
		val minFwd = ConstrainedCurve.impliedForwardFromFrontAndBack(
			new SpotBasisSpread(period.t0, za max zb),
			new SpotBasisSpread(period.t1, zc min zd)
		).rate
		val maxFwd = ConstrainedCurve.impliedForwardFromFrontAndBack(
			new SpotBasisSpread(period.t0, za min zb),
			new SpotBasisSpread(period.t1, zc max zd)
		).rate
		(new BasisSpread(period.t0, period.t1, minFwd), new BasisSpread(period.t0, period.t1, maxFwd))
	}

	def apply(period : Period) : (Double, Double) = (
		apply(period.t0),
		apply(period.t1)
	)

	def canAddPoint(spread : BasisSpread) : Boolean = {
		val (minSpread, maxSpread) = maxMinSpreads(spread)
		minSpread.rate <= spread.rate && spread.rate <= maxSpread.rate
	}

	def addPoint(spread : BasisSpread) : ConstrainedCurve = {
		assert(canAddPoint(spread))
		val z0 = apply(spread.t0)
		val z1 = apply(spread.t1)


		return this
	} 

	val n = ts.size - 1
	val fencePosts = 0 to n
	val midPosts = 1 to n - 1
	val fenceWires = 0 to n - 1


	def fencePostArray() = Array.fill(ts.size)(0.0)
	def fenceWireArray() = Array.fill(ts.size - 1)(0.0)

	val D = fencePostArray()
	def dx(i : Int) : Double = ts(i + 1) - ts(i)
	def dy(i : Int) : Double = zs(i + 1) - zs(i)
	def diff(i : Int) : Double = {dy(i) / dx(i)}

	for (i <- midPosts){
		D(i) = {
			val l : Double = diff(i - 1)
			val r : Double = diff(i)
			if ( (diff(i - 1) * diff(i)) <= 0)
				0.0
			else
				2.0 / (1.0 / diff(i - 1) + 1.0 / diff(i))
		}
	}

		
	D(0) = 3.0 * diff(0)/ 2.0 - D(1) / 2.0
	D(n) = 3.0 * diff(n - 1) / 2.0  - D(n - 1) / 2.0

	val L = fenceWireArray()
	val R = fenceWireArray()

	for(i <- fenceWires){
	L(i) = - 2.0 * (D(i + 1) + 2.0 * D(i)) / dx(i) + 6.0 * diff(i) / dx(i)
	R(i) = 2.0 * (2.0 * D(i + 1) + D(i)) / dx(i) - 6.0 * diff(i) / dx(i)
	}

	// Now build equations
	val a = fenceWireArray()
	val b = fenceWireArray()
	val c = fenceWireArray()
	val d = fenceWireArray()

	for (i <- fenceWires){
		d(i) = (R(i) - L(i)) / (6.0 * dx(i))
		val x0 = ts(i)
		val x1 = ts(i + 1)
		c(i) = ( x1 * L(i) - x0 * R(i) ) / (2.0 * dx(i))
		b(i) = ( dy(i) - c(i) * (x1 * x1 - x0 * x0) - d(i) * (x1 * x1 * x1 - x0 * x0 * x0) ) / dx(i)
		a(i) = zs(i) - b(i) * x0 - c(i) * x0 * x0 - d(i) * x0 * x0 * x0
	}

	def apply(x_ : Double) : Double = {
		if (ts.size == 0)
			0.0
		else if (ts.size == 1)
			zs(0)
		else if (x_ <= ts(0))
			zs(0)
		else if (x_ >= ts(n))
			zs(n)
		else if (ts.size == 2){
			(zs(1) - zs(0)) / (ts(1) - ts(0)) * (x_ - ts(0)) + zs(0)
		}
		else {
			val i = ts.lastIndexWhere(_ < x_)
			a(i) + b(i) * x_ + c(i) * x_ * x_ + d(i) * x_ * x_ * x_
		}
	}
}

