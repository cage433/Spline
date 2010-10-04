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
{
	require(ts.size == zs.size, "Axes need to be the same size")
	require(ts.toList == ts.toList.sortWith(_<_), "time axis must be sorted")

	val spline = new ConstrainedCubicSpline(ts, zs)
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

	def apply(period : Period) : (Double, Double) = (
		apply(period.t0),
		apply(period.t1)
	)

}
