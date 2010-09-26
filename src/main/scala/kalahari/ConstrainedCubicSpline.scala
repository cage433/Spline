package kalahari

trait ConstrainedCurve{
	val eps = ConstrainedCurve.eps
	def canAddPoint(x1 : Double, x2 : Double, y : Double) : Boolean
	def addPoint(x1 : Double, x2 : Double, y : Double) : ConstrainedCurve
	def apply(x : Double) : Double
	def apply(t0 : Double, T : Double) : Double = {
		val t1 = t0 + T
		val y0 = apply(t0)
		val y1 = apply(t1)
		ConstrainedCurve.impliedForwardFromFrontAndBack(t0, y0, t1, y1)
	}
	def almostEqual(x0 : Double, x1 : Double) = (x0 - x1).abs < eps
	def distance(x1 : Double, x2 : Double, y : Double) : Double
}

object ConstrainedCurve{
	val eps = 1e-6

	def impliedFrontFromForwardAndBack(tBack : Double, yBack : Double, T : Double, y : Double) : Double = {
		val tFront = tBack - T
		assert(tFront > 0, "Front time <= 0, " + tFront)

		(yBack * tBack - y * T) / tFront
	}

	def impliedBackFromForwardAndFront(tFront : Double, yFront : Double, T : Double, y : Double) : Double = {
		val tBack = tFront + T
		(yFront * tFront + y * T) / tBack
	}

	def impliedForwardFromFrontAndBack(
		tFront : Double, 
		yFront : Double,
		tBack : Double,
		yBack : Double
	) : Double = {
		(yBack * tBack - yFront * tFront) / (tBack - tFront)
	}
}

object EmptyConstrainedCurve extends ConstrainedCurve{
	def canAddPoint(x1 : Double, x2 : Double, y : Double)  = true
	def addPoint(x1 : Double, x2 : Double, y : Double) = {
		new ProperConstrainedSpline(Array(x1, x2).sortWith(_<_), Array(y, y))
	}
	def apply(x : Double) = 0

	// So that earliest points are added first
	def distance(x1 : Double, x2 : Double, y : Double) = x1
}

class SinglePointCurve(x : Double, y : Double) extends ConstrainedCurve{
	def canAddPoint(x1 : Double, x2 : Double, y_ : Double)  = {
		x < x1 - eps || x > x2 + eps
	}
	
	def addPoint(x1 : Double, x2 : Double, y_ : Double) = {
		new ProperConstrainedSpline(Map(x -> y, x1 -> y_, x2 -> y_))
	}
	def apply(x_ : Double) = y
	// So that earliest points are added first
	def distance(x1 : Double, x2 : Double, y : Double) = x1
}	


class ProperConstrainedSpline(x : Array[Double], y : Array[Double])
	extends ConstrainedCurve
{
	require(x.size >= 2, "x axis needs at least two points")
	require(x.size == y.size, "Axes need to be the same size")
	require(x.toList == x.toList.sortWith(_<_), "x axis must be sorted")


	def this(map : Map[Double, Double]) = this(
		map.keys.toArray.sortWith(_<_),
		map.keys.toArray.sortWith(_<_).map(map)
	)

	def leftAndRightBounds(x1 : Double, x2 : Double, y_ : Double) : (Bound, Bound) = {
		var bound1 = bound(x1)
		var bound2 = bound(x2)
		val y1 = apply(x1)
		val y2 = apply(x2)
		val rImplied = bound1.map(ConstrainedCurve.impliedBackFromForwardAndFront(x1, _, x2 - x1, y_))
		bound2 = rImplied.intersection(bound2)
		val lImplied = bound2.map(ConstrainedCurve.impliedFrontFromForwardAndBack(x2, _, x2 - x1, y_))
		bound1 = lImplied intersection bound1
		(bound1, bound2)
	}
		
	def canAddPoint(x1 : Double, x2 : Double, y_ : Double) = {
		val (l, r) = leftAndRightBounds(x1, x2, y_) 
		!l.isEmpty && ! r.isEmpty
	}

	def distance(x1 : Double, x2 : Double, y_ : Double) = {
		assert(canAddPoint(x1, x2, y_))
		val (bound1, bound2) = leftAndRightBounds(x1, x2, y_) 
		val y1 = apply(x1)
		val newY1 = bound1.closestPoint(y1)
		(y1 - newY1).abs
	}

	// for testing only
		

	def addPoint(x1 : Double, x2 : Double, y_ : Double) = {
		assert(canAddPoint(x1, x2, y_))
		val (bound1, bound2) = leftAndRightBounds(x1, x2, y_) 
		val y1 = apply(x1)
		val y2 = apply(x2)

		val newY1 = bound1.closestPoint(y1)
		val newY2 = y_ * (x2 - x1) / x2 - y1 * x1 / x2
		new ProperConstrainedSpline(
			Map(x1 -> newY1, x2 -> newY2) ++ x.zip(y)
		)
	} 

	private def sign(x : Double) = {
		if (x < 0)
			-1
		else if (x > 0)
			1
		else
			0
	}

	private lazy val leftSlope = sign( y(1) - y(0) ) 
	private lazy val rightSlope = sign( y(n) - y(n - 1) ) 

	def bound(x_ : Double) : Bound = {
		if (x_ <= x(0)){
			leftSlope match {
				case -1 => LowerBound(y(0))
				case 0 => TwoSidedBound(y(0), y(0))
				case 1 => UpperBound(y(0))
			}
		} else if (x_ >= x(n)) {
			rightSlope match {
				case -1 => UpperBound(y.last)
				case 0 => TwoSidedBound(y.last, y.last)
				case 1 => LowerBound(y.last)
			}
		} else {
			val i = x.lastIndexWhere(_ < x_)
			TwoSidedBound(y(i) min y(i + 1), y(i) max (y(i + 1)))
		}
	}

	val n = x.size - 1
	val fencePosts = 0 to n
	val midPosts = 1 to n - 1
	val fenceWires = 0 to n - 1


	def fencePostArray() = Array.fill(x.size)(0.0)
	def fenceWireArray() = Array.fill(x.size - 1)(0.0)

	val D = fencePostArray()
	def dx(i : Int) : Double = x(i + 1) - x(i)
	def dy(i : Int) : Double = y(i + 1) - y(i)
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
		val x0 = x(i)
		val x1 = x(i + 1)
		c(i) = ( x1 * L(i) - x0 * R(i) ) / (2.0 * dx(i))
		b(i) = ( dy(i) - c(i) * (x1 * x1 - x0 * x0) - d(i) * (x1 * x1 * x1 - x0 * x0 * x0) ) / dx(i)
		a(i) = y(i) - b(i) * x0 - c(i) * x0 * x0 - d(i) * x0 * x0 * x0
	}

	def apply(x_ : Double) : Double = {
		if (x_ <= x(0))
			y(0)
		else if (x_ >= x(n))
			y(n)
		else {
			val i = x.lastIndexWhere(_ < x_)
			a(i) + b(i) * x_ + c(i) * x_ * x_ + d(i) * x_ * x_ * x_
		}
	}
}

object ConstrainedCubicSpline{
	def apply(x : Array[Double], y : Array[Double]) : ConstrainedCurve = {
		x.size match {
			case 0 => EmptyConstrainedCurve
			case 1 => new SinglePointCurve(x(0), y(0))
			case _ => new ProperConstrainedSpline(x, y)
		}
	}
}
