package kalahari

class ConstrainedCubicSpline(xs : Array[Double], ys : Array[Double]){

	def fencePostArray() = Array.fill(xs.size)(0.0)
	def fenceWireArray() = Array.fill(xs.size - 1)(0.0)

	val D = fencePostArray()
	def dx(i : Int) : Double = xs(i + 1) - xs(i)
	def dy(i : Int) : Double = ys(i + 1) - ys(i)
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
		val x0 = xs(i)
		val x1 = xs(i + 1)
		c(i) = ( x1 * L(i) - x0 * R(i) ) / (2.0 * dx(i))
		b(i) = ( dy(i) - c(i) * (x1 * x1 - x0 * x0) - d(i) * (x1 * x1 * x1 - x0 * x0 * x0) ) / dx(i)
		a(i) = ys(i) - b(i) * x0 - c(i) * x0 * x0 - d(i) * x0 * x0 * x0
	}

	def apply(x_ : Double) : Double = {
		if (xs.size == 0)
			0.0
		else if (xs.size == 1)
			ys(0)
		else if (x_ <= xs(0))
			ys(0)
		else if (x_ >= xs(n))
			ys(n)
		else if (xs.size == 2){
			(ys(1) - ys(0)) / (xs(1) - xs(0)) * (x_ - xs(0)) + ys(0)
		}
		else {
			val i = xs.lastIndexWhere(_ < x_)
			a(i) + b(i) * x_ + c(i) * x_ * x_ + d(i) * x_ * x_ * x_
		}
	}
}
	
