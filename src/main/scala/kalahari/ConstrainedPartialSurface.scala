package kalahari

class ConstrainedCubicSurface(
	xAxis : Array[Double],
	yAxis : Array[Double],
	z : Map[(Double, Double), Double],
	newXAxis : Array[Double],
	newYAxis : Array[Double]
){
	val curve = yAxis.flatMap{ 
		y_ => 
			val pt = (0.0, y_)
			if (z.contains(pt))
				Some((y_, z(pt)))
			else
				None
	}.toArray

	val cs = {
		val (xs, ys) = curve.unzip
		new ConstrainedBasisCurve(xs.toArray, ys.toArray)
	}

	def surface : Array[Array[Double]] = {
		newYAxis.map{
			y_ =>
				newXAxis.map{
					x_ =>
						cs(x_, y_)
				}
		}
	}

}
