package kalahari

class Kalahari{
}
object Kalahari{

	def constrainedSplineSurface(
		yAxis : Array[Double],
		knownValues : Array[Any],
		newXAxis : Array[Double],
		newYAxis : Array[Double]
	) : Array[Array[Double]] = {

		var points = Map[Double, Double]()
		yAxis.zip(knownValues).foreach{
			case (y, z) =>
				if (z.isInstanceOf[Double])
					points = points + (y ->  z.asInstanceOf[Double])
		}
		val cs = new ConstrainedBasisSurface(points)
		newYAxis.map{
			y =>
				newXAxis.map{
					x =>
						cs.forwardRate(x, x + y)
				}
		}
	}
}
