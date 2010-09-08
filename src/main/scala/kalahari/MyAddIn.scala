package kalahari

class Kalahari{
}
object Kalahari{
	def myabs(x : Double) : Double = x.abs
	def myarray(x : Double) : Array[Double] = {
		Array(x + 1, x + 2, x + 3)
	}
	def mymatrix(x : Double) : Array[Array[Double]] = {
		(1 to 3).toArray.map{
			i =>
				(for (j <- 0 to 5)
					yield x * i + j).toArray
	}}
	def mysum(arr : Array[Double]) : Double = {
		(0.0 /: arr)(_+_)
	}
	def mysum2(arr : Array[Any]) : Double = {
		(0.0 /: arr.filter(_.isInstanceOf[Double]).map(_.asInstanceOf[Double]))(_+_)
	}

	def splineSurface(
		xAxis : Array[Double], 
		yAxis : Array[Double], 
		knownValues : Array[Array[Any]]) : Array[Array[Double]] = {
		
		splineSurfaceWithNewAxes(xAxis, yAxis, knownValues, xAxis, yAxis)
	}

	def splineSurfaceWithNewAxes(
		originalXAxis : Array[Double],
		originalyAxis : Array[Double],
		knownValues : Array[Array[Any]],
		newXAxis : Array[Double],
		newYAxis : Array[Double]
	) : Array[Array[Double]] = {

		var points = List[(Double, Double, Double)]()
		originalyAxis.zip(knownValues).foreach{
			case (y, rowVec) =>
				originalXAxis.zip(rowVec).foreach{
					case (x, z) =>
						if (z.isInstanceOf[Double])
							points = (x, y, z.asInstanceOf[Double]) :: points
			}
		}
		if (points.isEmpty){
			Array.fill(newYAxis.size)(Array.fill(newXAxis.size)(0.0))
		} else {
			val surface = new Surface(points).fillInGaps
			newYAxis.map{
				y =>
					newXAxis.map{
						x =>
							surface(x, y)
					}
			}
		}
	}
	def inputMatrix(m : Array[Array[Any]]) : Double = {
		m.foreach{
			vec =>
				println("Vec = " + vec.mkString(", "))
		}
		37
	}
		
}
