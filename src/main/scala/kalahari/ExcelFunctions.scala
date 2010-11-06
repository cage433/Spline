package kalahari

class ExcelFunctions{
}
object ExcelFunctions{

  private def buildSurface(yAxis : Array[Double], knownValues : Array[Any]) = {
		var points = Map[Double, Double]()
		yAxis.zip(knownValues).foreach{
			case (y, z) =>
				if (z.isInstanceOf[Double])
					points = points + (y ->  z.asInstanceOf[Double])
		}
		val cs = new ConstrainedBasisSurface(points)
		cs
  }

  private def buildSurfaceMatrix(
    cs : ConstrainedBasisSurface, 
    newXAxis : Array[Double], 
    newYAxis : Array[Double]
  ) : Array[Array[Double]] = {
		return newYAxis.map{
			y =>
				newXAxis.map{
					x =>
						cs.forwardRate(x, x + y)
				}
		}
  }

	def constrainedSplineSurface(
		yAxis : Array[Double],
		knownValues : Array[Any],
		newXAxis : Array[Double],
		newYAxis : Array[Double]
	) : Array[Array[Double]] = {
	  
    val cs = buildSurface(yAxis, knownValues)
    return buildSurfaceMatrix(cs, newXAxis, newYAxis)
	}

  def splineSurfaceWithForwardPointsAdded(
    timeAxis : Array[Double],
    knownValues : Array[Any],
    forwardRates : Array[Array[Any]],
    alpha : Double,
    newXAxis : Array[Double],
    newYAxis : Array[Double]
  ) : Array[Array[Double]] = {
    var cs = buildSurface(timeAxis, knownValues)
    forwardRates.foreach{
      row =>
        if (row.forall(_.isInstanceOf[Double])){
          val (t0, t1, zFwd) = (row(0).asInstanceOf[Double], row(1).asInstanceOf[Double], row(2).asInstanceOf[Double])
          cs = cs.addForwardRate(t0, t1, zFwd, alpha)
        }
    }
    return buildSurfaceMatrix(cs, newXAxis, newYAxis)
    
  }
}
