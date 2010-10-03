package kalahari

object MinimiseFunction{
	def apply(
		fn : Double => Double, 
		xLow : Double, 
		xHigh : Double, 
		yLow : Option[Double] = None,
		yHigh : Option[Double] = None,
		eps : Double = 1e-5
	) : Double = {
		val xMid = 0.5 * (xLow + xHigh)
		if ((xHigh - xLow) < eps)
			xMid
		else {
			val y0 = yLow.getOrElse(fn(xLow))
			val y1 = yHigh.getOrElse(fn(xHigh))
			val yMid = fn(xMid)
			if (y0 < y1)
				apply(fn, xLow, xMid, yLow, Some(yMid))
			else
				apply(fn, xMid, xHigh, Some(yMid), yHigh)
		}
	}
}
		
