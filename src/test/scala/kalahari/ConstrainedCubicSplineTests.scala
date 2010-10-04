package kalahari

import scala.collection.immutable.NumericRange
import org.scalatest.FunSuite

class ConstrainedCubicSplineTests extends FunSuite with TestUtils{
	
	def extrapolateFlat(
		x : Array[Double], 
		y : Array[Double], 
		fn : Double => Double
	) : (Double => Double) = {
		x_ : Double =>{
			if (x_ <= x(0))
				y(0)
			else if (x_ >= x.last)
				y.last
			else
				fn(x_)
		}
	}

	def testRange(
		range : NumericRange[Double], 
		cs : ConstrainedCubicSpline, 
		fn : Double => Double,
		tol : Double = 0.001
	){
		for (x <- range){
			assert((fn(x) - cs(x)) < 1e-3, "Expected " + fn(x) + ", got " + cs(x))
		}
	}

	test("straight line"){
		val x = Array(0.0, 1.0, 2.0, 3.0)
		val y = Array(0.0, 1.0, 2.0, 3.0)
		val cs = new ConstrainedCubicSpline(x, y)
    (0.0 to 3.0 by 0.01).foreach{
      case x =>
        assert((x - cs(x)) < 1e-3, "Expected " + x + ", got " + cs(x))
    }
	}	

	test("log"){
		val x = (1.0 to 5.0 by 0.1).toArray
		val y = x.map(math.log)
		val cs = new ConstrainedCubicSpline(x, y)
		testRange(
			1.0 to 5.0 by 0.01,
			cs,
			extrapolateFlat(x, y, math.log)
		)
	}

}
