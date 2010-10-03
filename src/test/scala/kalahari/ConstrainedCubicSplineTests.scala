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
		cs : ConstrainedCurve, 
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
		val cs = new ConstrainedBasisCurve(x, y)
    (0.0 to 3.0 by 0.01).foreach{
      case x =>
        assert((x - cs(x)) < 1e-3, "Expected " + x + ", got " + cs(x))
    }
	}	

	test("log"){
		val x = (1.0 to 5.0 by 0.1).toArray
		val y = x.map(math.log)
		val cs = new ConstrainedBasisCurve(x, y)
		testRange(
			1.0 to 5.0 by 0.01,
			cs,
			extrapolateFlat(x, y, math.log)
		)
	}

	test("implied points"){
		val curve = new ConstrainedBasisCurve(
			Map(
				0.0 -> 0.0,
				1.0 -> 1.0,
				2.0 -> 1.5,
				3.0 -> 1.8
			)
		)
		for ((t0, t1) <- List((0.75, 2.2), (1.1, 1.9), (0.1, 1.3))){
			val y0 = curve(t0)
			val y1 = curve(t1)
			val T = t1 - t0
			val y = curve(t0, T)
			val y1_imp = ConstrainedCurve.impliedBackFromForwardAndFront(
				new BasisSpread(t0, t1, y),
				new SpotBasisSpread(t0, y0)
			).rate
			assert((y1_imp - y1).abs < ConstrainedCurve.eps, "Not close enough " + y1_imp + " vs " + y1)
			val y0_imp = ConstrainedCurve.impliedFrontFromForwardAndBack(
				new BasisSpread(t0, t1, y),
				new SpotBasisSpread(t1, y1)
			).rate
			assert((y0_imp - y0).abs < ConstrainedCurve.eps, "Not close enough " + y0_imp + " vs " + y0)
		}
	}

}
