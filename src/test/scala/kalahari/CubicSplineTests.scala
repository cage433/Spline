package kalahari

import org.scalatest.FunSuite

class CubicSplineTests extends FunSuite with TestUtils{

  test("Log curve interpolates ok"){
    val xs = (1.0 to 5.0 by 0.1).toArray
    val ys = xs.map(math.log)
    val cs = CubicSpline(xs, ys)
    // test goes through supplied points
    xs.zip(ys).foreach{
      case (x, y) => 
        assert((y - cs(x)).abs < 1e-9, "Expected " + y + ", got " + cs(x))
    }
    (1.0 to 5.0 by 0.01).foreach{
      case x =>
        assert((math.log(x) - cs(x)) < 1e-3, "Expected " + math.log(x) + ", got " + cs(x))
    }
  }

  test("Two point curve is linear"){
    val xs = Array(1.0, 2.0)
    def fn(x : Double) = 1.0 + 3.0 * x
    val ys = xs.map(fn)
    val cs = CubicSpline(xs, ys)
    (1.0 to 2.0 by 0.1).foreach{
      case x =>
        assert((fn(x) - cs(x)) < 1e-9, "Expected " + fn(x) +  ", got " + cs(x))
    }
  }

  test("One point curve is constant"){
    val xs = Array(1.0)
    val ys = Array(2.3)
    val cs = CubicSpline(xs, ys)
    (1.0 to 2.0 by 0.1).foreach{
      case x =>
        assert((2.3 - cs(x)) < 1e-9, "Expected " + 2.3 +  ", got " + cs(x))
    }
  }


  test("Derivative is as required at endpoints"){
    def deriv(fn : Double => Double)(x0 : Double, dx : Double = 1e-6) : Double = {
      (fn (x0 + dx) - fn(x0 - dx)) / (2.0 * dx)
    }
    def testSplineEndpoints(xs : Array[Double], fn : Double => Double, yp1 : Double, ypn : Double){
      val spline = new CubicSpline(xs, xs.map(fn), Some(yp1), Some(ypn))
      assertDoubleEquals(yp1, deriv(spline(_))(xs.head))
      assertDoubleEquals(ypn, deriv(spline(_))(xs.last))
    }
    testSplineEndpoints(Array(1.0, 2.0, 3.0), math.sin, 0.0, 0.5)
    testSplineEndpoints(Array(1.0, 3.0), math.sin, 0.0, 0.5)
  }
}
