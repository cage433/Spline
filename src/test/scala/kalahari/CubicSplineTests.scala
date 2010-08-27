package kalahari

import org.scalatest.FunSuite

class CubicSplineTests extends FunSuite{

  test("Log curve interpolates ok"){
    val xs = (1.0 to 5.0 by 0.1).toArray
    val ys = xs.map(math.log)
    val cs = new CubicSpline(xs, ys)
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
    val cs = new CubicSpline(xs, ys)
    (1.0 to 2.0 by 0.1).foreach{
      case x =>
        assert((fn(x) - cs(x)) < 1e-9, "Expected " + fn(x) +  ", got " + cs(x))
    }
  }
}
