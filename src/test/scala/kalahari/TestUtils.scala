package kalahari

trait TestUtils{

  def assertDoubleEquals(expected : Double, actual : Double, tol : Double = 1e-9) = {
    assert((expected - actual).abs < tol, "Expected " + expected + ", actual " + actual)
  }
}

