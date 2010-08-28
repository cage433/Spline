package kalahari

import org.scalatest.FunSuite

class CurveTests extends FunSuite with TestUtils{


  test("Curve life cycle"){
    var curve = Curve()
    assert(curve.isEmpty)

    // Add a single point makes a constant curve
    curve += (2, 4)
    assertDoubleEquals(4, curve(5))

    // Add another point - note should be flat outside [2, 4]
    curve += (4, 5)
    assertDoubleEquals(5, curve(4))
    assertDoubleEquals(4.5, curve(3))
    assertDoubleEquals(4.0, curve(0.0))
  }

}
