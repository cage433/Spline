package kalahari
import org.scalatest.FunSuite

class BasisSurfaceTests extends FunSuite with TestUtils{
  val ts = Array(1.0, 2.0, 3.0, 5.0)
  val zs = Array(1.3, 1.5, 1.5, 2.0)

  val curve = new ConstrainedBasisSurface(ts, zs)
  test("Stay within bounds"){
    for( t <- 1.1 to 1.9 by 0.1;
        alpha <- List(0.1, 0.5, 0.9)){
      val (low, high) = curve.rateBound(t, alpha)
        assert(low >= 1.3)
        assert(high <= 1.5)
    }
    for( t <- 2.1 to 2.9 by 0.1;
        alpha <- List(0.1, 0.5, 0.9)){
      val (low, high) = curve.rateBound(t, alpha)
        assert(low === 1.5)
        assert(high === 1.5)
    }

  }

  test("Alpha relaxes bounds"){
    val alphas = List(0.1, 0.5, 0.9)
    val bounds = alphas.map(curve.rateBound(4.5, _))
    bounds.zip(bounds.tail).foreach{
      case ((l1, h1), (l2, h2)) =>
        assert(l1 > l2)
        assert(h1 < h2)
    }
  }

  test("Bounds exact at node"){
    val (low, high) = curve.rateBound(2.0, 0.5)
    assert(low === 1.5)
    assert(high === 1.5)
  }

  def checkRatesConsistent(t0 : Double, t1 : Double, z0 : Double, z1 : Double, zFwd : Double, alpha : Double){
    def checkRateBound(t : Double, z : Double){
      val (zLow, zHigh) = curve.rateBound(t, alpha)
      assert(zLow <= z)
      assert(z <= zHigh)
    }
    checkRateBound(t0, z0)
    checkRateBound(t1, z1)
    assert(
      zFwd === RateCalculator.fwdRateFromSpot(t0, t1, z0, z1)
    )
  }
  test("Best fitting spot rates with fwd node fixed"){
    val t0 = 2.0
    val t1 = 4.0
    val alpha = 0.5
    val (zfLow, zfHigh) = curve.forwardRateBounds(t0, t1, alpha)
    assert(zfLow + 0.1 < zfHigh)
    val zFwd = zfLow + 0.75 * (zfHigh - zfLow)
    val (z0, z1) = curve.bestFittingSpotRates(t0, t1, zFwd, alpha)
    assert(z0 === curve.spotRate(t0))
    checkRatesConsistent(t0, t1, z0, z1, zFwd, alpha)
  }

  test("Best fitting spot rates with back node fixed"){
    val t0 = 1.5
    val t1 = 3.0
    val alpha = 0.5
    val (zfLow, zfHigh) = curve.forwardRateBounds(t0, t1, alpha)
    assert(zfLow + 0.05 < zfHigh)
    val zFwd = zfLow + 0.75 * (zfHigh - zfLow)
    val (z0, z1) = curve.bestFittingSpotRates(t0, t1, zFwd, alpha)
    assert(z1 === curve.spotRate(t1))
    checkRatesConsistent(t0, t1, z0, z1, zFwd, alpha)
  }

  test("Arbs are correct"){
    import RateCalculator._
    val (t0, t1) = (0.5, 1.7)
    val (z0, z1) = (1.2, 1.4)
    val zFwd = fwdRateFromSpot(t0, t1, z0, z1)
    assertDoubleEquals(z0, frontRateFromFwdAndBack(t0, t1, zFwd, z1))
    assertDoubleEquals(z1, backRateFromFwdAndFront(t0, t1, zFwd, z0))
  }

  test("Test fwd fitting with two free nodes"){
    val t0 = 1.5
    val t1 = 4.0
    val alpha = 0.5
    val (zfLow, zfHigh) = curve.forwardRateBounds(t0, t1, alpha)
    assert(zfLow + 0.05 < zfHigh)
    val zFwd = zfLow + 0.75 * (zfHigh - zfLow)
    val (z0, z1) = curve.bestFittingSpotRates(t0, t1, zFwd, alpha)
    checkRatesConsistent(t0, t1, z0, z1, zFwd, alpha)

    // Chech relative differences from curve
    val A = curve.rateBoundWidth(t0, alpha)
    val B = curve.rateBoundWidth(t1, alpha)
    assert((z0 - curve.spotRate(t0)).abs / A === (z1 - curve.spotRate(t1)).abs / B)
  }

  test("Adding fwd rate to curve yields same fwd rate"){
    val t0 = 1.5
    val t1 = 4.0
    val alpha = 0.5
    val (zfLow, zfHigh) = curve.forwardRateBounds(t0, t1, alpha)
    assert(zfLow + 0.05 < zfHigh)
    val zFwd = zfLow + 0.75 * (zfHigh - zfLow)
    val newCurve = curve.addForwardRate(t0, t1, zFwd, alpha)
    assert(zFwd === newCurve.forwardRate(t0, t1))
    assert(zFwd != curve.forwardRate(t0, t1))
  }

}
