package kalahari


object RateCalculator{
  def fwdRateFromSpot(t0 : Double, t1 : Double, z0 : Double, z1 : Double) = {
    (z1 * t1 - z0 * t0) / (t1 - t0)
  }
  def frontRateFromFwdAndBack(t0 : Double, t1 : Double, zFwd : Double, z1 : Double) = {
    (z1 * t1 - zFwd * (t1 - t0)) / t0
  }
  def backRateFromFwdAndFront(t0 : Double, t1 : Double, zFwd : Double, z0 : Double) = {
    (z0 * t0 + zFwd * (t1 - t0)) / t1
  }
}

/**
 * Creates a basis surface from a strip of zero-start basis swaps
 */
class BasisSurface(val zeroStartTimes : Array[Double], val zeroStartSpreads : Array[Double])
{
  import RateCalculator._
	require(zeroStartTimes.size == zeroStartSpreads.size, "Axes need to be the same size")
	require(zeroStartTimes.toList == zeroStartTimes.toList.sortWith(_<_), "time axis must be sorted")

	def this(map : Map[Double, Double]) = this(
		map.keys.toArray.sortWith(_<_),
		map.keys.toArray.sortWith(_<_).map(map)
	)

	private val spline = new ConstrainedCubicSpline(zeroStartTimes, zeroStartSpreads)

  /**
   * Interpolates to find a zero-start rate
   */
	def spotRate(t : Double) : Double = spline(t)

  /**
   * Determines a forward-start rate from two zero-start rates
   */
	def forwardRate(t0 : Double, t1 : Double) : Double = {
		val bs0 = spotRate(t0)
		val bs1 = spotRate(t1)
		(bs1 * t1 - bs0 * t0) / (t1 - t0)
	}

  /**
   * Find the two times in our zero-start times that bound this. Used
   * to determine the bounding box used to fit a forward start rate
   */
  private def boundingTimes(t : Double) = {
    val i = zeroStartTimes.findIndexOf(_ > t)
    if (i < 1)
      throw new Exception("Time " + t + " is not bounded by two others in range " + zeroStartTimes)
    (zeroStartTimes(i - 1), zeroStartTimes(i))
  }

  /**
   * We fit forward start spreads by adding two zero-start points to this surface (if possible). This
   * tells us the min/max rates we can pick for this time point. It depends on the relaxation parameter alpha. 
   * alpha should be in the range [0, 1], where 0 permits no change to the existing surface
   */
  def zeroStartBoundingBox(t : Double, alpha : Double) : (Double, Double) = {
    val (t0, t1) = boundingTimes(t)
    val List(z0, z1, z) = List(t0, t1, t).map(spotRate(_))
    val zMax = z0 max z1
    val zMin = z0 min z1
    if (z > zMax)
      return (zMax, zMax)
    else if (z < zMin)
      return (zMin, zMin)
    else {
      val d = (zMax - z) min (z - zMin)
      val upperBound = z + alpha * d
      val lowerBound = z - alpha * d
      (lowerBound, upperBound)
    }
  }

  /**
   * From the bounding boxes for the two zero start rates we
   * determine the min/max values that we can use to fit a
   * forward rate for the given relaxation paameter alpha
   */
  def forwardRateBounds(t0 : Double, t1 : Double, alpha : Double) : (Double, Double) = {
    val (z0Low, z0High) = zeroStartBoundingBox(t0, alpha)
    val (z1Low, z1High) = zeroStartBoundingBox(t1, alpha)
    val maxRate = (z1High * t1 - z0Low * t0) / (t1 - t0)
    val minRate = (z1Low * t1 - z0High * t0) / (t1 - t0)
    (minRate, maxRate)
  }

  /**
   * Convenience method - returns the size of the bounding box
   */
  def boundingBoxWidth(t : Double, alpha : Double) = {
    val (zLow, zHigh) = zeroStartBoundingBox(t, alpha)
    zHigh - zLow
  }

  /**
   * Finds the best spots rates that can be added given a forward rate, i.e with the least distortion
   */ 
  def bestFittingSpotRates(t0 : Double, t1 : Double, fwdRate : Double, alpha : Double) : (Double, Double) = {
    // Find the spt rates for (t0, t1)
    val List(z0, z1) = List(t0, t1).map(spotRate(_))

    // Find the width of the bounding boxes for (t0, t1)
    val List(t0Bound, t1Bound) = List(t0, t1).map(boundingBoxWidth(_, alpha))

    if (t0Bound == 0){
      // If the front bounding box has zero width (this will only happen
      // if one of the points of the forward start rate exactly matches one
      // of our zero start rates) then the back rate is constraine dby arbitrage
      (z0, backRateFromFwdAndFront(t0, t1, fwdRate, z0))
    } else if (t1Bound == 0){
      // ditto for the back box
      (frontRateFromFwdAndBack(t0, t1, fwdRate, z1), z1)
    } else {
      // With non-empty bounding boxes for forward and back rates we choose spot rates
      // that are equidistant from the interpolated rates (measured using the relative distance
      // toe thier relative boudning boxes
      val za = (z0 * t1Bound / t0Bound  + z1 - fwdRate * (t1 - t0) / t1) / (t1Bound / t0Bound + t0 / t1)
      (za, backRateFromFwdAndFront(t0, t1, fwdRate, za))
    }
  }

  /** 
   * Given a forward rate, create a new surface that matches this rate- if possible without too much
   * distortion. If the boudning boxes of each spot rate are wide enough then the forward rate will
   * be matched exactly - otherwise the maximal or minal rate implied by the boxes will be used
   */
  def addForwardRate(t0 : Double, t1 : Double, zFwd : Double, alpha : Double) : BasisSurface = {
    if (t0 < zeroStartTimes.head)
      return this
    if (t1 > zeroStartTimes.last)
      return this
    val(zFwdLow, zFwdHigh) = forwardRateBounds(t0, t1, alpha)
    val zFwdActual = (zFwd min zFwdHigh) max zFwdLow
    val (z0, z1) = bestFittingSpotRates(t0, t1, zFwdActual, alpha)
    var curvePoints = zeroStartTimes.zip(zeroStartSpreads).toMap
    curvePoints += t0 -> z0
    curvePoints += t1 -> z1

    new BasisSurface(curvePoints)
  }

}
