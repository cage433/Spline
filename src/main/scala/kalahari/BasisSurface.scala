package kalahari

import RateCalculator._

class ConstrainedBasisSurface(val times : Array[Double], val spotSpreads : Array[Double])
{
	require(times.size == spotSpreads.size, "Axes need to be the same size")
	require(times.toList == times.toList.sortWith(_<_), "time axis must be sorted")

	def this(map : Map[Double, Double]) = this(
		map.keys.toArray.sortWith(_<_),
		map.keys.toArray.sortWith(_<_).map(map)
	)

	val spline = new ConstrainedCubicSpline(times, spotSpreads)

	def spotRate(t : Double) : Double = spline(t)
	def forwardRate(t0 : Double, t1 : Double) : Double = {
		val bs0 = spotRate(t0)
		val bs1 = spotRate(t1)
		(bs1 * t1 - bs0 * t0) / (t1 - t0)
	}

  def boundingTimes(t : Double) = {
    val i = times.findIndexOf(_ > t)
    if (i < 1)
      throw new Exception("Time " + t + " is not bounded by two others in range " + times)
    (times(i - 1), times(i))
  }

  def rateBound(t : Double, alpha : Double) : (Double, Double) = {
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

  def forwardRateBounds(t0 : Double, t1 : Double, alpha : Double) : (Double, Double) = {
    val (z0Low, z0High) = rateBound(t0, alpha)
    val (z1Low, z1High) = rateBound(t1, alpha)
    val maxRate = (z1High * t1 - z0Low * t0) / (t1 - t0)
    val minRate = (z1Low * t1 - z0High * t0) / (t1 - t0)
    (minRate, maxRate)
  }

  def rateBoundWidth(t : Double, alpha : Double) = {
    val (zLow, zHigh) = rateBound(t, alpha)
    zHigh - zLow
  }

  def bestFittingSpotRates(t0 : Double, t1 : Double, fwdRate : Double, alpha : Double) = {
    val List(z0, z1) = List(t0, t1).map(spotRate(_))
    val List(t0Bound, t1Bound) = List(t0, t1).map(rateBoundWidth(_, alpha))
    if (t0Bound == 0){
      (z0, backRateFromFwdAndFront(t0, t1, fwdRate, z0))
    } else if (t1Bound == 0){
      (frontRateFromFwdAndBack(t0, t1, fwdRate, z1), z1)
    } else {
      val za = (z0 * t1Bound / t0Bound  + z1 - fwdRate * (t1 - t0) / t1) / (t1Bound / t0Bound + t0 / t1)
      (za, backRateFromFwdAndFront(t0, t1, fwdRate, za))
    }
  }

  def addForwardRate(t0 : Double, t1 : Double, zFwd : Double, alpha : Double) : ConstrainedBasisSurface = {
    if (t0 < times.head)
      return this
    if (t1 > times.last)
      return this
    val(zFwdLow, zFwdHigh) = forwardRateBounds(t0, t1, alpha)
    val zFwdActual = (zFwd min zFwdHigh) max zFwdLow
    val (z0, z1) = bestFittingSpotRates(t0, t1, zFwdActual, alpha)
    var curvePoints = times.zip(spotSpreads).toMap
    curvePoints += t0 -> z0
    curvePoints += t1 -> z1

    new ConstrainedBasisSurface(curvePoints)
  }

}

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
