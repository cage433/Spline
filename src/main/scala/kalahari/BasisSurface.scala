package kalahari

class ConstrainedBasisSurface(val times : Array[Double], val spotSpreads : Array[Double])
{
	require(times.size == spotSpreads.size, "Axes need to be the same size")
	require(times.toList == times.toList.sortWith(_<_), "time axis must be sorted")

	def this(map : Map[Double, Double]) = this(
		map.keys.toArray.sortWith(_<_),
		map.keys.toArray.sortWith(_<_).map(map)
	)

	val spline = new ConstrainedCubicSpline(times, spotSpreads)

	def spotRate(t : Double) = spline(t)
	def forwardRate(t0 : Double, t1 : Double) : Double = {
		val bs0 = spotRate(t0)
		val bs1 = spotRate(t1)
		(bs1 * t1 - bs0 * t0) / (t1 - t0)
	}

}
