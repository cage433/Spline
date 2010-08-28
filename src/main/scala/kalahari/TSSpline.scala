package kalahari

class TSSpline(xs : Array[Double], ys : Array[Double]){
  private val spline = new CubicSpline(xs, ys, Some(0.0), Some(0.0))

  def apply(x : Double) = interpolate(x)._1 

  def interpolate(x : Double) : (Double, Boolean) = {
    if (x < xs.head)
      (ys.head, false)
    else if (x > xs.last)
      (ys.last, false)
    else
      (spline(x), true)
  }

}
