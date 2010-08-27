package kalahari

class Curve(xs : Array[Double], ys : Array[Double]){
  def +(x : Double, y : Double) : Curve = {
    assert(!xs.contains(x), "Already contains x")
    null
  }


}
