package kalahari

class Kalahari{
}
object Kalahari{
	def myabs(x : Double) : Double = x.abs
	def myarray(x : Double) : Array[Double] = {
		Array(x + 1, x + 2, x + 3)
	}
	def mymatrix(x : Double) : Array[Array[Double]] = {
		(1 to 3).toArray.map{
			i =>
				(for (j <- 0 to 5)
					yield x * i + j).toArray
	}}
	def mysum(arr : Array[Double]) : Double = {
		println("arr = " + arr.mkString(", "))
		(0.0 /: arr)(_+_)
	}
	def mysum2(arr : Array[Any]) : Double = {
		println("any arr = " + arr.mkString(", "))
		(0.0 /: arr.filter(_.isInstanceOf[Double]).map(_.asInstanceOf[Double]))(_+_)
	}
}
