package kalahari

trait Bound{
	def contains(x : Double) : Boolean
	def intersection(other : Bound) : Bound
	def isEmpty = false
	def closestPoint(x : Double) : Double
	def low_ : Option[Double] = None
	def high_ : Option[Double] = None
	def map(f : Double => Double) = {
		if (f(0) < f(1)){
			Bound(low_.map(f), high_.map(f))
		} else {
			Bound(high_.map(f), low_.map(f))
		}
	}
}

object Bound{
	def apply(low : Option[Double], high : Option[Double]) : Bound = {
		(low, high) match {
			case(Some(l), Some(r)) if l <= r => TwoSidedBound(l, r)
			case(Some(l), Some(r)) => EmptyBound
			case (Some(l), _) => LowerBound(l)
			case (_, Some(r)) => UpperBound(r)
			case _ => EmptyBound
		}
	}
	def apply(low : Double, high : Double) : Bound = Bound(Some(low), Some(high))
}
		
case object EmptyBound extends Bound{
	def contains(x : Double) = false
	def intersection(other : Bound) = EmptyBound
	override def isEmpty = true
	def closestPoint(x : Double) = throw new Exception("EmptyBound")
}

case class LowerBound(low : Double) extends Bound{
	def contains(x : Double) = x >= low
	def intersection(other : Bound) = other match {
		case EmptyBound => EmptyBound
		case LowerBound(low2) => LowerBound(low max low2)
		case UpperBound(high) => Bound(low, high)
		case _ : TwoSidedBound => other.intersection(this)
	}
	def closestPoint(x : Double) = if (contains(x)) x else low
	override def low_ = Some(low)
}

case class UpperBound(high : Double) extends Bound{
	def contains(x : Double) = x <= high
	def intersection(other : Bound) = other match {
		case EmptyBound => EmptyBound
		case LowerBound(low) => Bound(low, high)
		case UpperBound(high2) => UpperBound(high min high2)
		case _ : TwoSidedBound => other.intersection(this)
	}
	def closestPoint(x : Double) = if (contains(x)) x else high
	override def high_ = Some(high)
}


case class TwoSidedBound(low : Double, high : Double) extends Bound{
	require(low <= high, "Improper bound (" + low + ", " + high + ")")
	def contains(x : Double) = x >= low && x <= high
	def intersection(other : Bound) = other match {
		case EmptyBound => EmptyBound
		case LowerBound(low2) => Bound(low2 max low, high)
		case UpperBound(high2) => Bound(low, high min high2)
		case TwoSidedBound(low2, high2) => Bound(low max low2, high min high2)
	}
	def closestPoint(x : Double) = if (contains(x)) x 
		else if (x < low) 
			low
		else
			high
	override def low_ = Some(low)
	override def high_ = Some(high)
}

