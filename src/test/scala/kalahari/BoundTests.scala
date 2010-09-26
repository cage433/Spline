package kalahari

import org.scalatest.FunSuite

class BoundTests extends FunSuite with TestUtils{
	test("Intersections"){
		assert(
			LowerBound(1.0).intersection(UpperBound(2.0)) === TwoSidedBound(1.0, 2.0)
		)
		assert(
			LowerBound(1.0).intersection(UpperBound(-2.0)) === EmptyBound
		)
		assert(
			TwoSidedBound(1.0, 2.0).intersection(UpperBound(-2.0)) === EmptyBound
		)
		assert(
			TwoSidedBound(1.0, 2.0).intersection(LowerBound(-2.0)) === TwoSidedBound(1.0, 2.0)
		)
		assert(
			TwoSidedBound(1.0, 2.0).intersection(UpperBound(5.0)) === TwoSidedBound(1.0, 2.0)
		)
		assert(
			TwoSidedBound(1.0, 2.0).intersection(TwoSidedBound(1.5, 2.5)) === TwoSidedBound(1.5, 2.0)
		)
	}
}

