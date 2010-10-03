package kalahari

import scala.collection.immutable.NumericRange
import org.scalatest.FunSuite

class MimimiseFunctionTests extends FunSuite with TestUtils{
	test ("sin"){
		val observedMin = MinimiseFunction(math.sin, -3.14, 0.0)
		val expectedMin = -3.1415 / 2.0
		assert((observedMin - expectedMin).abs < 1e-4)
	}
}
