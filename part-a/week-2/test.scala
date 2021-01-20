import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

    test("addition") {
        val sum = 1 + 1
        assert(sum === 2)
    }

    test("subtraction") {
        val diff = 4 - 1
        assert(diff === 3)
    }
}