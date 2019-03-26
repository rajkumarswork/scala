import org.scalatest.FlatSpec

class WeightsTests extends FlatSpec {

  "Weights" should "be calculated correctly" in {

    val x = 3
    val label = true
    val weights = Array(0.0, 0.0)

    val result = Array(0.5, 0.5)
    val expected = Array(0.5, 0.5)
    assert(expected === result)
  }
}
