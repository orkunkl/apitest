import org.scalatest.Matchers._
import org.scalatest._
import ApiTest.Main._
import io.circe.Json
import io.circe.syntax._

class SizeTest extends FlatSpec with EitherValues {

  val json1: Json = Map("a" -> "b").asJson
  val json2: Json = Map("a" -> "b").asJson
  val json3: Json = Map("a" -> "b", "c" -> "d").asJson

  val jsonArray: Json  = List(Map("a" -> "b", "a" -> "c"), Map("a" -> "b", "a" -> "c")).asJson
  val jsonArray2: Json = List(Map("a" -> "b", "a" -> "c"), Map("a" -> "b", "a" -> "e")).asJson
  val jsonArray3: Json = List(Map("a" -> "b", "a" -> "c"),
                              Map("a" -> "b", "a" -> "d"),
                              Map("a" -> "b", "a" -> "c", "a" -> "x")).asJson

  "sizeTest" should "validate 2 jsons " in {
    sizeTest(json1, json2).right.value should be(List.empty)
  }

  it should "not validate jsons with different sizes" in {
    sizeTest(json1, json3) should be('left)
  }

  it should "not be equal" in {
    sizeTest(json1, json3) should be('left)
  }

  it should "validate arrays" in {
    sizeTest(jsonArray, jsonArray2) should be('right)
  }

  it should "not validate arrays" in {
    sizeTest(jsonArray, jsonArray3) should be('left)
  }
}
//matcher
//circe asJson kullan
