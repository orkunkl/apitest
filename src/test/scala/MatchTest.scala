import org.scalatest.Matchers._
import org.scalatest._
import ApiTest.ApiTest._
import io.circe.{Json, JsonObject, ParsingFailure}
import io.circe.parser._
import io.circe.syntax._

import scalaz.stream.nio.file

class MatchTest extends FlatSpec with EitherValues with OptionValues {

  val json1: JsonObject =
    Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json1AsJson: Json = Map("ford" -> "fast", "lambo" -> "crazy").asJson
  val json5AsJson: Json = Map("ford" -> "fasd", "lambo" -> "crazy").asJson
  val json3asJson: Json = Map("ford" -> "fast").asJson
  val json6asJson: Json = Map("xp"   -> "fast").asJson

  val json2: JsonObject =
    Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json4: JsonObject =
    Map("fort" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json5: JsonObject =
    Map("ford" -> "fasd", "lambo" -> "crazy").asJson.asObject.get
  val json3: JsonObject = Map("ford" -> "fast").asJson.asObject.get

  val resJson: Json = parse(scala.io.Source.fromFile("src/test/resApi.json").mkString).right.get
  val apiJson: Json =
    parse(scala.io.Source.fromFile("src/test/api.json").mkString).right.get.hcursor
      .get[Json]("value")
      .right
      .get

  val resObject
    : JsonObject = parse(scala.io.Source.fromFile("src/test/resObject.json").mkString).right.get.asObject.get
  val apiObject
    : JsonObject = parse(scala.io.Source.fromFile("src/test/apiObject.json").mkString).right.get.asObject.get

  "Matcher" should "match resource api json fields and api json fields" in {
    //ApiTest.fieldSizeTest(json1, json2, List.empty[String]) should be ('right)
  }
  it should "not match resource api json and api" in {
    //ApiTest.fieldSizeTest(json1, json3, List.empty[String]).right.value should contain ("lambo")
  }

  it should "be equal" in {
    json1 equals json2 should be(true)
  }

  it should "not be equal" in {
    json1 equals json4 should be(false)
  }
  it should "Jsons should not be equal" in {
    json1AsJson equals json5AsJson should be(false)
  }

  it should "find single field error" in {
    fieldMatchTest(json1AsJson, json3asJson).right.value should be(
      List(FoundError(List("root"), "lambo not found on api")))
  }

  it should "find single value error" in {
    fieldMatchTest(json1AsJson, json5AsJson).right.value should be(
      List(FoundError(List("root", "ford"), "Value fast is not equal to fasd")))
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching" in {
    BigDecimalParse("345".asJson, "3.45".asJson, List.empty[String]).value should be(List.empty)
  }
  it should "match 345 and 345 using BiggerDecimal Matching" in {
    BigDecimalParse("345".asJson, "345".asJson, List.empty[String]).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, both string" in {
    BigDecimalParse("""345""".asJson, """345""".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, one string" in {
    BigDecimalParse("345".asJson, """345""".asJson, List.empty[String]).value should be(List.empty)
  }

  it should "find errors between mock jsons" in {
    fieldMatchTest(resJson, apiJson).right.value should be(List.empty)
  }
  "ObjectOperations" should "find errors between mock jsons" in {

    objectOperations(resObject, apiObject, List("root")).right.value should not be List.empty
  }
}
