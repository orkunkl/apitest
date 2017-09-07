import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import ApiTest._
import io.circe.parser._
import io.circe.syntax._

class MatchTest extends FlatSpec with EitherValues with OptionValues{

  val json1 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json1AsJson = Map("ford" -> "fast", "lambo" -> "crazy").asJson
  val json5AsJson = Map("ford" -> "fasd", "lambo" -> "crazy").asJson
  val json3asJson = Map("ford" -> "fast").asJson
  val json6asJson = Map("xp" -> "fast").asJson

  val json2 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json4 = Map("fort" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json5 = Map("ford" -> "fasd", "lambo" -> "crazy").asJson.asObject.get
  val json3 = Map("ford" -> "fast").asJson.asObject.get

  val resJson = parse("    {\n        \"date_spain_format\": \"2016-05-23T00:00:00\", \n        \"date_us_format\": \"2016-05-23T00:00:00\", \n        \"euro_money\": \"1125.00\", \n        \"non_special_characters\": \"Words\", \n        \"number_spain_format\": \"3456.78\", \n        \"number_us_format\": \"345678\", \n        \"percent_spain_format\": \"34.5\", \n        \"percent_us_format\": \"345\", \n        \"special_characters\": \"Alt Àneu\", \n        \"us_money\": \"112500.00\"\n    }").right.get
  val apiJson = parse("        {\n            \"__id\": \"row-3ced_pppd.e7py\", \n            \"date_spain_format\": \"2016-05-23T00:00:00Z\", \n            \"date_us_format\": \"2016-05-23T00:00:00Z\", \n            \"euro_money\": 1125.0, \n            \"non_special_characters\": \"Words\", \n            \"number_spain_format\": 3456.78, \n            \"number_us_format\": 345678, \n            \"percent_spain_format\": 0.345, \n            \"percent_us_format\": 3.45, \n            \"special_characters\": \"Alt Àneu\", \n            \"us_money\": 112500.0\n        }").right.get
  // .asJson.asArray.get.map(_.asObject.get)

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

  it should "match jsons fields" in {
    ApiTest.fieldMatchTest(json1AsJson, json5AsJson).right.value should equal(List.empty)
  }
  it should "find extra single field" in {
    ApiTest.fieldMatchTest(json1AsJson, json3asJson).right.value should equal(List("lambo"))
  }
  it should "find extra two fields" in {
    ApiTest.fieldMatchTest(json1AsJson, json6asJson).right.value should equal(List("ford","lambo"))
  }
  it should "not find an extra field between resource and api" in {
    ApiTest.fieldMatchTest(resJson, apiJson).right.value should equal(List.empty)
  }
  it should "find an extra field named __id between resource and api" in {
    ApiTest.fieldMatchTest(apiJson, resJson).right.value should equal(List("__id"))
  }

  it should "return empty list on identical JString comparison" in {
    ApiTest.fieldMatchTest("string1".asJson, "string1".asJson).right.value should be (List.empty)
  }

  it should "return an error prompt on different JString comparison" in {
    ApiTest.fieldMatchTest("string1".asJson, "string3".asJson).right.value should be (List("Values of " + "res" + " does not match with " + "api" + "\n resource = " + "string1" + " api = " + "string3"))
  }

  it should "not find any match errors on comparison of same integer but one is string" in {
    ApiTest.fieldMatchTest("123".asJson, """123""".asJson).right.value should equal(List.empty)
  }

  it should "not find any match errors on comparison of same integer and both are string" in {
    ApiTest.fieldMatchTest("""123""".asJson, """123""".asJson).right.value should equal(List.empty)
  }

  it should "successfully match 345 and 3.45 as doubles" in {
    ApiTest.fieldMatchTest("""345""".asJson, """3.45""".asJson).right.value should be (List.empty)
  }

  it should "not match 345 and 34.5 as doubles" in {
    ApiTest.fieldMatchTest("""345""".asJson, """34.5""".asJson).right.value should not be List.empty
  }

  it should "match 3.45 and 3.45 as doubles" in {
    ApiTest.fieldMatchTest("""345""".asJson, """34.5""".asJson).right.value should be (List.empty)
  }

  it should "successfully match DateTime 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    ApiTest.fieldMatchTest("""2016-05-23T00:00:00""".asJson, """2016-05-23T00:00:00Z""".asJson).right.value should equal(List.empty)
  }

  it should "not match DateTime 2016-05-23T00:00:00 and 2016-05-23T00:00:05" in {
    ApiTest.fieldMatchTest("""2016-05-23T00:00:00""".asJson, """2016-05-23T00:00:05""".asJson).right.value should not be(List.empty)
  }

  it should "not match DateTime 2016-05-23T00:00:00 and 2016-05-23T000000" in {
    ApiTest.fieldMatchTest("""2016-05-23T00:00:00""".asJson, """2016-05-23T000000""".asJson).right.value should not be (List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching" in {
    ApiTest.BigDecimalParse("345".asJson, "3.45".asJson, List.empty[String]).value should be (List.empty)
  }
  it should "match 345 and 345 using BiggerDecimal Matching" in {
    ApiTest.BigDecimalParse("345".asJson, "345".asJson, List.empty[String]).value should be (List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, both string" in {
    ApiTest.BigDecimalParse("""345""".asJson, """345""".asJson, List.empty[String]).value should be (List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, one string" in {
    ApiTest.BigDecimalParse("345".asJson, """345""".asJson, List.empty[String]).value should be (List.empty)
  }
  "string1".asJson == "string1".asJson
  123.asJson == 123.asJson
  "123".asJson == 123.asJson

}