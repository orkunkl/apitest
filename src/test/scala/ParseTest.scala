import java.io.File

import org.scalatest.Matchers._
import org.scalatest._
import ApiTest._
import io.circe.JsonObject
import io.circe.parser._
import io.circe.syntax._

class ParseTest extends FlatSpec {

  val jsonArray               = List(Map("ford" -> "fast"), Map("ford" -> "fast")).asJson
  val jsonObjectWithQuote     = Map("xd" -> """3""").asJson
  val jsonObjectWithoutQuote  = Map("xd" -> 3).asJson
  val jsonObjectWithoutQuote2 = Map("xd" -> 3.toString).asJson

  "Parser" should "parse JsonArray to Vector[JsonObject]" in {}

  it should "not match jsonObjectWithQuote and jsonObjectWithoutQuote" in {
    jsonObjectWithQuote should be equals jsonObjectWithQuote
  }
  it should "match jsonObjectWithQuote and jsonObjectWithoutQuote2" in {
    jsonObjectWithQuote should be equals jsonObjectWithoutQuote2
  }
}
//matcher
//circe asJson kullan
