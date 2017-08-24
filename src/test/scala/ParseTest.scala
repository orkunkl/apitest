import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import apitest.ApiTest
import io.circe.parser._
import io.circe.syntax._

class ParseTest extends FlatSpec {

  val jsonArray = List(Map("ford" -> "fast"), Map("ford" -> "fast")).asJson

  "Parser" should "parse JsonArray to Vector[JsonObject]" in {
    ApiTest.parseToArray(jsonArray, "json array") should be ('right)
  }

}
//matcher
//circe asJson kullan

