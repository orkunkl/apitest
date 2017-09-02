import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import ApiTest._
import io.circe.parser._
import io.circe.syntax._

class MatchTest extends FlatSpec with EitherValues {

  val json1 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json1AsJson = Map("ford" -> "fast", "lambo" -> "crazy").asJson
  val json5AsJson = Map("ford" -> "fasd", "lambo" -> "crazy").asJson

  val json2 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json4 = Map("fort" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json5 = Map("ford" -> "fasd", "lambo" -> "crazy").asJson.asObject.get
  val json3 = Map("ford" -> "fast").asJson.asObject.get
   // .asJson.asArray.get.map(_.asObject.get)

  "Matcher" should "match resource api json fields and api json fields" in {
    ApiTest.fieldSizeTest(json1, json2, List.empty[String]) should be ('right)
  }

  it should "not match resource api json and api" in {
    ApiTest.fieldSizeTest(json1, json3, List.empty[String]).right.value should contain ("lambo")
  }

  it should "be equal" in {
    json1 equals json2 should be (true)
  }
  it should "not be equal" in {
    json1 equals json4 should be (false)
  }
  it should "not be equaasdasdl" in {
    json1 equals json5 should be (false)
  }

  it should "Jsons should not be equal" in {
    json1AsJson equals json5AsJson should be (false)
  }
}
