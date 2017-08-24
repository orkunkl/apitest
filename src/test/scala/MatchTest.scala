import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import apitest.ApiTest
import io.circe.parser._
import io.circe.syntax._

class MatchTest extends FlatSpec with EitherValues {

  val json1 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json2 = Map("ford" -> "fast", "lambo" -> "crazy").asJson.asObject.get
  val json3 = Map("ford" -> "fast").asJson.asObject.get
   // .asJson.asArray.get.map(_.asObject.get)

  "Matcher" should "match resource api json fields and api json fields" in {
    ApiTest.fieldSizeTest(json1, json2, List.empty[String]) should be ('right)
  }

  it should "not match resource api json and api" in {
    ApiTest.fieldSizeTest(json1, json3, List.empty[String]).right.value should contain ("lambo")
  }
}
