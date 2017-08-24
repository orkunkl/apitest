import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import apitest.ApiTest
import io.circe.parser._
import io.circe.syntax._

class SizeTest extends FlatSpec {

  val array1 = Map("a" -> "b").asJson.asObject.get
  val array2 = Map("a" -> "b").asJson.asObject.get
  val array3 = Map("a" -> "b", "c" -> "d").asJson.asObject.get

  "Size" should "be equal" in {
    ApiTest.sizeTest(array1, array2) should be ('right)
  }

  it should "not be equal" in {
    ApiTest.sizeTest(array1, array3) should be ('left)
  }
}
//matcher
//circe asJson kullan

