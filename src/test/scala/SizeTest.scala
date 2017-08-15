import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import apitest.ApiTest
import io.circe.parser._
import io.circe.syntax._

class SizeTest extends FlatSpec {

  val array1 = List(1, 2, 3).asJson.asArray.get
  val array2 = List("a", "b", "c").asJson.asArray.get
  val array3 = List("a", "b", "c", "s").asJson.asArray.get

  "Size" should "be equal" in {
    ApiTest.sizeTest(array1, array2) should be ('right)
  }

  it should "be not equal" in {
    ApiTest.sizeTest(array1, array3) should be ('left)
  }
}
//matcher
//circe asJson kullan

