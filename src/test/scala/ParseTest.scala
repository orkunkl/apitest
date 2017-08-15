import java.io.File
import org.scalatest.Matchers._
import org.scalatest._
import apitest.ApiTest
import io.circe.parser._
import io.circe.syntax._

class ParseTest extends FlatSpec {

  "Parser" should "parse from string" in {
    ApiTest.parse("""{"key": "value"}""") should be (Some(Map("key" -> "value").asJson))
  }

  it should "parse from file" in {
    ApiTest.parse(new File("./src/test/testFile")) should be (Some(Map("key" -> "value").asJson))
  }
}
//matcher
//circe asJson kullan

