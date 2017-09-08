import ApiTest.ParseOperations._
import org.scalatest.Matchers.be
import org.scalatest.{EitherValues, FlatSpec, OptionValues}
import io.circe.syntax._
import org.scalatest.Matchers._

class CompareTest extends FlatSpec with EitherValues with OptionValues {

  val jsonDateTime1 = """2016-05-23T00:00:00""".asJson
  val jsonDateTime2 = """2016-05-23T00:00:00Z""".asJson

  "BigDecimalComparison" should "match 345 and 3.45 using BiggerDecimal Matching" in {
    BigDecimalComparison("345".asJson, "3.45".asJson, List.empty[String]).value should be(
      List.empty)
  }
  it should "match 345 and 345 using BiggerDecimal Matching" in {
    BigDecimalComparison("345".asJson, "345".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, both string" in {
    BigDecimalComparison("""345""".asJson, """345""".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, one string" in {
    BigDecimalComparison("345".asJson, """345""".asJson, List.empty[String]).value should be(
      List.empty)
  }

  "DateTimeComparison" should "match 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    DateTimeComparison(jsonDateTime1, jsonDateTime2, List("root")).value should be(List.empty)
  }
}
