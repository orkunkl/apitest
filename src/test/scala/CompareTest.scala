import ApiTest.ParseOperations._
import io.circe.Json
import org.scalatest.Matchers.be
import org.scalatest.{EitherValues, FlatSpec, OptionValues}
import io.circe.syntax._
import org.scalatest.Matchers._

class CompareTest extends FlatSpec with EitherValues with OptionValues {

  val jsonDateTime1: Json = """2016-05-23T00:00:00""".asJson
  val jsonDateTime2: Json = """2016-05-23T00:00:00Z""".asJson
  val jsonDateTime3: Json = """2016-05-25T00:00:00Z""".asJson

  /**
    *   Happy path tests
    * */
  "BigDecimalComparison(double formats are different)" should "match 345 and 3.45 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("345".asJson, "3.45".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching both as Number" in {
    BigDecimalComparison(345.asJson, 3.45.asJson, List.empty[String]).value should be(List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching, first as number second as String" in {
    BigDecimalComparison(345.asJson, "3.45".asJson, List.empty[String]).value should be(List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching, first as String second as number" in {
    BigDecimalComparison("345".asJson, 3.45.asJson, List.empty[String]).value should be(List.empty)
  }

  it should "not match 455 and 3.45 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("455".asJson, "3.45".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "not match 4.5 and 3.45 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("4.5".asJson, "3.45".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "not match 345.1 and -1 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("345.1".asJson, "-1".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "not match asd and 3.45 using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, 3.45.asJson, List.empty[String]).value should be(List.empty)
  }

  it should "not match asd and asd using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, "asd".asJson, List.empty[String]).value should be(
      List.empty)
  }

  "BigDecimalComparison(double formats are same)" should "match 345 and 345 using BiggerDecimal Matching both as number" in {
    BigDecimalComparison(345.asJson, 345.asJson, List.empty[String]).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as both string" in {
    BigDecimalComparison("345".asJson, "345".asJson, List.empty[String]).value should be(
      List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, first as string second as number" in {
    BigDecimalComparison("345".asJson, 345.asJson, List.empty[String]).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, first as number second as string" in {
    BigDecimalComparison(345.asJson, "345".asJson, List.empty[String]).value should be(List.empty)
  }

  "DateTimeComparison" should "match 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    DateTimeComparison(jsonDateTime1, jsonDateTime2, List("root")).value should be(List.empty)
  }

  it should "not match 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    DateTimeComparison(jsonDateTime1, jsonDateTime3, List("root")).value should not be (List.empty)
  }

}
