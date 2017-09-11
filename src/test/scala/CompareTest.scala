import ApiTest.Main.FoundError
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
    BigDecimalComparison("345".asJson, "3.45".asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching both as Number" in {
    BigDecimalComparison(345.asJson, 3.45.asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching, first as number second as String" in {
    BigDecimalComparison(345.asJson, "3.45".asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 3.45 using BiggerDecimal Matching, first as String second as number" in {
    BigDecimalComparison("345".asJson, 3.45.asJson, List("root")).value should be(List.empty)
  }

  it should "not match 455 and 3.45 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("455".asJson, "3.45".asJson, List("root")).value should not be (List.empty)
  }

  it should "not match 4.5 and 3.45 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("4.5".asJson, "3.45".asJson, List("root")).value should not be (List.empty)
  }

  it should "not match 345.1 and -1 using BiggerDecimal Matching both as String" in {
    BigDecimalComparison("345.1".asJson, "-1".asJson, List("root")).value should not be (List.empty)
  }

  it should "not match asd and 3.45 using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, 3.45.asJson, List("root")) should be(None)
  }

  it should "not match asd and asd using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, "asd".asJson, List("root")) should be(None)
  }

  "BigDecimalComparison(double formats are same)" should "match 345 and 345 using BiggerDecimal Matching both as number" in {
    BigDecimalComparison(345.asJson, 345.asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as both string" in {
    BigDecimalComparison("345".asJson, "345".asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, first as string second as number" in {
    BigDecimalComparison("345".asJson, 345.asJson, List("root")).value should be(List.empty)
  }

  it should "match 345 and 345 using BiggerDecimal Matching as, first as number second as string" in {
    BigDecimalComparison(345.asJson, "345".asJson, List("root")).value should be(List.empty)
  }

  "BigDecimalComparison" should "not match asd and 3.45 using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, 3.45.asJson, List("root")) should be(None)
  }

  it should "not match asd and asd using BiggerDecimal Matching" in {
    BigDecimalComparison("asd".asJson, "asd".asJson, List("root")) should be(None)
  }

  "DateTimeComparison" should "match 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    DateTimeComparison(jsonDateTime1, jsonDateTime2, List("root")).value should be(List.empty)
  }

  it should "not match 2016-05-23T00:00:00 and 2016-05-23T00:00:00Z" in {
    DateTimeComparison(jsonDateTime1, jsonDateTime3, List("root")).value should not be (List.empty)
  }

  it should "not match xp and 2016-05-23T00:00:00Z" in {
    DateTimeComparison("xp".asJson, jsonDateTime3, List("root")) should be(None)
  }

  "StringComparison" should "match asd and asd " in {
    StringComparison("asd".asJson, "asd".asJson, List("root")).value should be(List.empty)
  }

  it should "match 0 and 0 " in {
    StringComparison("0".asJson, "0".asJson, List("root")).value should be(List.empty)
  }

  it should "match 0 and 0, first as number" in {
    StringComparison(0.asJson, "0".asJson, List("root")).value should be(List.empty)
  }

  it should "match 0 and 0, second as number" in {
    StringComparison("0".asJson, 0.asJson, List("root")).value should be(List.empty)
  }

  it should "match 0 and 0, both as number" in {
    StringComparison(0.asJson, 0.asJson, List("root")).value should be(List.empty)
  }

  it should "match true and true" in {
    StringComparison("true".asJson, "true".asJson, List("root")).value should be(List.empty)
  }

  it should "not match true and false" in {
    StringComparison("true".asJson, "false".asJson, List("root")).value should be(
      List(FoundError(List("root"), "Value true is not equal to false")))
  }

  it should "not match xd and zaaa" in {
    StringComparison("xd".asJson, "zaaa".asJson, List("root")).value should be(
      List(FoundError(List("root"), "Value xd is not equal to zaaa")))
  }

  it should "not 15 and zaaa, first as number" in {
    StringComparison(15.asJson, "zaaa".asJson, List("root")).value should be(
      List(FoundError(List("root"), "Value 15 is not equal to zaaa"))
    )
  }
}
