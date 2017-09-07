import DataTypes.DataTypes.Value
import org.scalatest.{EitherValues, FlatSpec}
import io.circe.syntax._
import com.github.nscala_time.time.Imports._

class DataTypeTest extends FlatSpec with EitherValues {
  val datetimestr = "2016-05-23T00:00:00"
  val datetime = DateTime.parse(datetimestr)
  val json = Map("datetime" -> datetimestr).asJson

  "Date time test" should "match ValueDateTime to DateTime" in {
      json.as[Value] equals datetime
  }
}
