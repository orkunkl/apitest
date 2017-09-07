package DataTypes
import cats.Eq
import io.circe._
import com.github.nscala_time.time.Imports._
import io.circe.Json.{JArray, JBoolean, JNumber, JObject, JString}
import org.joda.time.{DateTime => JodaDateTime}
import shapeless.ops.coproduct.Folder
import shapeless.{Coproduct, Generic}


import scala.util.Try

object DataTypes {

  trait Value extends Any {
    def equals(obj: Value): Boolean
  }

  implicit class ValueDateTime(val self: DateTime) extends Value {
    def equals(obj: Value): Boolean = obj equals self
  }


  implicit class ValueDateTimeWithZone(val self: DateTime) extends Value {
    def equals(obj: Value): Boolean = obj equals self
  }

  implicit val decodeValue: Decoder[Value] = new Decoder[Value] {
    final def apply(c: HCursor): Decoder.Result[Value] = {
      for {
        focus <- c.focus.toRight(DecodingFailure("could not get focus", c.history))
        value <- focus.asString.toRight(DecodingFailure("could not get focus", c.history)).flatMap(x =>
          Try(DateTime.parse(x)).toEither.left.map(err => DecodingFailure.fromThrowable(err, c.history))
            .map(_ =>DateTime.parse(x.splitAt(x.length-1)._1).asInstanceOf[Value]).left.map(err => DecodingFailure.fromThrowable(err, c.history))
        )
      } yield value

    }
  }
}