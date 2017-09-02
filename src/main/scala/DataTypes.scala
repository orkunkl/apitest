package DataTypes
import io.circe.{Decoder, DecodingFailure, HCursor}
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.RichDateTime
import org.joda.time.{DateTime => JodaDateTime}

object DataTypes {

  trait Value extends  Any{
    def equals(obj: Value): Boolean
  }

  implicit class DateTime(val self: RichDateTime) extends Value {
     def equals(obj: Value): Boolean =
       obj.isInstanceOf[DateTime] && obj.asInstanceOf[DateTime].self.equals(this)
  }


  implicit class DateTimeWithZone(val self: RichDateTime) extends Value {
    def equals(obj: Value): Boolean =
      obj.isInstanceOf[DateTime] && obj.asInstanceOf[DateTime].self.equals(this)
  }

  implicit class ValueString(val self: String) extends Value {
    def equals(obj: Value): Boolean =
      obj.isInstanceOf[String] && obj.asInstanceOf[String] == this
  }


  implicit val decodeValue: Decoder[Value] = new Decoder[Value] {
    final def apply(c: HCursor): Decoder.Result[Value] = {
      for {
        focus <- c.focus.toRight("couldn't get the focus")
        json <- focus.asBoolean.orElse(focus.asNumber).orElse(focus.asString).map(_.toString).toRight("value couldnt be parsed")
      } yield {
        json
      }
    }
  }
}
