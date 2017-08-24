package DataTypes
import com.github.nscala_time.time.RichDateTime
import io.circe.{Decoder, DecodingFailure, HCursor}

object DataTypes {

  trait Value {

    def equals(obj: Value): Boolean
  }

  implicit class DateTime(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = ???
  }


  implicit class DateTimeWithZone(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = self.underlying
  }


  implicit class CheckBox(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = ???
  }


  implicit class Flag(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = ???
  }


  implicit class Star(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = ???
  }


  implicit class Phone(val self: RichDateTime) extends Value {
    override def equals(obj: Value): Boolean = ???
    implicit val decodeFoo: Decoder[Value] = new Decoder[RichDateTime] {
      final def apply(c: HCursor): Decoder.Result[Value] = {

        )
      }
    }
  }

  implicit val decodeFoo: Decoder[Value] = new Decoder[Value] {
    final def apply(c: HCursor): Decoder.Result[Value] = {
      c.focus.map(x =>
        for {
          value <- x.as[String].left
          value <- x.as[Date].left
        }
      )
    }
  }
}
