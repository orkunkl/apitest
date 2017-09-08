package ApiTest
import Main.FoundError
import io.circe.Json
import com.github.nscala_time.time.Imports._

import scala.util.Try

object ParseOperations {

  type ErrorOption = Option[List[FoundError]]

  def BigDecimalComparison(res: Json, api: Json, trace: List[String]): ErrorOption = {
    for {
      resDecimal <- res.asString.flatMap(x => Try(BigDecimal(x)).toOption) orElse res.asNumber
        .flatMap(_.toBigDecimal)
      apiDecimal <- api.asString.flatMap(x => Try(BigDecimal(x)).toOption) orElse api.asNumber
        .flatMap(_.toBigDecimal)
    } yield {
      if (resDecimal == apiDecimal) List.empty
      else if (resDecimal / 100 == apiDecimal) List.empty
      else
        List(FoundError(
          trace,
          s"Value of ${res.name} does not match with ${api.name} resource = $resDecimal api = $apiDecimal"))
    }
  }

  def StringComparison(res: Json, api: Json, trace: List[String]): ErrorOption = {
    for {
      resString <- res.asString
      apiString <- api.asString
    } yield {
      if (resString == apiString) List.empty
      else List(FoundError(trace, s"Value $resString is not equal to $apiString"))
    }
  }

  def DateTimeComparison(res: Json, api: Json, trace: List[String]): ErrorOption =
    for {
      resString   <- res.asString
      apiString   <- api.asString
      resDateTime <- Try { DateTime.parse(resString) }.toOption
      apiDateTime <- Try { DateTime.parse(apiString) }.toOption
    } yield {
      if (resDateTime.toLocalDateTime == apiDateTime.toLocalDateTime) List.empty
      else List(FoundError(trace, s"DateTime $resString is not equal to $apiString"))
    }
}
