package ApiTest
import ApiTest.FoundError
import io.circe.Json

import scala.util.Try

object ParseOperations {

  def BigDecimalParse(res: Json, api: Json, trace: List[String]): Option[List[FoundError]] = {
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

  def StringParse(res: Json, api: Json, trace: List[String]): Option[List[FoundError]] = {
    for {
      resString <- res.asString
      apiString <- api.asString
    } yield {
      if (resString == apiString) List.empty
      else List(FoundError(trace, s"Value $resString is not equal to $apiString"))
    }
  }
}
