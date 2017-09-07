package ApiTest

import io.circe.{Json, _}
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client.blaze.{PooledHttp1Client, _}
import com.typesafe.scalalogging._
import org.http4s.client.Client
import io.circe.parser.parse

import scala.util.Random
import scala.util.Try

object ApiTest {

  val logger = Logger("test")

  def main(args: Array[String]): Unit = {

    val httpClient = PooledHttp1Client()

    val tests = for {
      urls <- createUrls(httpClient)
      (resUrl, apiUrl) = urls

      _ = println("Resource Api : " + resUrl)
      _ = println("Api : " + apiUrl)

      /*callTry <- Try(httpClient.expect[Json](resUrl).unsafePerformSync,
                     httpClient.expect[Json](apiUrl).unsafePerformSync).toEither*/
      //callTry <-  (parse(scala.io.Source.fromFile("src/test/resApi.json").mkString),  parse(scala.io.Source.fromFile("src/test/api.json").mkString))

      resApiJson <- parse(scala.io.Source.fromFile("src/test/resApi.json").mkString)
      apiJson    <- parse(scala.io.Source.fromFile("src/test/api.json").mkString)

      _ = logger.info("resApiJson " + resApiJson)
      _ = logger.info("apiJson " + apiJson)

      apiJsonValue <- apiJson
        .as[JsonObject]
        .flatMap(_.apply("value").toRight("value not found on api"))
      testResult <- test(resApiJson, apiJsonValue)
    } yield testResult

    tests.map(errors => errors.map(println)).left.map(_ => println("No errors"))

    httpClient.shutdownNow()
  }

  def test(resApi: Json, api: Json) =
    for {
      testResult <- fieldMatchTest(resApi, api)
    } yield testResult

  def createUrls(httpClient: Client): Either[Object, (Uri, Uri)] =
    for {
      apiSetsUrl <- Uri
        .fromString("https://cheetah.rc-socrata.com/api/odata/v4/")
        .toEither
      apiUrlCall <- Try(httpClient.expect[Json](apiSetsUrl).unsafePerformSync).toEither
      _ = logger.info("sending request to " + apiSetsUrl)
      apiRetrieveValues <- apiUrlCall.hcursor.get[Json]("value")
      apiRandomUrl <- apiRetrieveValues.asArray
        .toRight("could not parse value to jsonArray")
        .flatMap(
          x =>
            apiRetrieveValues.hcursor
              .downN(Random.nextInt(x.size))
              .get[String]("url"))

      _ = logger.info("apiRandomUrl " + apiRandomUrl)
      _ = println("Chosen random api url " + apiRandomUrl)

      apiUrl = apiSetsUrl / apiRandomUrl
      _      = logger.info("apiUrl " + apiUrl)

      resUrl <- Uri
        .fromString("https://cheetah.rc-socrata.com/resource/" + apiRandomUrl)
        .toEither
      _ = println("Chosen random resource api url " + resUrl)
      _ = println("Chosen random api url " + apiUrl)

    } yield (resUrl, apiUrl)

  // Unacceptable test. Stops the whole test if size test fails
  def sizeTest(res: Json, api: Json): Either[String, Unit] = {
    (res, api) match {
      case _ if (res.isArray && api.isArray) =>
        val resApiSize = res.asArray.get.size
        val apiSize    = api.asArray.get.size
        if (resApiSize == apiSize) Right()
        else
          Left("Sizes are not equal, resource = " + resApiSize + ", api = " + apiSize)
      case _ if res.isObject && api.isObject => Right()
      case _ =>
        Left(
          "Resource api and api json types do not match! resource = " +
            res.arrayOrObject("", _ => "array", _ => "object") + ", api = " + api
            .arrayOrObject("", _ => "array", _ => "object"))
    }
  }
  // Acceptable test. Does not stop the whole test if size test fails except the case of types of focused jsons are different
  def fieldMatchTest(res: Json,
                     api: Json,
                     trace: List[String] = List("root")): Either[String, List[FoundError]] = {
    res.arrayOrObject(
      ValueOperations(res, api, trace),
      resArray =>
        api.asArray
          .toRight(res.name + " is an array but " + api.name + " is not")
          .flatMap(apiArray => arrayOperations(resArray, apiArray, trace)),
      resObject =>
        api.asObject
          .toRight(res.name + " is an object but " + api.name + " is not")
          .flatMap(apiObject => objectOperations(resObject, apiObject, trace))
    )
  }
  def arrayOperations(res: Vector[Json],
                      api: Vector[Json],
                      trace: List[String]): Either[String, List[FoundError]] =
    res
      .zip(api)
      .zipWithIndex
      .foldLeft(Right(List.empty[FoundError]): Either[String, List[FoundError]]) {
        case (acc, ((x, y), a)) =>
          val newErrors = fieldMatchTest(x, y, trace.:+(a.toString))
          appendError(acc, newErrors)
      }

  def objectOperations(res: JsonObject,
                       api: JsonObject,
                       trace: List[String]): Either[String, List[FoundError]] = {

    val r =
      for (f <- api.fields.toSet union res.fields.toSet)
        yield
          (res(f), api(f)) match {
            case (Some(r), Some(a)) => fieldMatchTest(r, a, trace.:+(r.name))
            case (None, Some(a))    => Right(List(FoundError.empty))
            case (Some(r), None)    => Right(List(FoundError(trace, s"${r.name} not found on api")))
            case _                  => Right(List(FoundError.empty)) // mumkun degil
          }
    //TODO change FoundError to something that has unit
    r.reduce((x, y) => x.flatMap(err1 => y.map(err2 => err1 ++ err2)))
  }

  def ValueOperations(res: Json,
                      api: Json,
                      trace: List[String] = List("root")): Either[String, List[FoundError]] = {
    val parseProcess = ParseOperations.BigDecimalParse(res, api, trace) orElse ParseOperations
      .StringParse(res, api, trace)
    parseProcess.toRight("Values are not equal")
  }

  def appendError(a1: Either[String, List[FoundError]],
                  a2: Either[String, List[FoundError]]): Either[String, List[FoundError]] =
    a1.flatMap(x => a2.map(x1 => x ++ x1))

  case class FoundError(trace: List[String], error: String) {
    override def toString: String = s"Trace = ${trace.reduce(_ + " -> " + _)}, Error = $error"
    def empty                     = FoundError(List.empty, "")
  }
  object FoundError {
    def empty = FoundError(List.empty, "")
  }
}
