package ApiTest

import io.circe.parser.parse
import io.circe.{Json, _}
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client

import scala.collection.IterableLike
import scala.util.{Random, Try}

object Main {

  type ErrorEither = Either[FoundError, List[FoundError]]

  def main(args: Array[String]): Unit = {

    val httpClient = PooledHttp1Client()

    val tests = for {
      urls <- createUrls(httpClient)
      (resUrl, apiUrl) = urls

      _ = println("Resource Api : " + resUrl)
      _ = println("Api : " + apiUrl)

      callTry <- Try(httpClient.expect[Json](resUrl).unsafePerformSync,
                     httpClient.expect[Json](apiUrl).unsafePerformSync).toEither
      //callTry <-  (parse(scala.io.Source.fromFile("src/test/resApi.json").mkString),  parse(scala.io.Source.fromFile("src/test/api.json").mkString))
      (resApiJson, apiJson) = callTry
      /*resApiJson <- parse(scala.io.Source.fromFile("src/test/resApi.json").mkString)
      apiJson    <- parse(scala.io.Source.fromFile("src/test/api.json").mkString)*/

      apiJsonValue <- apiJson
        .as[JsonObject]
        .flatMap(_.apply("value").toRight("value not found on api"))
      testResult <- test(resApiJson, apiJsonValue)
    } yield testResult

    tests
      .map(errors => if (errors.isEmpty) println("No Errors Found") else errors.foreach(println))
      .left
      .map(_ => println("Unknown error found"))

    httpClient.shutdownNow()
  }

  def test(resApi: Json, api: Json) = {
    val tests = for {
      _          <- sizeTest(resApi, api)
      testResult <- fieldAndValueMatchTest(resApi, api)
    } yield testResult

    if (tests.isLeft) tests.swap.map(x => List(x)).left.map(x => "" /*unreachable code*/ )
    else tests
  }
  def createUrls(httpClient: Client): Either[Object, (Uri, Uri)] =
    for {
      apiSetsUrl <- Uri
        .fromString("https://cheetah.rc-socrata.com/api/odata/v4/")
        .toEither
      apiUrlCall        <- Try(httpClient.expect[Json](apiSetsUrl).unsafePerformSync).toEither
      apiRetrieveValues <- apiUrlCall.hcursor.get[Json]("value")
      apiRandomUrl <- apiRetrieveValues.asArray
        .toRight("could not parse value to jsonArray")
        .flatMap(
          x =>
            apiRetrieveValues.hcursor
              .downN(Random.nextInt(x.size))
              .get[String]("url"))

      _ = println("Chosen random api url " + apiRandomUrl)

      apiUrl = apiSetsUrl / apiRandomUrl

      resUrl <- Uri
        .fromString("https://cheetah.rc-socrata.com/resource/" + apiRandomUrl)
        .toEither
      _ = println("Chosen random resource api url " + resUrl)
      _ = println("Chosen random api url " + apiUrl)

    } yield (resUrl, apiUrl)

  /**
    *
    *   Size test related functions
    *
    * */
  // Unacceptable test. Stops the whole test if size test fails
  def sizeTest(res: Json, api: Json, trace: List[String] = List("root")): ErrorEither = {
    res.arrayOrObject(
      Right(List.empty),
      resArray =>
        api.asArray
          .toRight(FoundError(trace, s"${res.name} is an array but ${api.name} is not"))
          .flatMap(apiArray => compareRecursiveArraySize(resArray, apiArray, trace.:+(res.name))),
      resObject =>
        api.asObject
          .toRight(FoundError(trace, s"${res.name} is an object but ${api.name} is not"))
          .flatMap(apiObject => compareObjectSizes(resObject, apiObject, trace.:+(res.name)))
    )
  }
  def compareRecursiveArraySize(res: Vector[Json],
                                api: Vector[Json],
                                trace: List[String]): ErrorEither = {
    lazy val arrayComparison =
      res
        .zip(api)
        .zipWithIndex
        .foldLeft(Right(List.empty[FoundError]): ErrorEither) {
          case (acc, ((x, y), a)) =>
            val newErrors = sizeTest(x, y, trace.:+(a.toString))
            traverse(acc, newErrors)
        }
    traverse(compareArraySize(res, api, trace), arrayComparison)
  }
  def compareArraySize(res: Vector[Json], api: Vector[Json], trace: List[String]): ErrorEither =
    if (res.size == api.size) Right(List.empty)
    else
      Left(
        FoundError(trace, s"Array sizes are not equal, Resource = ${res.size} Api = ${api.size}"))

  def compareRecursiveObjectSize(res: JsonObject,
                                 api: JsonObject,
                                 trace: List[String]): ErrorEither = {
    lazy val objectComparison =
      res.toList
        .zip(api.toList)
        .zipWithIndex
        .foldLeft(Right(List.empty[FoundError]): ErrorEither) {
          case (acc, ((x, y), a)) =>
            val newErrors = sizeTest(x._2, y._2, trace.:+(a.toString))
            traverse(acc, newErrors)
        }
    traverse(compareObjectSizes(res, api, trace), objectComparison)
  }

  def compareObjectSizes(res: JsonObject, api: JsonObject, trace: List[String]): ErrorEither =
    if (res.size == api.size) Right(List.empty)
    else
      Left(
        FoundError(trace, s"Object sizes are not equal, Resource = ${res.size} Api = ${api.size}"))

  /**
    *
    *   Field And Value match related functions
    *
    * */
  def fieldAndValueMatchTest(res: Json,
                             api: Json,
                             trace: List[String] = List("root")): ErrorEither = {
    res.arrayOrObject(
      matchFieldAndValues(res, api, trace),
      resArray =>
        api.asArray
          .toRight(FoundError(trace, s"${res.name} is an array but ${api.name} is not"))
          .flatMap(apiArray => arrayOperations(resArray, apiArray, trace.:+(res.name))),
      resObject =>
        api.asObject
          .toRight(FoundError(trace, s"${res.name} is an object but ${api.name} is not"))
          .flatMap(apiObject => objectOperations(resObject, apiObject, trace.:+(res.name)))
    )
  }
  def arrayOperations(res: Vector[Json], api: Vector[Json], trace: List[String]): ErrorEither =
    res
      .zip(api)
      .zipWithIndex
      .foldLeft(Right(List.empty[FoundError]): ErrorEither) {
        case (acc, ((x, y), a)) =>
          val newErrors = fieldAndValueMatchTest(x, y, trace.:+(a.toString))
          traverse(acc, newErrors)
      }

  def objectOperations(res: JsonObject, api: JsonObject, trace: List[String]): ErrorEither = {

    val r =
      for (x <- api.fields.toSet union res.fields.toSet)
        yield
          (res(x), api(x)) match {
            case (Some(e), Some(a)) => fieldAndValueMatchTest(e, a, trace.:+(e.name))
            case (None, Some(_))    => Right(List.empty[FoundError])
            case (Some(e), None)    => Right(List(FoundError(trace, s"${e.name} not found on api")))
            case _                  => Right(List.empty[FoundError]) // mumkun degil
          }
    r.filterNot(x => x.map(y => y.isEmpty).getOrElse(true))
      .filterNot(a => a.map(x => x.head.isEmpty).getOrElse(true))
      .foldLeft(Right(List.empty[FoundError]): ErrorEither)((a, b) => traverse(a, b))

  }

  def matchFieldAndValues(res: Json, api: Json, trace: List[String] = List("root")): ErrorEither = {
    val parseProcess =
      ParseOperations.BigDecimalComparison(res, api, trace) orElse
        ParseOperations.DateTimeComparison(res, api, trace) orElse
        ParseOperations.StringComparison(res, api, trace)
    parseProcess.toRight(FoundError(trace, s" Unknown type on ${res.name}"))
  }

  case class FoundError(trace: List[String], error: String) {
    def tracePrint(): String =
      trace.reduceOption(_ + " -> " + _).getOrElse(if (trace.length == 1) trace.head else "")
    //if (trace.length == 1) trace.head else {trace.reduceOption(_ + " -> " + _).getOrElse()}//if (trace.length < 2) trace.head else trace.reduce(_ + " -> " + _)
    override def toString: String = s"Trace = ${tracePrint()}, Error = $error"

    def isEmpty: Boolean = trace.isEmpty && error != ""
  }

  def traverse(e1: ErrorEither, e2: => ErrorEither): ErrorEither =
    for { a <- e1; b <- e2 } yield a ++ b

  object FoundError {
    def empty = FoundError(List.empty[String], "")
  }
}
