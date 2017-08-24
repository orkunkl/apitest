package Apitest

import DataTypes.DataTypes._
import com.github.nscala_time.time.RichDateTime
import io.circe._
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client.blaze.{PooledHttp1Client, _}
import com.typesafe.scalalogging._
import io.circe.Decoder.Result
import org.http4s.client.Client
import com.github.nscala_time.time.Imports._

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

      callTry <- Try(httpClient.expect[Json](resUrl).unsafePerformSync, httpClient.expect[Json](apiUrl).unsafePerformSync).toEither
      (resApiJson, apiJson) = callTry
      _ = logger.info("resApiJson " + resApiJson)
      _ = logger.info("apiJson " + apiJson)

      resApiJsonObject <- parseToArray(resApiJson, "Resource api")
      apiJsonObject <- parseToObject(apiJson, "api")
      testResult <- test(resApiJsonObject, apiJsonObject)
    } yield testResult


    tests match {
      case Left(c) => println("\n" + c)
      case Right(x) => if(x.nonEmpty) println("\n" + x) else println("\nNo errors")
    }
    httpClient.shutdownNow()
  }

  def test(resApi: Vector[JsonObject], api: JsonObject): Either[String, List[String]] =
    for {
      apiJsonArrayUnparsed <- api.apply("value").toRight("parsing error to value")
      apiJsonArray <- parseToArray(apiJsonArrayUnparsed, "api")
      _ <- sizeTest(resApi, apiJsonArray)
      testResult <- fieldSizeTest(resApi.head, apiJsonArray.head, List.empty[String])
    } yield testResult

  def createUrls(httpClient: Client): Either[Object, (Uri, Uri)] =
    for {
      apiSetsUrl <- Uri.fromString("https://cheetah.rc-socrata.com/api/odata/v4/").toEither
      apiUrlCall <- Try(httpClient.expect[Json](apiSetsUrl).unsafePerformSync).toEither
      _ = logger.info("sending request to " + apiSetsUrl)
      apiRetrieveValues <- apiUrlCall.hcursor.get[Json]("value")
      apiRandomUrl <- apiRetrieveValues.asArray.toRight("could not parse value to jsonArray").flatMap(x => apiRetrieveValues.hcursor.downN(Random.nextInt(x.size)).get[String]("url"))

      _ = logger.info("apiRandomUrl " + apiRandomUrl)

      apiUrl = apiSetsUrl / apiRandomUrl
      _ = logger.info("apiUrl " + apiUrl)

      resUrl <- Uri.fromString("https://cheetah.rc-socrata.com/resource/" + apiRandomUrl).toEither

    } yield (resUrl, apiUrl)

  def sizeTest(res: Vector[JsonObject], api: Vector[JsonObject]): Either[String, Unit] = {
    if(res.size == api.size)
      if(res.isEmpty) Left("Empty values, No errors have found") else Right()
    else
      Left("sizes do not match. Expected = " + res.size + " Found = " + api.size)
  }

  def fieldSizeTest(res: JsonObject, api: JsonObject, errorList: List[String]): Either[String, List[String]] = {
    val diff = res.fields.diff(api.fields)
    if(diff.nonEmpty) Right(errorList ++ diff.toList)
    else Right(errorList)
  }


  def matchTest(resApi: Json, api: Json): Either[String, List[String]] = {
    for {
      resApiFields <- resApi.hcursor.fields.toStream
      apiFields <- api.hcursor.fields
    } yield {
      resApiFields.foldLeft(Right(List.empty): Either[String, List[String]])((b,a) =>
        val parseResValue = for {
          resValue <- resApi.hcursor.get[Value](a).left
          resValue <- resApi.hcursor.get[](a).left
        } yield resValue
      )

    }
  }



  def parseToObject(json: Json, name: String): Either[String, JsonObject] =
    json.asObject match { case Some(x) => Right(x) case None => Left("parse error on " + name + " to JsonObject") }

  def parseToArray(json: Json, name: String): Either[String, Vector[JsonObject]] =
    json.asArray.toRight("error on parsing " + name + " to array").map(x => x.flatMap(_.asObject))

  def typeParser(value: String)
}
