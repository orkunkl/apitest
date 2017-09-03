package ApiTest

import DataTypes.DataTypes._
import com.github.nscala_time.time.RichDateTime
import io.circe.{Json, _}
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client.blaze.{PooledHttp1Client, _}
import com.typesafe.scalalogging._
import io.circe.Decoder.Result
import org.http4s.client.Client
import com.github.nscala_time.time.Imports._
import io.circe.Json.{JArray, JObject}

//import scala.collection.mutable.RedBlackTree.Tree
import scala.collection.{SortedMap, mutable}
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
      apiJsonValue <- apiJson.as[JsonObject].flatMap(_.apply("value").toRight("value not found on api"))
      testResult <- test(resApiJson, apiJsonValue)
    } yield testResult


    tests match {
      case Left(c) => println("\n" + c)
      case Right(x) => if(x.nonEmpty) println("\n" + x) else println("\nNo errors")
    }
    httpClient.shutdownNow()
  }

  def test(resApi: Json, api: Json): Either[String, List[String]] =
    for {
      _ <- sizeTest(resApi, api)
      testResult <- fieldSizeTest(resApi.head, apiJsonArray.head, List.empty[String])
    } yield testResult

  def createUrls(httpClient: Client): Either[Object, (Uri, Uri)] =
    for {
      apiSetsUrl <- Uri.fromString("https://cheetah.rc-socrata.com/api/odata/v4/").toEither
      apiUrlCall <- Try(httpClient.expect[Json](apiSetsUrl).unsafePerformSync).toEither
      _ = logger.info("sending request to " + apiSetsUrl)
      apiRetrieveValues <- apiUrlCall.hcursor.get[Json]("value")
      apiRandomUrl <- apiRetrieveValues.asArray.toRight("could not parse value to jsonArray")
                        .flatMap(x => apiRetrieveValues.hcursor.downN(Random.nextInt(x.size)).get[String]("url"))

      _ = logger.info("apiRandomUrl " + apiRandomUrl)

      apiUrl = apiSetsUrl / apiRandomUrl
      _ = logger.info("apiUrl " + apiUrl)

      resUrl <- Uri.fromString("https://cheetah.rc-socrata.com/resource/" + apiRandomUrl).toEither

    } yield (resUrl, apiUrl)

  // Unacceptable test. Stops the whole test if size test fails
  def sizeTest(res: Json, api: Json): Either[String, Unit] = {
    (res, api) match {
      case _ if(res.isArray && api.isArray) =>
        val resApiSize = res.asArray.get.size
        val apiSize = api.asArray.get.size
        if(resApiSize == apiSize) Right()
        else Left("Sizes are not equal, resource = " + resApiSize + ", api = " + apiSize)
      case _ if(res.isObject && api.isObject) => Right()
      case _ => Left("Resource api and api json types do not match! resource = " +
                        res.arrayOrObject("", _ => "array", _ => "object") + ", api = " + api.arrayOrObject("", _ => "array", _ => "object"))

    }
  }
  // Acceptable test. Does not stop the whole test if size test fails
  def fieldMatchTest(res: Json, api: Json, indentation: Int = 1): Either[String, List[String]] = {
    res.arrayOrObject(
      Right(List.empty[String]),
      resArray => {
        api.asArray.toRight((0 to indentation).map(_ => "\t").mkString("") + api.name + " is not an array as")
          .map(apiArray => resArray.zip(apiArray).)
      }
    )
  }


  /*def ValueMatchTest[A](res: Json, api: Json, errorList: List[String], indentation: Int = 1)(f: (Json, Json) => A): Either[String, List[A]] = {
    res.fold(
      if(api.isNull) Right(List.empty[String]) else Left(res + " is null but api is not"),
      boolean => if(api.isBoolean) f()

    )}*/



  def fieldMatchTest(resApi: JsonObject, api: Json, errorList: List[String]): Either[String, List[String]] = ???



  //def compareArrays


  def parseToObject(json: Json, name: String): Either[String, JsonObject] =
    json.asObject match { case Some(x) => Right(x) case None => Left("parse error on " + name + " to JsonObject") }

  def parseToArray(json: Json, name: String): Either[String, Vector[JsonObject]] =
    json.asArray.toRight("error on parsing " + name + " to array").map(x => x.flatMap(_.asObject))

  /*def zipJsons[A](resJson: Json, apiJson: Json)(f: Json => A): Either[String, Traversable[A]] = {
    resJson.toList.foldLeft(rhs) {
      case (acc, (key, value)) =>
        rhs(key).fold(acc.add(key, value)) { r => acc.add(key, value.deepMerge(r)) }
    }
  }*/
  //def typeParser(value: String)
}
