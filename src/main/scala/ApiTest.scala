package apitest

import java.io.File
import java.net.URL

import io.circe.Json.JArray
import io.circe.{HCursor, Json, JsonObject, Parser}
import org.http4s.client.blaze._
import org.http4s.circe._
import io.circe.parser

import scala.collection.IterableLike
import scala.util.{Failure, Success, Try}

object ApiTest {
  def main(args: Array[String]): Unit = {

  }
  def parseData(resApiResponseRaw: String, apiResponseRaw: String): Option[(Json,Json)] = {
    val resApiJsonTry = parse(resApiResponseRaw)
    val apiJsonTry =  parse(apiResponseRaw)

    val tryParsing = for{
      resApiJson <- resApiJsonTry
      apiJson <- apiJsonTry
    } yield Some((resApiJson, apiJson))
    tryParsing.getOrElse(None)
  }
  def test(resApi: Json, api: Json) = {
   /* val apiFormat = root.value.arr
    val resFormat = root.arr

    resFormat.getOption()
    apiFormat.getOption(resApi)

      val tests = for {
        sizeTest <- sizeTest(resApi, api)
        matchTest <- matchTest(resApi, api)
      } yield {
        ()
      }
*/
      /*for {}
      val tryParsingToObjects = for{
        resApiJsonValue <- resApi.asArray
        apiJsonValue <- api.asObject.get.apply("value").get.asArray
      } yield {

        println("Data has been received")
        val resApiJsonValueSize = resApiJsonValue.size
        val apiJsonValueSize = apiJsonValue.size

        (if(resApiJsonValueSize != apiJsonValueSize)
          Some("sizes do not match! Resource api size = " + resApiJsonValueSize + " api size = " + apiJsonValueSize + "\n")
        else None) match {
          case Some(err) => Error(err)
          case None =>
            resApiJsonValue.head.hcursor.fields.get.foldLeft(Seq.empty[Error])((errors, field) =>
              apiJsonValue.head.asObject.get.apply(field) match {
               case None => errors.+: (Error(field + " not found in api"))
              }) match {
                case Seq.empty => None
                case _ => Some(_)
              }
        }
      }
      tryParsingToObjects.getOrElse(Some("There has been an error with parsing json to object"))*/
  }
  def sizeTest(res: Vector[Json], api: Vector[Json]): Either[String, Unit] = {
    if(res.size == api.size)
      Right()
    else
      Left("sizes do not match. Expected = " + res.size + " Found = " + api.size)
  }


  def matchTest(res: Vector[Json], api: Vector[Json]): Either[String, Unit] = ???

  def parse(raw: String): Option[Json] =
    parser.parse(raw).toOption

  def parse(file: File): Option[Json] =
    Try(scala.io.Source.fromFile(file).mkString).toOption.flatMap(parse)
}
case class Error(str: String)

