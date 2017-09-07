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
import io.circe.syntax._

object ApiTest {

  val logger = Logger("test")

  def main(args: Array[String]): Unit = {
    val json3asJson = Map("ford" -> "fast").asJson
    val httpClient = PooledHttp1Client()
    val resUrl = "https://cheetah.rc-socrata.com/resource/y8rf-3nrp"
    val apiUrl = "https://cheetah.rc-socrata.com/api/odata/v4/y8rf-3nrp"
    for {
      callTry <- Try(httpClient.expect[Json](resUrl).unsafePerformSync, httpClient.expect[Json](apiUrl).unsafePerformSync).toEither
      (resJson, apiJson) = callTry
      apiJsonValue <- apiJson.as[JsonObject].flatMap(_.apply("value").toRight("value not found on api"))
    } yield {
      println(resJson)
      println(apiJsonValue)


    }
  }

/* def main(args: Array[String]): Unit = {

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
     _ = println("Resource api json " + resApiJson)
     _ = println("Api json " + apiJson)

     apiJsonValue <- apiJson.as[JsonObject].flatMap(_.apply("value").toRight("value not found on api"))
     testResult <- test(resApiJson, apiJsonValue)
   } yield testResult


   tests match {
     case Left(c) => println("\n" + c)
     case Right(x) => if(x.nonEmpty) println("\n" + x) else println("\nNo errors")
   }
   httpClient.shutdownNow()
 }*/

 /*def test(resApi: Json, api: Json): Either[String, List[String]] =
   for {
     _ <- sizeTest(resApi, api)
     testResult <- fieldSizeTest(resApi.head, apiJsonArray.head, List.empty[String])
   } yield testResult
*/
 def createUrls(httpClient: Client): Either[Object, (Uri, Uri)] =
   for {
     apiSetsUrl <- Uri.fromString("https://cheetah.rc-socrata.com/api/odata/v4/").toEither
     apiUrlCall <- Try(httpClient.expect[Json](apiSetsUrl).unsafePerformSync).toEither
     _ = logger.info("sending request to " + apiSetsUrl)
     apiRetrieveValues <- apiUrlCall.hcursor.get[Json]("value")
     apiRandomUrl <- apiRetrieveValues.asArray.toRight("could not parse value to jsonArray")
                       .flatMap(x => apiRetrieveValues.hcursor.downN(Random.nextInt(x.size)).get[String]("url"))

     _ = logger.info("apiRandomUrl " + apiRandomUrl)
     _ = println("Chosen random api url " + apiRandomUrl)

     apiUrl = apiSetsUrl / apiRandomUrl
     _ = logger.info("apiUrl " + apiUrl)

     resUrl <- Uri.fromString("https://cheetah.rc-socrata.com/resource/" + apiRandomUrl).toEither
     _ = println("Chosen random resource api url " + resUrl)
     _ = println("Chosen random api url " + apiUrl)

   } yield (resUrl, apiUrl)

 // Unacceptable test. Stops the whole test if size test fails
 def sizeTest(res: Json, api: Json): Either[String, Unit] = {
   (res, api) match {
     case _ if(res.isArray && api.isArray) =>
       val resApiSize = res.asArray.get.size
       val apiSize = api.asArray.get.size
       if(resApiSize == apiSize) Right()
       else Left("Sizes are not equal, resource = " + resApiSize + ", api = " + apiSize)
     case _ if res.isObject && api.isObject => Right()
     case _ => Left("Resource api and api json types do not match! resource = " +
                       res.arrayOrObject("", _ => "array", _ => "object") + ", api = " + api.arrayOrObject("", _ => "array", _ => "object"))
   }
 }
 // Acceptable test. Does not stop the whole test if size test fails except the case of types of focused jsons are different
 def fieldMatchTest(res: Json, api: Json, trace: List[String] = List.empty[String]): Either[String, List[FoundError]] = {
   res.arrayOrObject({
     BigDecimalParse(res, api, trace.+:(res.name)) orElse
       Option(if (res.toString() == api.toString()) List.empty else List(FoundError(trace, ""))) toRight ("")
   },
     resArray =>
       api.asArray.toRight(res.name + " is an array but " + api.name + " is not").flatMap(apiArray =>
         arrayOperations(resArray, apiArray, trace)
       ),
     resObject =>
       api.asObject.toRight(res.name + " is an object but " + api.name + " is not").flatMap(
         apiObject => {
           objectOperations(resObject, apiObject, trace)
         }
       )
   )
 }
  def arrayOperations(res: Vector[Json], api: Vector[Json], trace: List[String]) =
    res.zip(api).zipWithIndex.foldLeft(Right(List.empty[FoundError]): Either[String, List[FoundError]])((acc,b) =>
      acc.flatMap(x => fieldMatchTest(b._1._1, b._1._2, trace.+:(b._2.toString)).map(err => x ++ err)))

  def objectOperations(res: JsonObject, api: JsonObject, trace: List[String]) = {
    val resList = res.toList
    val apiList = api.toList
    val resMinusApi = res.toList.filterNot(element => apiList.map(_._1).contains(element._1))
    val apiMinusRes = api.toList.filterNot(element => resList.map(_._1).contains(element._1))

    val fieldDifferences = resMinusApi.map(x => FoundError(trace, x._1))
    val resDiffed = resList.diff(resMinusApi).sortBy(_._1)
    val apiDiffed = apiList.diff(apiMinusRes).sortBy(_._1)
    resDiffed.zip(apiDiffed).foldLeft(Right(List.empty[FoundError]): Either[String, List[FoundError]])((errorsRight,e) =>
      errorsRight.flatMap(errors => fieldMatchTest(e._1._2, e._2._2, trace.+:(e._1._1)).map(x => errors ++ x))
    )
    /*val resList = res.toList
    val apiList = api.toList
    val differencesRes = resList.filterNot(key => apiList.map(_._1).contains(key._1))
    val differencesApi = apiList.filterNot(key => resList.map(_._1).contains(key._1))
    val differences = differencesRes
    resList.diff(differencesRes).sortBy(_._1).map(_._2).zip(apiList.diff(differencesApi).sortBy(_._1).map(_._2))
      .foldLeft(Right(List.empty[String]): Either[String, List[String]])((acc,b) =>
        acc.map(x => fieldMatchTest(b._1, b._2, trace.+:(b._1)).map(y => x ++ y)).joinRight)
      .map(a => a ++ differences.map("Field " + _._1 + " not found on api") )*/
  }


  def BigDecimalParse(res: Json, api: Json, trace: List[String]) = {
    val resValue = res.asString.flatMap(x => Try(BigDecimal(x)).toOption) orElse res.asNumber.flatMap(x => Try(x.toBigDecimal).toOption)
    val apiValue = api.asString.flatMap(x => Try(BigDecimal(x)).toOption) orElse api.asNumber.flatMap(x => Try(x.toBigDecimal).toOption)
    for{
      resDecimal <- resValue.map(_.asInstanceOf[BigDecimal])
      apiDecimal <- apiValue.map(_.asInstanceOf[BigDecimal])
    } yield {
      (resDecimal, apiDecimal) match {
        case x if x._1 == x._2 => List.empty
        case x if x._1 /n 100 == x._2 => List.empty
        case x if x._1 != x._2 => List(FoundError(trace, "Value of " + res.name + " does not match with " + api.name + "\n resource = " + x._1 + " api = " + x._2))
      }
    }
  }

  case class FoundError(trace: List[String], error: String)
}

