import io.circe.{HCursor, Json, JsonObject, Parser}
import org.http4s.client.blaze._
import org.http4s.circe._
import io.circe.parser._

import scala.collection.IterableLike

object apiTest {
  def main(args: Array[String]): Unit = {
    val resApiUrl = "https://cheetah.rc-socrata.com/resource/y8rf-3nrp"
    val apiUrl = "https://cheetah.rc-socrata.com/api/odata/v4/y8rf-3nrp"

    val httpClient = PooledHttp1Client()

    val resApiUrlCall = httpClient.expect[Json](resApiUrl)
    val apiUrlCall = httpClient.expect[Json](apiUrl)
    println("Sending request")

    val error = for {
      resApiJsonBody <- resApiUrlCall
      apiJsonBody <- apiUrlCall
    } yield {
      println(resApiJsonBody)
      println(apiJsonBody)
      val api = "{\n  \"@odata.context\" : \"https://cheetah.rc-socrata.com/api/odata/v4/$metadata#y8rf-3nrp\",\n  \"value\" : [\n    {\n      \"__id\" : \"row-eavs~fc8k~iewa\",\n      \"number_us_format\" : 123456,\n         \"percent_us_format\" : 1,\n      \"percent_spain_format\" : 0,\n      \"date_us_format\" : \"2016-03-21T00:00:00Z\",\n      \"date_spain_format\" : \"2016-03-21T00:00:00Z\",\n      \"us_money\" : 2500.00,\n      \"euro_money\" : 25.00,\n      \"non_special_characters\" : \"Republic\",\n      \"special_characters\" : \"Republicà\"\n    },\n    {\n      \"__id\" : \"row-ynau_6pqm~5ant\",\n      \"number_us_format\" : 234567,\n         \"percent_us_format\" : 2,\n      \"percent_spain_format\" : 0,\n      \"date_us_format\" : \"2016-04-22T00:00:00Z\",\n      \"date_spain_format\" : \"2016-04-22T00:00:00Z\",\n      \"us_money\" : 12500.00,\n      \"euro_money\" : 125.00,\n      \"non_special_characters\" : \"Yes\",\n      \"special_characters\" : \"Sí\"\n    },\n    {\n      \"__id\" : \"row-3ced_pppd.e7py\",\n      \"number_us_format\" : 345678,\n          \"percent_us_format\" : 3,\n      \"percent_spain_format\" : 0,\n      \"date_us_format\" : \"2016-05-23T00:00:00Z\",\n      \"date_spain_format\" : \"2016-05-23T00:00:00Z\",\n      \"us_money\" : 112500.00,\n      \"euro_money\" : 1125.00,\n      \"non_special_characters\" : \"Words\",\n      \"special_characters\" : \"Alt Àneu\"\n    }\n  ]\n}"

      val res = "[\n  {\n    \"number_spain_format\" : \"1234567\",\n    \"percent_us_format\" : \"123\",\n    \"date_spain_format\" : \"2016-03-21T00:00:00\",\n    \"euro_money\" : \"25.00\",\n    \"non_special_characters\" : \"Republic\",\n    \"percent_spain_format\" : \"12.3\",\n    \"date_us_format\" : \"2016-03-21T00:00:00\",\n    \"us_money\" : \"2500.00\",\n    \"special_characters\" : \"Republicà\",\n    \"number_us_format\" : \"123456\"\n  },\n  {\n    \"number_spain_format\" : \"2345.67\",\n    \"percent_us_format\" : \"234\",\n    \"date_spain_format\" : \"2016-04-22T00:00:00\",\n    \"euro_money\" : \"125.00\",\n    \"non_special_characters\" : \"Yes\",\n    \"percent_spain_format\" : \"23.4\",\n    \"date_us_format\" : \"2016-04-22T00:00:00\",\n    \"us_money\" : \"12500.00\",\n    \"special_characters\" : \"Sí\",\n    \"number_us_format\" : \"234567\"\n  },\n  {\n    \"number_spain_format\" : \"3456.78\",\n    \"percent_us_format\" : \"345\",\n    \"date_spain_format\" : \"2016-05-23T00:00:00\",\n    \"euro_money\" : \"1125.00\",\n    \"non_special_characters\" : \"Words\",\n    \"percent_spain_format\" : \"34.5\",\n    \"date_us_format\" : \"2016-05-23T00:00:00\",\n    \"us_money\" : \"112500.00\",\n    \"special_characters\" : \"Alt Àneu\",\n    \"number_us_format\" : \"345678\"\n  }\n]"
      //test(resApiJsonBody.toString(), apiJsonBody.toString())
      test(res, api)


      /* resApiJson1.asObject.get.toMap.map(_._1).map( field => apiJson1.asObject.get.apply(field) match {
        case None => println(field + " is not in api")
      })
      println(resApiJson1.asObject)*/

      /*val resDifferences = apiJson.head.asObject.get.fields.diff(resApiJson.head.asObject.get.fields)
      val apiDifferences = resApiJson.head.asObject.get.fields.diff(apiJson.head.asObject.get.fields)
      println(apiJson.map(_.asObject.get.toList).map(_.filter( a => !resDifferences.contains(a._1)).sortBy(_._1)))
      resApiJson.map(x => x.asObject.get.toList).map(_.filter( a => !apiDifferences.contains(a._1)).sortBy(_._1))
        .zip(apiJson.map(_.asObject.get.toList).map(_.filter( a => !resDifferences.contains(a._1)).sortBy(_._1)))*/
      //.map(a => a._1.zip(a._2).map())
      //.map((x,y) => x._1.zip(x._2).map())
      /* println(resApiJson.map(x => x.asObject.get.toList.sortBy(_._1))
        .zip(apiJson.map(_.asObject.get.filter( a => !differences.contains(a._1)).toList))
      )*/
    }
    error.unsafePerformSync
  }
  def parse(resApiResponseRaw: String, apiResponseRaw: String) = {

  }
  def test(resApi: String, api: String) = {
    val resApiJsonTry = parse(resApi)
    val apiJsonTry = parse(api)

    val tryParsing = for{
      resApiJson <- resApiJsonTry
      apiJson <- apiJsonTry
    } yield {
      val tryParsingToObjects = for{
        resApiJsonValue <- resApiJson.asArray
        apiJsonValue <- apiJson.asObject.get.apply("value").get.asArray
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
      tryParsingToObjects.getOrElse(Some("There has been an error with parsing json to object"))
    }
    tryParsing.getOrElse(println("There has been an error with parsing json"))
  }
}
case class Error(str: String)
