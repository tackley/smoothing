import java.io.File
import play.api._
import play.api.libs.json.{JsArray, Json}
import scalax.io.Resource

object Stuff extends App {

  val app = new DefaultApplication(new File("."), getClass.getClassLoader, None, Mode.Dev)

  val jsonString = Resource.fromInputStream(app.resourceAsStream("/public/data.json").get).string

  val json = Json.parse(jsonString)


  val today = ((json \ "seriesData")(1) \ "data").as[JsArray]


  for (obj <- today.value) {
    val x = (obj \ "x").as[Long]
    val y = (obj \ "y").as[Long]
    println(s"$x -> $y,")
  }

}