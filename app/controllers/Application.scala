package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import scala.collection.TraversableOnce


object Application extends Controller {

  def index(data: String) = Action {
    Ok(views.html.index(data))
  }

  def asJson(data: TraversableOnce[(Int, Int)]): JsValue =
    Json.toJson(for ((x, y) <- data.toList) yield Json.arr(x * 1000, y))

  def original() = Action {
    Ok(asJson(RawData.values))
  }

  def sampled() = Action {
    // simple downsample to 100 data points
    val bucketSize = RawData.values.size / 100

    val sampled = RawData.values.grouped(bucketSize).map(_.head).toList

    Ok(asJson(sampled))
  }

  def averaged() = Action {
    // simple average to 100 data points
    val bucketSize = RawData.values.size / 100
    val buckets = RawData.values.grouped(bucketSize)

    val averaged = buckets.map { l =>
      val sum = l.map(_._2).sum
      val avg = sum / l.size
      l.head._1 -> avg
    }

    Ok(asJson(averaged))
  }

  def modemedianbucket() = Action {
//    val bucketSize = RawData.values.size / 100
//    val buckets = RawData.values.grouped(bucketSize)
//
//    val globalMax = RawData.values.maxBy(_._2)
//    val globalMin = RawData.values.minBy(_._2)
//
//    for (bucket <- buckets) yield {
//      bucket.find(_._2 == globalMax && _._2 == globalMin) orElse {
//
//      }
//    }


    Ok(asJson(RawData.values))
  }
}
