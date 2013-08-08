package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import scala.collection.TraversableOnce
import scala.math.abs


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

  implicit class listInt2Averages(ys: List[Int]) {
    lazy val mode: Option[Int] = {
      // i.e. a unique most frequent value
      val frequencies: Map[Int, Int] = ys.groupBy(y => y) mapValues { _.length }
      val mode = frequencies.toList sortBy { -_._2 } take 2 match {
        case p :: Nil => Some(p._1)
        case p1 :: p2 :: Nil if p1._2 != p2._2 => Some(p1._1)
        case _ => None
      }

      mode
    }

    lazy val median: Int = {
      // i.e. middle or left of middle value when sorted
      ys.sorted.apply(ys.length/2)
    }

    lazy val mean: Double = {
      // i.e. the usual average
      ys.sum.toDouble / ys.length
    }
  }

  def modemedianbucket() = Action {
    // Bucketted Mode then Median downsampling to 100 data points

    val globalMax = (RawData.values map { _._2 }).max
    val globalMin = (RawData.values map { _._2 }).min

    val bucketSize = RawData.values.size / 100
    val buckets = {
      val grouped = RawData.values.grouped(bucketSize).toList

      // Make sure that the ﬁrst and last data points in the original data are
      // also the ﬁrst and last data points in the downsampled data
      // - Done by removing other points from the first and last buckets
      val firstBucket = List(RawData.values.head)
      val middle = grouped.drop(1).take(grouped.length - 2)
      val lastBucket = List(RawData.values.last)

      firstBucket :: middle ::: List(lastBucket)
    }

    val modemedianbucketed = buckets map { bucket: List[(Int,Int)] =>
      // if a global peak or trough is found in the bucket then use that data point
      var selected = bucket.find(List(globalMax, globalMin) contains _._2)

      // o/w if there is a y-value mode (i.e. unique most frequent y) then use
      // the leftmost datapoint with that y
      selected = selected orElse {
        bucket.map(_._2).mode flatMap { mode =>
          bucket.find(_._2 == mode)
        }
      }

      // o/w use the y-value median data point, (i.e. middle or just left of
      // middle y when sorted.)
      selected = selected orElse {
        val median = bucket.map(_._2).median
        bucket.find(_._2 == median)
      }

      selected.get
    }

    Ok(asJson(modemedianbucketed))
  }

  def largesttrianglethreebucket() = Action {
    // Bucketted largest triangle downsampling to 100 data points

    // Split the data into equal number of buckets as the threshold but have the
    // ﬁrst bucket only containing the ﬁrst data point and the last bucket
    // containing only the last data point
    val bucketSize = RawData.values.size / 100
    val buckets = {
      val grouped = RawData.values.grouped(bucketSize).toList

      val firstBucket = List(RawData.values.head)
      val middle = grouped.drop(1).take(grouped.length - 2)
      val lastBucket = List(RawData.values.last)

      firstBucket :: middle ::: List(lastBucket)
    }

    val largesttrianglethreebucket: List[(Int, Int)] = {
      // Select the point in the ﬁrst bucket
      var selected = buckets.head

      val averagePoints: List[(Double, Double)] = buckets map { points =>
        // The average point in a bucket is the average of xs and ys separately.
        (
          (points map { _._1 }).mean,
          (points map { _._2 }).mean
        )
      }

      // for each bucket except the ﬁrst and last do
      (buckets drop 1).zip(averagePoints drop 2) map { case (bucket, next) =>

        // Rank every point in the bucket by calculating the area of a triangle
        // it forms with the selected point in the last bucket and the average
        // point in the next bucket
        val previous = selected.last
        val ranks: List[((Int, Int), Double)] = bucket map { current =>
          val area = 0.5 * abs(
            (next._1 - previous._1) * (current._2 - previous._2) -
              (current._1 - previous._1) * (next._2 - previous._1)
          )

          (current, area)
        }

        // Select the point with the highest rank within the bucket
        selected = selected :+ ranks.maxBy(_._2)._1
      }

      // Select the point in the last bucket
      selected ::: buckets.last
    }

    Ok(asJson(largesttrianglethreebucket))
  }
}
