import org.apache.commons.math3.stat.descriptive.rank.Percentile

import scala.annotation.tailrec
import scala.util.Random

object BinProbs {
  def main(args: Array[String]): Unit = {
    val random = new Random()


    // returns filled count
    //    @tailrec
    //    def fillBins(level: Int, filledCount: Int, remaining: Int): Int = {
    //      if (remaining == 0) {
    //        // All draws completed, return filled count
    //        filledCount
    //      } else {
    //        val trieHit: Boolean = random.nextInt(1 << level) == 0
    //        val newFilledCount = if (trieHit) {
    //          val binHit: Boolean = random.nextInt(32) < filledCount
    //          if (binHit) {
    //            filledCount
    //          } else {
    //            filledCount + 1
    //          }
    //        } else {
    //          // Missed this subtrie, no change to fillCount
    //          filledCount
    //        }
    //        fillBins(level, newFilledCount, remaining - 1)
    //      }
    //    }

    // returns filled count
    @tailrec
    def fillBins(filledCount: Int, remaining: Int): Int = {
      if (remaining == 0) {
        // All draws completed, return filled count
        filledCount
      } else {
        val binHit: Boolean = random.nextInt(32) < filledCount
        val newFilledCount = if (binHit) {
          filledCount
        } else {
          filledCount + 1
        }
        fillBins(newFilledCount, remaining - 1)
      }
    }

    def percentile(seq: Array[Int], p: Double): Int = {
      seq.apply((seq.size * p).round.toInt)
    }

//    val percentile = new Percentile()

//    {
//      val samples: Array[Int] = (for (i <- 0 until 100) yield {
//        val x = fillBins(0, 2)
//        println(x)
//        x
//      }).toArray
//      //println(s"1s: ${samples.filter(_ == 1.0).size}")
//      println(percentile(samples, 0.95))
//    }


    //println("filled\tinsertions")
    //    for (level <- Seq(0)) {
    for (filledCount <- (0 until 32)) {

      var insertions = 0
      @tailrec
      def findInsertionsForNewSize(newSize: Int): Unit = {
        val samples: Array[Int] = (for (i <- 0 until 10000) yield {
          val filled: Int = fillBins(filledCount, insertions)
          //println(filled)
          filled
        }).sorted.toArray
        val sampledNewSize = percentile(samples, 0.75)
        //println(s"filled: $filledCount\tpending: $insertions\tprediction: $sampledNewSize\tgoal:$newSize")
        if (sampledNewSize < newSize) {
          insertions += 1
          findInsertionsForNewSize(newSize)
        } else {
          println(s"$sampledNewSize (${sampledNewSize - percentile(samples, 0.25)})")
        }
      }

      val insertionSizeTable = for (newSize <- (filledCount+1 to 32)) yield {
        findInsertionsForNewSize(newSize)
        insertions
      }
      //print(s"/* $filledCount */ ")
      println(insertionSizeTable.mkString("Array(", ", ", "),"))
    }
  }
}


            //val filledPercentile = percentile.evaluate(samples, 0.8)
//            def perc(p: Double): Double = (percentile.evaluate(samples, p) * 10).round / 10
//            val p10 = perc(0.10)
//            val p50 = perc(0.50)
//            val p90 = perc(0.90)
//            println(s"$filledCount\t$remaining\t\t$p10\t$p50\t$p90")
//            println(s"$filledCount\t$remaining\t\t$p10\t$p50\t$p90")

//        }

//          val est = if (remainingLog2 >= 8) {
//            32
//          } else {
//            val empty = 32 - filledCount
//            val growth = Math.min(1 + (4 * remainingLog2), remaining)
//            filledCount + growth
//          }

//          println(s"$level\t$filledCount\t$remaining\t$p50\t$est")


          //val filledPercentile = percentile.evaluate(samples, 0.8)
          //val cleaned = -1 * (filledCount - 32)/(filledPercentile / 32)
          //println(s"$level\t$filledCount\t$remaining\t$filledPercentile\t${remainingLog2}\t${1 << level}\t$cleaned")
//        }
//      }
//    }

    /*
    0  0  1  1.0    1 -1025.0
0  0  4  2.0    1 -513.0
0  0  8  5.0    1 -205.8
0  0  16  9.0    1 -114.77777777777777
0  0  32  16.0    1 -65.0
0  0  128  29.0    1 -36.310344827586206
0  0  256  31.0    1 -34.03225806451613
0  0  512  32.0    1 -33.0
0  0  1024  32.0    1 -33.0
0  0  2048  32.0    1 -33.0
0  0  4096  32.0    1 -33.0
0  0  8192  32.0    1 -33.0
0  0  16384  32.0    1 -33.0

     */

//    def needs(level: Int, levelSize: Int, remaining: Int): Boolean = {
//      val origBitmap =
//    }
//
//    var draws = 0
//    var count = 0
//    var bits = 0
//    while (count < 32) {
//      draws += 1
//      val index = random.nextInt(32)
//      val bit = 1 << index
//      if ((bits & bit) == 0) {
//        count += 1
//      }
//      bits |= bit
//    }
//}
