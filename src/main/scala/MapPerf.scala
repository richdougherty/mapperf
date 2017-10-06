package scala.collection.immutable

import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.immutable.HashMap.{HashMap1, HashTrieMap}

object MapPerf {

  def main(args: Array[String]): Unit = {
    val hashMap = apply("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)
    println(hashMap)
  }

  protected final def improve(hcode: Int): Int = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  /*
  for inputs:
  x -> ab3
  y -> a4c
  z -> bxf

  a -> (b3, x)
    -> (4c, y)
  b -> (bxf, z)


  https://en.wikipedia.org/wiki/Coupon_collector%27s_problem
  - average 135 elements to flip all 32 bits


  scan
  - record last hash
  - store starting point for each level
  - go back and create once reached that level

  first hash is 'ab3'

  var cursor = 0
  def createHashMap(level: Int): HashMap = {

  }


  - generate bitmap for children of ''
    - record starting point
    - zero the bitmap
    - scan all items that start with ''
    - encounter 'ab3'
      - char after prefix '' is 'a'
      - set bitmap bit for 'a'
    - encounter 'a4c'
      - char after prefix '' is 'a'
      - set bitmap bit for 'a' (again!)
    - encounter 'bxf'
      - char after prefix '' is 'b'
      - set bitmap bit for 'b'
    - finished all children (could quick exit if bitmap equals 0xfffff...)
    - count bits
    - if bits == 1
      - create HashMap1
    - if bits > 1
      - create array num_set(bits)
      - arr = createHashMap(level + 5)


      - stop when - reach new prefix (or all bits flipped?)
      - count bits
        - 1 - create hashmap1
        - 1+ - create


    - record starting point
    - scan all items that start with 'a'
    - flip bits based on char following
    - stop when - reach new prefix (or all bits flipped?)
    - count bits
      - 1 - create hashmap1
      - 1+ - create


   */

  def apply[K,V](elems: (K, V)*): HashMap[K,V] = {

    println("Calculating hashAndIndex array")

    // PERF: Handroll
    val hashAndIndex: Array[Long] = elems.zipWithIndex.map { case ((key, value), index) =>
      val hash = improve(key.hashCode)
      val x = (hash.toLong << 32) | index
      println(s"Storing ${hash.toHexString}, $index as $x")
      x
    }.toArray

    Arrays.sort(hashAndIndex)

    println(s"Sorted hashAndIndex array: ${hashAndIndex.mkString}")

    var i = 0

    def createHashMap(level: Int): HashMap[K,V] = {
      var prefix = -1
      def debug(msg: String): Unit = {
        println(s"${prefix.toHexString}, $level, $i: $msg")
      }

      debug(s"Called createHashMap($level) at index $i")
      val start = i
      var bitmap = 0
      val prefixMask = (1 << level) - 1
      debug(s"Prefix mask is ${prefixMask.toHexString}")

      prefix = {
        val hash = (hashAndIndex(i) >>> 32).toInt
        debug(s"First child hash is ${hash.toHexString}")
        hash & prefixMask
      }
      debug(s"Prefix is ${prefix.toHexString}")

      // Get bitmap for children

      val childLevel = level + 5

      @tailrec
      def populateBitmap(): Unit = {
        if (i < hashAndIndex.size) {
          val childHash = (hashAndIndex(i) >>> 32).toInt
          debug(s"Scanning child ${childHash.toHexString}")
          if (((childHash & prefixMask) ^ prefix) == 0) {
            debug(s"Child ${childHash.toHexString} matches prefix")
            val index = (childHash >>> childLevel) & 0x1f
            debug(s"Child index in bitmap is $index")
            bitmap |= (1 << index)
            if (bitmap != -1) {
              i += 1
              populateBitmap()
            }
          }
        }
      }

      populateBitmap()

      i = start

      // Create hash

      val childCount = Integer.bitCount(bitmap)
      debug(s"Child count for prefix ${prefix.toHexString} is $childCount")
      if (childCount == 1) {
        val childHashAndIndex = hashAndIndex(i)
        val hash = (childHashAndIndex >>> 32).toInt
        val elemsIndex = childHashAndIndex.toInt
        val kv = elems(elemsIndex)
        i += 1
        new HashMap1[K,V](kv._1, hash, kv._2, kv)
      } else {
        val childArray = new Array[HashMap[K,V]](childCount)
        var size = 0

        @tailrec
        def addChildren(): Unit = {
          if (i < hashAndIndex.size) {
            val childHash = (hashAndIndex(i) >>> 32).toInt
            debug(s"Child hash is ${childHash.toHexString}")
            if (((childHash & prefixMask) ^ prefix) == 0) {
              val index = (childHash >>> childLevel) & 0x1f
              debug(s"Child ${childHash.toHexString} index in bitmap is $index")
              val offset = Integer.bitCount(bitmap & ((1 << index) - 1))
              debug(s"Child ${childHash.toHexString} index in array is $offset")
              val childHashMap = createHashMap(childLevel)
              debug(s"Got child ${childHash.toHexString} HashMap: $childHashMap")
              childArray(offset) = childHashMap
              size += childHashMap.size
              addChildren()
            }
          }
        }
        addChildren()

        new HashTrieMap[K,V](bitmap, childArray, size)
      }

    }

    val r = createHashMap(0)
    println(s"Created HashMap at level 0 $r")
    r

  }

}