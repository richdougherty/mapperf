package scala.collection.immutable

import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.immutable.HashMap.{HashMap1, HashMapCollision1, HashTrieMap}

object MapPerf {

  def main(args: Array[String]): Unit = {
    val tests = Seq[Seq[(String,Int)]](
      Seq("a" -> 1),
      Seq("a" -> 1, "b" -> 2),
      Seq("1" -> 1, "6" -> 6),
      Seq("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4),
      Seq("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "d" -> 44),
      (0 until 7).map(i => (i.toString -> i))
    )

    for (test <- tests) {
      println(s"------ Testing $test ------")
      //val slow = Map.apply(test: _*)
      val slow: HashMap[String, Int] = ((new HashMap) ++ (test)).asInstanceOf[HashMap[String, Int]]
      val fast = apply(test: _*)
      println("slow:")
      printHashMap(slow)
      println(s"fast: $fast")
      printHashMap(fast)
      assert(slow.size == fast.size, s"fast.size -> ${fast.size}")
      for ((k,_) <- test) {
        assert(slow.get(k) == fast.get(k), s"fast.get($k) -> ${fast.get(k)}")
      }
    }
  }

  def printHashMap(hm: HashMap[_,_], indent: String = ""): Unit = {
    print(indent)
    hm match {
      case hm1: HashMap1[_, _] =>
        println(s"HashMap1(${hm1.hash.toHexString}, ${hm1.key} -> ${hm1.value})")
      case htm: HashTrieMap[_, _] =>
        println(s"HashTrieMap(bitmap = ${htm.bitmap.toHexString}, size = ${htm.size}):")
        for (elem <- htm.elems) {
          printHashMap(elem, indent = indent + "- ")
        }
      case hmc1: HashMapCollision1[_, _] =>
        println(s"HashMapCollision1(${hmc1.hash.toHexString}, ${hmc1.kvs})")
      case _: HashMap[_, _] if hm.isEmpty =>
        println("HashMap")
    }
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

    //println("Calculating hashAndIndex array")

    val numElems = elems.length

    val hashAndIndex: Array[Long] = {
      val arr = new Array[Long](numElems)
      var i = 0
      while (i < numElems) {
        val key = elems(i)._1
        val hash = improve(key.hashCode)
        val x = (hash.toLong << 32) | i
        println(s"Storing ${hash.toHexString}, $i as ${x.toHexString}")
        arr(i) = x
        i += 1
      }
      arr
    }

    Arrays.sort(hashAndIndex)

    println(s"Sorted hashAndIndex array: ${hashAndIndex.map(_.toHexString).mkString(", ")}")

    var i = 0

    def createHashMap(level: Int): HashMap[K,V] = {
      if (level < 32) {

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

//        val childLevel = level + 5
        //val childMask = ((1 << 5) - 1) << childLevel

        @tailrec
        def populateBitmap(): Unit = {
          if (i < numElems) {
            val childHash = (hashAndIndex(i) >>> 32).toInt
            debug(s"Scanning child ${childHash.toHexString}")
            if (((childHash & prefixMask) ^ prefix) == 0) {
              debug(s"Child ${childHash.toHexString} matches prefix")
              val index = (childHash >>> level) & 0x1f
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
          createHashMap(level + 5)
        } else {
          val childArray = new Array[HashMap[K, V]](childCount)
          var size = 0

          @tailrec
          def addChildren(): Unit = {
            if (i < numElems) {
              val childHash = (hashAndIndex(i) >>> 32).toInt
              debug(s"Child hash is ${childHash.toHexString}")
              if (((childHash & prefixMask) ^ prefix) == 0) {
                val index = (childHash >>> level) & 0x1f
                debug(s"Child ${childHash.toHexString} index in bitmap is $index")
                val offset = Integer.bitCount(bitmap & ((1 << index) - 1))
                debug(s"Child ${childHash.toHexString} index in array is $offset")
                val childHashMap = createHashMap(level + 5)
                debug(s"Got child ${childHash.toHexString} HashMap: $childHashMap")
                childArray(offset) = childHashMap
                size += childHashMap.size
                addChildren()
              }
            }
          }

          addChildren()

          new HashTrieMap[K, V](bitmap, childArray, size)
        }

      } else {

        // At level >= 32 we handle items with the same hash.
        // We need to keep items with the same hash, but replace
        // items with the same key.

        val hash = (hashAndIndex(i) >>> 32).toInt

        def debug(msg: String): Unit = {
          println(s"${hash.toHexString}, $level!!, $i: $msg")
        }
        debug(s"Creating map at level >= 32")

        var acc: HashMap[K, V] = null

        @tailrec
        def joinElemsWithHash(): Unit = {
          if (i < numElems) {
            val childHashAndIndex = hashAndIndex(i)
            val childHash = (childHashAndIndex >>> 32).toInt
            debug(s"Scanning element with hash ${childHash.toHexString}")
            if (childHash == hash) {
              val elemsIndex = childHashAndIndex.toInt
              val kv = elems(elemsIndex)
              if (acc == null) {
                debug(s"Creating new HashMap1")
                acc = new HashMap1[K, V](kv._1, childHash, kv._2, kv)
              } else {
                debug(s"Adding to existing HashMap")
                acc = acc.updated0(kv._1, childHash, level, kv._2, kv, null)
              }
              i += 1
              joinElemsWithHash()
            }
          }
        }

        joinElemsWithHash()
        acc
      }
    }

    val r = createHashMap(0)
    println(s"Created HashMap at level 0 $r")
    r

  }

}