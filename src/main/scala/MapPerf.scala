package scala.collection.immutable

import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.immutable.HashMap.{HashMap1, HashMapCollision1, HashTrieMap, Int }

object MapPerf {

  def main(args: Array[String]): Unit = {
    val tests = Seq[Seq[(String,Int)]](
      Seq("a" -> 1),
      Seq("a" -> 1, "b" -> 2),
      Seq("1" -> 1, "6" -> 6),
      Seq("1" -> 1, "2" -> 2, "6" -> 6),
      Seq("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4),
      Seq("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "d" -> 44),
      (0 until 7).map(i => (i.toString -> i)),
      (0 until 1000).map(i => (i.toString -> i))
    )

    for (test <- tests) {
//      println()
//      println(s"------ Testing $test ------")
//      println()
      //val slow = Map.apply(test: _*)
      val slow: HashMap[String, Int] = ((new HashMap) ++ (test)).asInstanceOf[HashMap[String, Int]]
      val fast = apply(test: _*)
      if (slow != fast) {
        println(s"Input: $test")
        printHashMap(slow)
        //printHashMap(fast)
      }
//      assert(slow.size == fast.size, s"fast.size -> ${fast.size}")
//      for ((k,_) <- test) {
//        assert(slow.get(k) == fast.get(k), s"fast.get($k) -> ${fast.get(k)}")
//      }
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
        val x = (Integer.reverse(hash).toLong << 32) | i
//        println(s"Storing $key, ${hash.toHexString}, $i as ${x.toHexString}")
        arr(i) = x
        i += 1
      }
      arr
    }

    Arrays.sort(hashAndIndex)

//    println(s"Sorted hashAndIndex array: ${hashAndIndex.map(_.toHexString).mkString(", ")}")

    var i = 0
    def currHash(): Int = Integer.reverse((hashAndIndex(i) >>> 32).toInt)

    def createHashMap(level: Int): HashMap[K,V] = {

      def debug(msg: => String): Unit = {
//        for (i <- 0 until (level / 5)) {
//          print("- ")
//        }
//        println(s"$level, $i: $msg")
      }

      if (level < 32) {

        debug(s"Called createHashMap($level) at index $i")
        val start = i
        var bitmap = 0
        val prefixMask = (1 << level) - 1
        debug(s"Prefix mask is ${prefixMask.toHexString}")

        val firstHash = currHash()
        debug(s"First child hash is ${firstHash.toHexString}")
        val prefix = firstHash & prefixMask
        debug(s"Prefix is ${prefix.toHexString}")

        // Get bitmap for children

//        val childLevel = level + 5
        //val childMask = ((1 << 5) - 1) << childLevel

        var onlyOneHashSeen = true

        @tailrec
        def populateBitmap(): Unit = {
          if (i < numElems) {
            val childHash = currHash()
            debug(s"Scanning child ${childHash.toHexString}")
            if (((childHash & prefixMask) ^ prefix) == 0) {
              debug(s"Child ${childHash.toHexString} matches prefix")
              if (onlyOneHashSeen && childHash != firstHash) {
                debug("More than one hash seen for this prefix")
                onlyOneHashSeen = false
              }
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

        if (onlyOneHashSeen) {
          debug(s"Only single hash seen for this prefix")
          createHashMap(32) // HACK to force single-hash HashMap
        } else {
          debug(s"More than one hash for prefix: creating HashTrieMap")
          val arraySize = Integer.bitCount(bitmap)
          val childArray = new Array[HashMap[K, V]](arraySize)
          var size = 0

          @tailrec
          def addChildren(): Unit = {
            if (i < numElems) {
              val childHash = currHash()
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

        val hash = currHash()
        debug(s"Creating map at level >= 32")
        debug(s"Storing element(s) for hash $hash")

        var acc: HashMap[K, V] = null

        @tailrec
        def joinElemsWithHash(): Unit = {
          if (i < numElems) {
            val childHash = currHash()
            debug(s"Scanning element with hash ${childHash.toHexString}")
            if (childHash == hash) {
              val elemsIndex = hashAndIndex(i).toInt // last 32 bits
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
    //println(s"Created HashMap at level 0 $r")
    r

  }
//
//  def apply2[K,V](elems: (K, V)*): HashMap[K,V] = {
//
//    def computeHash(key: K) = improve(key.hashCode)
//
//    val numElems = elems.length
//
//    def loop(i: Int, hm: HashMap[K,V]): HashMap[K,V] = {
//      if (i < numElems) {
//        val kv: (K, V) = elems(i)
//        loop(
//          i + 1,
//          updateMut(hm, kv._1, computeHash(kv._1), 0, kv._2, kv, remaining = numElems - i - 1)
//        )
//      } else {
//        hm
//      }
//    }

    /*
    level
    filled

    P(uses this trie) = 1/(2^level)
    P(trie need to grow | uses this trie) = (32 - filled)/32


     */

//    def updateMut(hm: HashMap[K,V], key: K, hash: Int, level: Int, value: V, kv: (K, V), remaining: Int): HashMap[K, V] = {
//      hm match {
//        // Add an element to one other element
//        case hm1: HashMap1[K, V] =>
//          //println(s"HashMap1(${hm1.hash.toHexString}, ${hm1.key} -> ${hm1.value})")
//          if (hash == hm1.hash) {
//            if (key == hm1.key) {
//              // Same hash and same key
//              if (value.asInstanceOf[AnyRef] eq hm1.value.asInstanceOf[AnyRef]) hm1 else {
//                new HashMap1(key, hash, value, kv)
//              }
//            } else {
//              // Same hash but different key - a hash collision
//              new HashMapCollision1[K,V](hash, ListMap.empty[K,V].updated(hm1.key, hm1.value).updated(key, value))
//            }
//          } else {
//            // Different hash
//            val that = new HashMap1[K, V](key, hash, value, kv)
//            makeHashTrieMap(hm1.hash, hm1, hash, that, level, size = 2)
//          }
//        // Add an element to a trie structure
//        case htm: HashTrieMap[K, V] =>
//          val index = (hash >>> level) & 0x1f
//          val mask = (1 << index)
//          val offset = Integer.bitCount(htm.bitmap & (mask - 1))
//          if ((htm.bitmap & mask) == 0) {
//            // The index is already free
//            val currLength = Integer.bitCount(htm.bitmap)
//            if (currLength < htm.elems.length) {
//              // We can squeeze this into the existing array!
//              System.arraycopy(htm.elems, offset, htm.elems, offset + 1, currLength - offset)
//            } else {
//              // We need to grow the array to fit this in
//              val remainingAtLevel = remaining >>> Math.min(0, level - 1)
//              /*
//
//               */
//
//
//              val growth = remainingAtLevel
//              val newLength = Math.min(currLength + 3, 32) // TODO: Better growth algorithm
//            }
//
//            val elemsNew = new Array[HashMap[K,V]](elems.length + 1)
//            Array.copy(elems, 0, elemsNew, 0, offset)
//            elemsNew(offset) = new HashMap1(key, hash, value, kv)
//            Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
//            new HashTrieMap(htm.bitmap | mask, elemsNew, htm.size + 1)
//          } else {
//            val sub = elems(offset)
//            val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
//            if(subNew eq sub) this else {
//              val elemsNew = new Array[HashMap[A, B1]](elems.length)
//              Array.copy(elems, 0, elemsNew, 0, elems.length)
//              elemsNew(offset) = subNew
//              new HashTrieMap(htm.bitmap, elemsNew, htm.size + (subNew.size - sub.size))
//            }
//          }
//
//
//          //println(s"HashTrieMap(bitmap = ${htm.bitmap.toHexString}, size = ${htm.size}):")
////          for (elem <- htm.elems) {
////            printHashMap(elem, indent = indent + "- ")
////          }
//        // Add an element to a collision structure
//        case hmc1: HashMapCollision1[_, _] =>
//          //println(s"HashMapCollision1(${hmc1.hash.toHexString}, ${hmc1.kvs})")
//        case _: HashMap[K, V] if hm.isEmpty =>
//          //println("HashMap")
//      }
//    }

//    def makeHashTrieMap(hash0:Int, elem0:HashMap[K,V], hash1:Int, elem1:HashMap[K,V], level:Int, size:Int) : HashTrieMap[K,V] = {
//      val index0 = (hash0 >>> level) & 0x1f
//      val index1 = (hash1 >>> level) & 0x1f
//      if(index0 != index1) {
//        val bitmap = (1 << index0) | (1 << index1)
//        val elems = new Array[HashMap[A,B]](2)
//        if(index0 < index1) {
//          elems(0) = elem0
//          elems(1) = elem1
//        } else {
//          elems(0) = elem1
//          elems(1) = elem0
//        }
//        new HashTrieMap[A, B](bitmap, elems, size)
//      } else {
//        val elems = new Array[HashMap[A,B]](1)
//        val bitmap = (1 << index0)
//        elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size)
//        new HashTrieMap[A, B](bitmap, elems, size)
//      }
//    }

//    loop(0, HashMap.empty)
//
//  }

  private val countTable: Array[Array[Int]] = Array(
    Array(1, 1, 3, 2, 4, 3, 6, 4, 7, 5, 9, 6, 11, 7, 13, 8, 14, 9, 16, 10, 18, 11, 20, 12, 23, 13, 25, 14, 28, 15, 30, 16, 33, 17, 36, 18, 40, 19, 43, 20, 47, 21, 52, 22, 56, 23, 62, 24, 68, 25, 76, 26, 85, 27, 96, 28, 110, 29, 132, 30, 168, 31, 256, 32),
    // Existing count = 1
    Array(2, 2, 3, 3, 5, 4, 6, 5, 8, 6, 10, 7, 12, 8, 13, 9, 15, 10, 17, 11, 19, 12, 22, 13, 24, 14, 27, 15, 29, 16, 32, 17, 35, 18, 38, 19, 42, 20, 46, 21, 50, 22, 55, 23, 61, 24, 67, 25, 75, 26, 84, 27, 95, 28, 110, 29, 131, 30, 166, 31, 255, 32),
    // Existing count = 2
    Array(2, 3, 4, 4, 5, 5, 7, 6, 9, 7, 10, 8, 12, 9, 14, 10, 16, 11, 18, 12, 21, 13, 23, 14, 25, 15, 28, 16, 31, 17, 34, 18, 37, 19, 41, 20, 45, 21, 49, 22, 55, 23, 60, 24, 66, 25, 74, 26, 83, 27, 94, 28, 108, 29, 130, 30, 165, 31, 255, 32),
    // Existing count = 3
    Array(2, 4, 4, 5, 6, 6, 8, 7, 9, 8, 11, 9, 13, 10, 15, 11, 17, 12, 19, 13, 22, 14, 24, 15, 27, 16, 30, 17, 33, 18, 36, 19, 40, 20, 44, 21, 48, 22, 54, 23, 59, 24, 66, 25, 73, 26, 81, 27, 93, 28, 107, 29, 129, 30, 164, 31, 252, 32),
    // Existing count = 4
    Array(3, 5, 5, 6, 6, 7, 8, 8, 10, 9, 12, 10, 14, 11, 16, 12, 18, 13, 21, 14, 23, 15, 26, 16, 29, 17, 32, 18, 35, 19, 39, 20, 43, 21, 47, 22, 52, 23, 58, 24, 64, 25, 72, 26, 80, 27, 92, 28, 106, 29, 128, 30, 164, 31, 252, 32),
    // Existing count = 5
    Array(3, 6, 5, 7, 7, 8, 9, 9, 11, 10, 13, 11, 15, 12, 17, 13, 19, 14, 22, 15, 25, 16, 28, 17, 31, 18, 34, 19, 38, 20, 42, 21, 46, 22, 51, 23, 56, 24, 63, 25, 70, 26, 79, 27, 91, 28, 106, 29, 126, 30, 162, 31, 249, 32),
    // Existing count = 6
    Array(3, 7, 5, 8, 7, 9, 9, 10, 11, 11, 14, 12, 16, 13, 18, 14, 21, 15, 24, 16, 26, 17, 30, 18, 33, 19, 36, 20, 40, 21, 45, 22, 50, 23, 55, 24, 62, 25, 69, 26, 78, 27, 90, 28, 104, 29, 126, 30, 162, 31, 250, 32),
    // Existing count = 7
    Array(4, 8, 6, 9, 8, 10, 10, 11, 12, 12, 14, 13, 17, 14, 19, 15, 22, 16, 25, 17, 28, 18, 32, 19, 35, 20, 39, 21, 44, 22, 49, 23, 54, 24, 61, 25, 68, 26, 77, 27, 88, 28, 103, 29, 124, 30, 160, 31, 248, 32),
    // Existing count = 8
    Array(4, 9, 6, 10, 8, 11, 11, 12, 13, 13, 15, 14, 18, 15, 21, 16, 24, 17, 27, 18, 30, 19, 34, 20, 38, 21, 42, 22, 47, 23, 53, 24, 59, 25, 67, 26, 76, 27, 87, 28, 102, 29, 123, 30, 158, 31, 248, 32),
    // Existing count = 9
    Array(4, 10, 7, 11, 9, 12, 11, 13, 14, 14, 17, 15, 19, 16, 22, 17, 25, 18, 29, 19, 32, 20, 36, 21, 41, 22, 46, 23, 51, 24, 58, 25, 65, 26, 74, 27, 85, 28, 100, 29, 122, 30, 157, 31, 246, 32),
    // Existing count = 10
    Array(5, 11, 7, 12, 10, 13, 12, 14, 15, 15, 18, 16, 20, 17, 24, 18, 27, 19, 31, 20, 35, 21, 39, 22, 44, 23, 50, 24, 56, 25, 64, 26, 73, 27, 84, 28, 99, 29, 120, 30, 156, 31, 244, 32),
    // Existing count = 11
    Array(5, 12, 8, 13, 10, 14, 13, 15, 16, 16, 19, 17, 22, 18, 26, 19, 29, 20, 33, 21, 38, 22, 43, 23, 48, 24, 55, 25, 62, 26, 72, 27, 83, 28, 97, 29, 119, 30, 154, 31, 242, 32),
    // Existing count = 12
    Array(5, 13, 8, 14, 11, 15, 14, 16, 17, 17, 20, 18, 24, 19, 27, 20, 32, 21, 36, 22, 41, 23, 47, 24, 53, 25, 61, 26, 70, 27, 81, 28, 95, 29, 117, 30, 153, 31, 243, 32),
    // Existing count = 13
    Array(6, 14, 9, 15, 12, 16, 15, 17, 18, 18, 22, 19, 26, 20, 30, 21, 34, 22, 39, 23, 45, 24, 51, 25, 59, 26, 68, 27, 79, 28, 94, 29, 116, 30, 151, 31, 240, 32),
    // Existing count = 14
    Array(6, 15, 9, 16, 13, 17, 16, 18, 20, 19, 24, 20, 28, 21, 32, 22, 37, 23, 43, 24, 49, 25, 57, 26, 66, 27, 77, 28, 92, 29, 114, 30, 150, 31, 238, 32),
    // Existing count = 15
    Array(7, 16, 10, 17, 14, 18, 18, 19, 21, 20, 26, 21, 30, 22, 35, 23, 41, 24, 48, 25, 56, 26, 64, 27, 76, 28, 90, 29, 112, 30, 147, 31, 236, 32),
    // Existing count = 16
    Array(7, 17, 11, 18, 15, 19, 19, 20, 23, 21, 28, 22, 33, 23, 39, 24, 45, 25, 53, 26, 62, 27, 74, 28, 89, 29, 110, 30, 146, 31, 234, 32),
    // Existing count = 17
    Array(8, 18, 12, 19, 16, 20, 21, 21, 26, 22, 31, 23, 37, 24, 43, 25, 51, 26, 60, 27, 71, 28, 87, 29, 108, 30, 143, 31, 231, 32),
    // Existing count = 18
    Array(9, 19, 13, 20, 18, 21, 23, 22, 28, 23, 34, 24, 41, 25, 48, 26, 58, 27, 69, 28, 84, 29, 106, 30, 142, 31, 230, 32),
    // Existing count = 19
    Array(10, 20, 15, 21, 20, 22, 25, 23, 31, 24, 38, 25, 46, 26, 55, 27, 67, 28, 82, 29, 103, 30, 139, 31, 228, 32),
    // Existing count = 20
    Array(10, 21, 16, 22, 22, 23, 28, 24, 35, 25, 43, 26, 53, 27, 64, 28, 79, 29, 100, 30, 136, 31, 225, 32),
    // Existing count = 21
    Array(12, 22, 18, 23, 25, 24, 32, 25, 39, 26, 49, 27, 61, 28, 76, 29, 98, 30, 134, 31, 223, 32),
    // Existing count = 22
    Array(13, 23, 20, 24, 28, 25, 36, 26, 46, 27, 58, 28, 73, 29, 95, 30, 132, 31, 220, 32),
    // Existing count = 23
    Array(15, 24, 23, 25, 32, 26, 42, 27, 53, 28, 68, 29, 91, 30, 127, 31, 216, 32),
    // Existing count = 24
    Array(17, 25, 26, 26, 37, 27, 49, 28, 64, 29, 87, 30, 124, 31, 213, 32),
    // Existing count = 25
    Array(19, 26, 31, 27, 44, 28, 60, 29, 83, 30, 118, 31, 206, 32),
    // Existing count = 26
    Array(23, 27, 37, 28, 54, 29, 76, 30, 113, 31, 203, 32),
    // Existing count = 27
    Array(28, 28, 46, 29, 69, 30, 107, 31, 198, 32),
    // Existing count = 28
    Array(36, 29, 61, 30, 99, 31, 190, 32),
    // Existing count = 29
    Array(48, 30, 89, 31, 181, 32),
    // Existing count = 30
    Array(73, 31, 168, 32),
    // Existing count = 31
    Array(147, 32)
  )

  private def predictCount(existingCount: Int, updatesRemaining: Int): Int = {
    val row = countTable(existingCount)

    @tailrec
    def scan(i: Int): Int = {
      val minUpdates = row(i)
      if (updatesRemaining <= minUpdates) {
        row(i + 1)
      } else {
        scan(i + 2)
      }
    }
    scan(0)
  }

//  class Builder[A,B] {
//    var sizeHint = -1
//
//    def +=()
//    def sizeHint(hint: Int): Unit = {
//
//    }
//  }

}