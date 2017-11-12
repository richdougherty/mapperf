/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import scala.annotation.tailrec
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.generic._
import scala.collection.immutable.HashMap.Int
import scala.collection.immutable.Map.{Map1, Map2, Map3, Map4}


final class MapBuilder2[A, B]
    extends mutable.ReusableBuilder[(A, B), scala.collection.immutable.Map[A, B]] {

  private var size: Int = 0
  private var sizeHint: Int = -1
  private var kv1: (A,B) = _
  private var kv2: (A,B) = _
  private var kv3: (A,B) = _
  private var kv4: (A,B) = _
  private var hash1: Int = _
  private var hash2: Int = _
  private var hash3: Int = _
  private var hash4: Int = _

  private var hashMap: HashMap2[A,B] = _

  override def sizeHint(hint: Int): Unit = {
    sizeHint = hint
  }

  override def +=(x: (A, B)): this.type = {
    val h = x.hashCode

    def tryUpdateMap1(): Boolean = {
      if (h == hash1 && x == kv1) {
        hash1 = h
        kv1 = x
        true
      } else false
    }
    def tryUpdateMap2(): Boolean = {
      if (tryUpdateMap1()) true else if (h == hash2 && x == kv2) {
        hash2 = h
        kv2 = x
        true
      } else false
    }
    def tryUpdateMap3(): Boolean = {
      if (tryUpdateMap2()) true else if (h == hash3 && x == kv3) {
        hash3 = h
        kv3 = x
        true
      } else false
    }
    def tryUpdateMap4(): Boolean = {
      if (tryUpdateMap3()) true else if (h == hash4 && x == kv4) {
        hash4 = h
        kv4 = x
        true
      } else false
    }
    def updateHashMap(hashCode: Int, keyValue: (A, B)): Unit = {
      val key = keyValue._1
      val value = keyValue._2
      val improvedHash = HashMap2.improveHashCode(hashCode) // TODO: Move computeHash to companion?
      val remainingUpdates = Math.min(0, sizeHint - size)
      hashMap = hashMap.updated0(key, improvedHash, level = 0, value, keyValue, mutUpdates = remainingUpdates, merger = null)
      size += 1
    }

    size match {
      case 0 =>
        size = 1
        hash1 = h // Optimization: could lazily calculate this if needed
        kv1 = x
      case 1 =>
        assert(kv1 != null)
        if (!tryUpdateMap1()) {
          size = 2
          hash2 = h
          kv2 = x
        }
      case 2 =>
        assert(kv1 != null && kv2 != null)
        if (!tryUpdateMap2()) {
          size = 3
          hash3 = h
          kv3 = x
        }
      case 3 =>
        assert(kv1 != null && kv2 != null && kv3 != null)
        if (!tryUpdateMap3()) {
          size = 4
          hash4 = h
          kv4 = x
        }
      case 4 =>
        assert(kv1 != null && kv2 != null && kv3 != null && kv4 != null, s"$kv1, $kv2, $kv3, $kv4")
        if (!tryUpdateMap4()) {
          // Convert to a HashMap
          size = 0 // Set to 0 since appends will increment this
          hashMap = HashMap2.empty
          // Move existing values to hashMap
          // Note: for performance don't clear hash1-4
          updateHashMap(hash1, kv1)
          updateHashMap(hash2, kv2)
          updateHashMap(hash3, kv3)
          updateHashMap(hash4, kv4)
          kv1 = null
          kv2 = null
          kv3 = null
          kv4 = null
          // Add new value to HashMap
          updateHashMap(h, x)
        }
      case _ =>
        if (hashMap == null) {
          // User has called the `result` method; the hashMap has been cleared to prevent further mutable updates
          throw new IllegalStateException("Clear builder before using again")
        }

    }
    this
  }



  def clear() {
    size = 0
    sizeHint = -1
    kv1 = null
    kv2 = null
    kv3 = null
    kv4 = null
    // Note: for performance don't clear hash1-4
    hashMap = null
  }
  def result: scala.collection.immutable.Map[A, B] = {
    size match {
      case 0 => Map.empty
      case 1 => new Map1(kv1._1, kv1._2)
      case 2 => new Map2(kv1._1, kv1._2, kv2._1, kv2._2)
      case 3 => new Map3(kv1._1, kv1._2, kv2._1, kv2._2, kv3._1, kv3._2)
      case 4 => new Map4(kv1._1, kv1._2, kv2._1, kv2._2, kv3._1, kv3._2, kv4._1, kv4._2)
      case _ =>
        val result = hashMap
        hashMap = null // Clear to prevent further mutable updates; undefined state permitted by method contract
        result
    }

  }
}


//abstract class ImmutableMapFactory2[CC[A, +B] <: immutable.Map[A, B] with immutable.MapLike[A, B, CC[A, B]]] extends MapFactory[CC]


object Map2 extends ImmutableMapFactory[Map] {

  override def newBuilder[A, B]: mutable.Builder[(A, B), Map[A, B]] = new MapBuilder2[A, B]

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] = new MapCanBuildFrom[K, V]

  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  override def apply[A, B](elems: (A, B)*): Map[A, B] = {  // XXX: New code XXXX
    elems.length match {
      case 0 => empty
      case 1 => new Map1(elems(0)._1, elems(0)._2)
      case len =>
        val builder = newBuilder[A, B]
        builder.sizeHint(len)
        builder ++= elems
        builder.result()
    }
  }

  private object EmptyMap extends AbstractMap[Any, Nothing] with Map[Any, Nothing] with Serializable {
    override def size: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def + [V1](kv: (Any, V1)): Map[Any, V1] = updated(kv._1, kv._2)
    def - (key: Any): Map[Any, Nothing] = this
  }

}

/** This class implements immutable maps using a hash trie.
 *
 *  '''Note:''' The builder of this hash map may return specialized representations for small maps.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash_tries "Scala's Collection Library overview"]]
 *  section on `Hash Tries` for more information.
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(2L)
sealed class HashMap2[A, +B] extends AbstractMap[A, B]
                        with Map[A, B]
                        with MapLike[A, B, HashMap2[A, B]]
                        with Serializable
{
  import HashMap2.{bufferSize, nullToEmpty}

  override def size: Int = 0

  override def empty = HashMap2.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) => U): Unit = ()

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override final def contains(key: A): Boolean =
    contains0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap2[A, B1] =
    updated0(key, computeHash(key), 0, value, null, 0, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap2[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, 0, null)

//  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap2[A, B1] =
//    this + elem1 + elem2 ++ elems

  def - (key: A): HashMap2[A, B] =
    removed0(key, computeHash(key), 0)

  override def tail: HashMap2[A, B] = this - head._1

  override def filter(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap2[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap2[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap2[A, B @uV]], offset0: Int): HashMap2[A, B] = null

  protected def elemHashCode(key: A) = key.##

  // TODO: Make this private/move to companion object?
  protected final def improve(hcode: Int) = HashMap2.improveHashCode(hcode)

  // TODO: Make this a method on the companion object?
  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  import HashMap2.{MergeFunction, Merger, liftMerger}

  private[collection] def get0(key: A, hash: Int, level: Int): Option[B] = None
  protected def contains0(key: A, hash: Int, level: Int): Boolean = false

  /** TODO TODO: comment formatting
   * @param key The new key to add.
   * @param hash The hash of the key.
   * @param level The level of the current object. Equal to the bits of the trie prefix above this object. The root
   *              has a level of 0. Due to the branching factor of 32, each level below that has a prefix with 5 more
   *              bits. The second level of the trie has a level of 5.
   * @param value The new value to add.
   * @param kv A pair holding the new key and value.
   * @param mutUpdates If -1, indicates this is an update on an immutable HashMap. If 0 or greater, indicates this an
   *                   update on a HashMap that is being made by a builder, so the HashMap can be treated as temporarily
   *                   mutable. The value is a hint as to the number of updates that are expected after this this
   *                   operation. For example, 0 indicates that the number of future updates is 0 or unknown, but 5
   *                   indicates that 5 more updates are expected. This allows the HashMap to optimize its internal
   *                   storage to hold the expected future number of updates, e.g. by preallocating more space in an
   *                   array.
   * @param merger If present, this object indicates which value to use if there is a key collision. If null then the
   *               new value is used.
   */
  private[collection] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), mutUpdates: Int, merger: Merger[A, B1]): HashMap2[A, B1] = {
    // TODO: This default implementation is only appropriate for the EmptyHashMap subclass. This method should be abstract.
    new HashMap2.HashMap1(key, hash, value, kv)
  }

  protected def removed0(key: A, hash: Int, level: Int): HashMap2[A, B] = this

  protected def writeReplace(): AnyRef = new HashMap2.SerializationProxy(this)

  def split: Seq[HashMap2[A, B]] = Seq(this)

  /** Creates a new map which is the merge of this and the argument hash map.
   *
   *  Uses the specified collision resolution function if two keys are the same.
   *  The collision resolution function will always take the first argument from
   *  `this` hash map and the second from `that`.
   *
   *  The `merged` method is on average more performant than doing a traversal and reconstructing a
   *  new immutable hash map from scratch, or `++`.
   *
   *  @tparam B1      the value type of the other hash map
   *  @param that     the other hash map
   *  @param mergef   the merge function or null if the first key-value pair is to be picked
   */
  def merged[B1 >: B](that: HashMap2[A, B1])(mergef: MergeFunction[A, B1]): HashMap2[A, B1] = merge0(that, 0, liftMerger(mergef))

  protected def merge0[B1 >: B](that: HashMap2[A, B1], level: Int, merger: Merger[A, B1]): HashMap2[A, B1] = that
}

/** $factoryInfo
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 */
object HashMap2 extends ImmutableMapFactory[HashMap2] with BitOperations.Int {

  private[collection] def improveHashCode(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }


  private[collection] abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def invert: Merger[A, B]
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private[this] val defaultMerger : Merger[Any, Any] = liftMerger0((a,b) => a)

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val invert: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def invert: Merger[A1, B1] = self
    }
  }

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap2[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: HashMap2[A, B] = EmptyHashMap.asInstanceOf[HashMap2[A, B]]

  private object EmptyHashMap extends HashMap2[Any, Nothing] { 
    override def head: (Any, Nothing) = throw new NoSuchElementException("Empty Map")
    override def tail: HashMap2[Any, Nothing] = throw new NoSuchElementException("Empty Map")
  }

  private val countTable: Array[Array[Int]] = Array(
    Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 18, 20, 21, 23, 25, 27, 30, 32, 35, 38, 41, 45, 50, 55, 61, 69, 81, 102),
    Array(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 17, 19, 20, 22, 24, 26, 29, 31, 34, 37, 41, 44, 49, 54, 61, 69, 80, 102),
    Array(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 13, 14, 16, 18, 19, 21, 23, 25, 28, 30, 33, 36, 39, 43, 48, 53, 60, 68, 80, 101),
    Array(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 18, 20, 22, 24, 27, 29, 32, 35, 38, 42, 47, 52, 58, 67, 78, 99),
    Array(1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 14, 15, 17, 19, 21, 23, 26, 28, 31, 34, 37, 41, 46, 51, 57, 66, 77, 97),
    Array(1, 2, 3, 4, 5, 7, 8, 9, 11, 13, 14, 16, 18, 20, 22, 24, 27, 30, 33, 36, 40, 44, 50, 56, 64, 76, 97),
    Array(1, 2, 3, 4, 6, 7, 8, 10, 11, 13, 15, 17, 19, 21, 23, 26, 28, 32, 35, 39, 43, 49, 55, 63, 75, 96),
    Array(1, 2, 3, 4, 6, 7, 9, 10, 12, 14, 16, 17, 20, 22, 25, 27, 30, 34, 38, 42, 47, 54, 62, 73, 95),
    Array(1, 2, 3, 4, 6, 7, 9, 11, 12, 14, 16, 18, 21, 23, 26, 29, 33, 36, 41, 46, 52, 61, 72, 93),
    Array(1, 2, 3, 5, 6, 8, 9, 11, 13, 15, 17, 19, 22, 25, 28, 31, 35, 40, 45, 51, 59, 71, 92),
    Array(1, 2, 3, 5, 6, 8, 10, 12, 14, 16, 18, 21, 23, 27, 30, 34, 38, 43, 50, 58, 70, 91),
    Array(1, 2, 4, 5, 7, 8, 10, 12, 14, 17, 19, 22, 25, 28, 32, 37, 42, 48, 57, 68, 89),
    Array(1, 2, 4, 5, 7, 9, 11, 13, 15, 18, 20, 23, 27, 31, 35, 40, 47, 55, 67, 87),
    Array(1, 2, 4, 6, 7, 9, 11, 14, 16, 19, 22, 25, 29, 33, 39, 45, 54, 65, 86),
    Array(1, 2, 4, 6, 8, 10, 12, 15, 17, 20, 24, 27, 32, 37, 44, 52, 63, 84),
    Array(1, 2, 4, 6, 8, 10, 13, 16, 19, 22, 26, 30, 35, 42, 50, 62, 83),
    Array(1, 3, 4, 6, 9, 11, 14, 17, 20, 24, 28, 34, 40, 48, 60, 80),
    Array(1, 3, 5, 7, 9, 12, 15, 18, 22, 26, 32, 38, 46, 58, 78),
    Array(1, 3, 5, 7, 10, 13, 16, 20, 24, 30, 36, 44, 56, 76),
    Array(1, 3, 5, 8, 11, 14, 18, 22, 27, 34, 42, 54, 75),
    Array(1, 3, 6, 9, 12, 16, 20, 25, 31, 40, 51, 72),
    Array(1, 3, 6, 9, 13, 17, 22, 29, 37, 49, 69),
    Array(1, 4, 7, 10, 15, 20, 26, 34, 46, 67),
    Array(1, 4, 8, 12, 17, 23, 31, 43, 64),
    Array(2, 5, 9, 13, 20, 28, 39, 60),
    Array(2, 5, 10, 16, 24, 35, 56),
    Array(2, 6, 12, 20, 31, 52),
    Array(2, 7, 15, 26, 46),
    Array(3, 9, 20, 40),
    Array(3, 13, 32),
    Array(5, 23),
    Array(10)
  )

  private[immutable] def predictCount(existingCount: Int, updatesRemaining: Int): Int = {
    if (existingCount == 32) 32 else {
      val row: Array[Int] = countTable(existingCount)

      @tailrec
      def scan(column: Int): Int = {
        if (column >= row.length) 32 else {
          val minUpdates: Int = row(column)
          if (updatesRemaining >= minUpdates) {
            existingCount + column + 1
          } else {
            scan(column + 1)
          }
        }
      }

      scan(0)
    }
  }

  /**
   * Utility method to create a HashTrieMap from two leaf HashMaps (HashMap1 or HashMapCollision1) with non-colliding hash code).
   *
   * @param hash0 The hash of the first map's elements.
   * @param elem0 A HashMap containing one or more items with the same hash.
   * @param hash1 The hash of the second map's element(s).
   * @param elem1 A HashMap containing one or more items with the same hash.
   * @param level The level of the trie in bits. The resulting trie will be at this level. Intermediate tries will be
   *              created if necessary in order to store the child maps with different prefixes.
   * @param size The size of the resulting trie.
   * @param mutUpdates TODO TODO TODO
   */
  private def makeHashTrieMap[A, B](hash0:Int, elem0:HashMap2[A, B], hash1:Int, elem1:HashMap2[A, B], level:Int, size:Int, mutUpdates: Int) : HashTrieMap[A, B] = {
    val mutUpdatesAtLevel = mutUpdates >> level // Roughly how many updates we expect at this level

    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val predictedElems = HashMap2.predictCount(2, mutUpdatesAtLevel)
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashMap2[A,B]](predictedElems)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieMap[A, B](bitmap, elems, size)
    } else {
      val predictedElems = HashMap2.predictCount(1, mutUpdatesAtLevel)
      val elems = new Array[HashMap2[A,B]](predictedElems)
      val bitmap = (1 << index0)
      elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size, mutUpdates)
      new HashTrieMap[A, B](bitmap, elems, size)
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashMap1[A,+B](private[collection] val key: A, private[collection] val hash: Int, private[collection] val value: (B @uV), private[collection] var kv: (A,B @uV)) extends HashMap2[A,B] {
    override def size = 1

    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: A) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && key == this.key
    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), mutUpdates: Int, merger: Merger[A, B1]): HashMap2[A, B1] =
      if (hash == this.hash && key == this.key ) {
        if (merger eq null) {
          // Keys match, but no merger function is present, so use the new value.
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this // Optimization if the new value is the same
          else new HashMap1(key, hash, value, kv)
        } else {
          // Keys match, use the provided merger function to work out which value to use.
          val nkv = merger(this.ensurePair, if(kv != null) kv else (key, value))
          new HashMap1(nkv._1, hash, nkv._2, nkv) // TODO: Should we really update if the merge doesn't require a change?
        }
      } else {
        if (hash != this.hash) {
          // Keys are different and have different hashes. However, they share a common hash prefix of `level` bits.
          // Create a new trie for their common hash prefix. The trie will need enough levels to distinguish the hashes'
          // from each other.
          val that = new HashMap1[A, B1](key, hash, value, kv)
          makeHashTrieMap[A,B1](this.hash, this, hash, that, level, 2, mutUpdates)
        } else {
          // Keys are not equal, however their 32-bit hashes have collided. This is rare, but not impossible. Construct
          // a special collision object to handle this situation.
          new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap2[A, B] =
      if (hash == this.hash && key == this.key) HashMap2.empty[A,B] else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap2[A, B @uV]], offset0: Int): HashMap2[A, B] =
      if (negate ^ p(ensurePair)) this else null

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    // this method may be called multiple times in a multithreaded environment, but that's ok
    private[HashMap2] def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }
    protected override def merge0[B1 >: B](that: HashMap2[A, B1], level: Int, merger: Merger[A, B1]): HashMap2[A, B1] = {
      that.updated0(key, hash, level, value, kv, mutUpdates = -1, merger.invert)
    }
  }

  private[collection] class HashMapCollision1[A, +B](private[collection] val hash: Int, val kvs: ListMap[A, B @uV])
          extends HashMap2[A, B @uV] {
    // assert(kvs.size > 1)

    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && kvs.contains(key)

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), mutUpdates: Int, merger: Merger[A, B1]): HashMap2[A, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new HashMapCollision1(hash, kvs.updated(key, value))
        else new HashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        val that = new HashMap1(key, hash, value, kv)
        makeHashTrieMap(this.hash, this, hash, that, level, size + 1, mutUpdates)
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap2[A, B] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        kvs1.size match {
          case 0 =>
            HashMap2.empty[A,B]
          case 1 =>
            val kv = kvs1.head
            new HashMap1(kv._1,hash,kv._2,kv)
          case x if x == kvs.size =>
            this
          case _ =>
            new HashMapCollision1(hash, kvs1)
        }
      } else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap2[A, B @uV]], offset0: Int): HashMap2[A, B] = {
      val kvs1 = if(negate) kvs.filterNot(p) else kvs.filter(p)
      kvs1.size match {
        case 0 =>
          null
        case 1 =>
          val kv@(k,v) = kvs1.head
          new HashMap1(k, hash, v, kv)
        case x if x == kvs.size =>
          this
        case _ =>
          new HashMapCollision1(hash, kvs1)
      }
    }

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override def split: Seq[HashMap2[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uV]) = new HashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }
    protected override def merge0[B1 >: B](that: HashMap2[A, B1], level: Int, merger: Merger[A, B1]): HashMap2[A, B1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, mutUpdates = -1, merger)
      m
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashTrieMap[A, +B](
    private[collection] val bitmap: Int,
    private[collection] val elems: Array[HashMap2[A, B @uV]],
    private[collection] var size0: Int
  ) extends HashMap2[A, B @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Option[B] = {
      // Note: this code is duplicated with `contains0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).get0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).get0(key, hash, level + 5)
        } else {
          None
        }
      }
    }

    override protected def contains0(key: A, hash: Int, level: Int): Boolean = {
      // Note: this code is duplicated from `get0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).contains0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).contains0(key, hash, level + 5)
        } else {
          false
        }
      }
    }

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), mutUpdates: Int, merger: Merger[A, B1]): HashMap2[A, B1] = {
      // The index of the hash at this level of the trie, given as a number from 0-31.
      val index = (hash >>> level) & 0x1f
      // The bit in the `bitmap` field that corresponds to the index of the hash.
      val mask = (1 << index)
      // The offset of this object in the elems field. Found by counting the bits below the `mask` in the `bitmap`.
      val offset = Integer.bitCount(bitmap & (mask - 1))
      // Test the mask against the bitmap to see if we can store the new key
      if ((bitmap & mask) != 0) {
        // There is already a HashMap at that index. Pass the update operation down
        // to that HashMap.
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, mutUpdates, merger)
        if(subNew eq sub) {
          // The child HashMap is still the same. This can happen if we're doing a mutable update during building
          // or if there was a merge operation that avoided an update.
          this
        } else {
          // The child HashMap at that index has changed.
          val sizeNew = size + (subNew.size - sub.size)
          if (mutUpdates == -1) {
            // This HashMap is immutable; return a modified copy
            val elemsNew = new Array[HashMap2[A,B1]](elems.length)
            Array.copy(elems, 0, elemsNew, 0, elems.length)
            elemsNew(offset) = subNew
            new HashTrieMap(bitmap, elemsNew, sizeNew)
          } else {
            // This HashMap is mutable; modify its size and array directly
            size0 = sizeNew
            val elemsNew = elems.asInstanceOf[Array[HashMap2[A,B1]]]
            elemsNew(offset) = subNew
            this
          }
        }
      } else {
        // There is a free space at `index`.
        if (mutUpdates == -1) {
          // Updating an immutable HashMap. Create a new HashMap1 and store it at that location.
          val elemsNew = new Array[HashMap2[A,B1]](elems.length + 1)
          Array.copy(elems, 0, elemsNew, 0, offset)
          elemsNew(offset) = new HashMap1(key, hash, value, kv)
          Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
          new HashTrieMap(bitmap | mask, elemsNew, size + 1)
        } else {
          // Updating a mutable HashMap. Update the existing array if there's space,
          // otherwise allocate a new array.
          val elemsUsed = Integer.bitCount(bitmap)
          val elemsNeeded = elemsUsed + 1
          if (elemsNeeded < elems.length) {
            // There is space to update the existing array. Move the exising elements
            // to make space, then add the new element.
            //System.err.println(s"${elems.length}, $elemsUsed, $offset")
            Array.copy(elems, offset, elems, offset + 1, elemsUsed - offset)
            val elemsNew = elems.asInstanceOf[Array[HashMap2[A,B1]]]
            elemsNew(offset) = new HashMap1(key, hash, value, kv)
            size0 = size + 1
            this
          } else {
            // We need to allocate a new array.
            val mutUpdatesAtLevel = mutUpdates >> level
            val newElemsSize = HashMap2.predictCount(elemsNeeded, mutUpdatesAtLevel)
            val elemsNew = new Array[HashMap2[A,B1]](newElemsSize)
            Array.copy(elems, 0, elemsNew, 0, offset)
            elemsNew(offset) = new HashMap1(key, hash, value, kv)
            Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
            new HashTrieMap(bitmap | mask, elemsNew, size + 1)
          }
        }
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashMap2[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap2[A,B]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieMap[_,_]])
              elemsNew(0)
            else
              new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            HashMap2.empty[A,B]
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieMap[_,_]]) {
          subNew
        } else {
          val elemsNew = new Array[HashMap2[A,B]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap2[A, B @uV]], offset0: Int): HashMap2[A, B] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size0) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieMap[A, B]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieMap
        val length = offset - offset0
        val elems1 = new Array[HashMap2[A, B]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieMap(bitmap1, elems1, rs)
      }
    }

    override def iterator: Iterator[(A, B)] = new TrieIterator[(A, B)](elems.asInstanceOf[Array[Iterable[(A, B)]]]) {
      final override def getElem(cc: AnyRef): (A, B) = cc.asInstanceOf[HashMap1[A, B]].ensurePair
    }

    override def foreach[U](f: ((A, B)) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
        i += 1
        if ((b & 1) != 0) left -= 1
        b = b >>> 1
      }
      i
    }

    override def split: Seq[HashMap2[A, B]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))

        val (e1, e2) = elems.splitAt(splitpoint)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected override def merge0[B1 >: B](that: HashMap2[A, B1], level: Int, merger: Merger[A, B1]): HashMap2[A, B1] = that match {
      case hm: HashMap1[_, _] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, mutUpdates = -1, merger)
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
        val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[HashMap2[A, B1]](subcount)

        // run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))

          // collision
          if (thislsb == thatlsb) {
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            // condition below is due to 2 things:
            // 1) no unsigned int compare on JVM
            // 2) 0 (no lsb) should always be greater in comparison
            if (unsignedCompare(thislsb - 1, thatlsb - 1)) {
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            }
            else {
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[A, B1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: HashMapCollision1[_, _] => that.merge0(this, level, merger.invert)
      case hm: HashMap2[_, _] => this
      case _ => throw new IllegalStateException("section supposed to be unreachable.")
    }
  }

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty map is represented as null for performance reasons. This method converts
   * null to the empty map for use in public methods
   */
  @inline private def nullToEmpty[A, B](m: HashMap2[A, B]): HashMap2[A, B] = if (m eq null) empty[A, B] else m

  /**
   * Utility method to keep a subset of all bits in a given bitmap
   *
   * Example
   *    bitmap (binary): 00000001000000010000000100000001
   *    keep (binary):                               1010
   *    result (binary): 00000001000000000000000100000000
   *
   * @param bitmap the bitmap
   * @param keep a bitmask containing which bits to keep
   * @return the original bitmap with all bits where keep is not 1 set to 0
   */
  private def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  @SerialVersionUID(2L)
  private class SerializationProxy[A,B](@transient private var orig: HashMap2[A, B]) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for ((k,v) <- orig) {
        out.writeObject(k)
        out.writeObject(v)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val key = in.readObject().asInstanceOf[A]
        val value = in.readObject().asInstanceOf[B]
        orig = orig.updated(key, value)
      }
    }

    private def readResolve(): AnyRef = orig
  }
}
