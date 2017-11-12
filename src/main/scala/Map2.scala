package scala.collection

import scala.collection.generic.{CanBuildFrom, MapFactory}
import scala.collection.immutable.MapBuilder2

object Map2 extends MapFactory[scala.collection.Map] {
  def empty[K, V]: immutable.Map[K, V] = immutable.Map.empty

  override def newBuilder[A, B]: mutable.Builder[(A, B), Map[A, B]] = new MapBuilder2[A, B]

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] = new MapCanBuildFrom[K, V]

  /** An abstract shell used by { mutable, immutable }.Map but not by collection.Map
   *  because of variance issues.
   */
  abstract class WithDefault[K, +V](underlying: Map[K, V], d: K => V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
    override def size               = underlying.size
    def get(key: K)                 = underlying.get(key) // removed in 2.9: orElse Some(default(key))
    def iterator                    = underlying.iterator
    override def default(key: K): V = d(key)
  }

}
