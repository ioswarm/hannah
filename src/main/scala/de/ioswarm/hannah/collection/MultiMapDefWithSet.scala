package de.ioswarm.hannah.collection

/**
  * Created by andreas on 16.04.17.
  */
private[collection] class MultiMapDefWithSet[K,V](map: Map[K, Set[V]]) extends MultiMapDef[K,V] with Map[K, Set[V]] {
  override def addBinding(key: K, value: V): MultiMapDefWithSet.this.type = ???

  override def +[V1 >: Set[V]](kv: (K, V1)): Map[K, V1] = map + kv

  override def get(key: K): Option[Set[V]] = map.get(key)

  override def iterator: Iterator[(K, Set[V])] = map.iterator

  override def -(key: K): Map[K, Set[V]] = map - key
}
