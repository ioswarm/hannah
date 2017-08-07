package de.ioswarm.hannah.collection

/**
  * Created by andreas on 16.04.17.
  */
private[collection] class MultiMapWithList[K, V](map: Map[K, List[V]]) {
  def addBinding(key: K, value: V): Map[K, List[V]]  = map + (key -> {value :: map.getOrElse(key, Nil)})
  def addBinding(t: (K, V)): Map[K, List[V]] = addBinding(t._1, t._2)
  def :+(key: K, value: V): Map[K, List[V]] = addBinding(key, value)
  def :+(t: (K, V)): Map[K, List[V]] = addBinding(t._1, t._2)

  def removeBinding(key: K, value: V): Map[K, List[V]] = ???

}
