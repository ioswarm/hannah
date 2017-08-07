package de.ioswarm.hannah.collection

/**
  * Created by andreas on 16.04.17.
  */
trait MultiMapDef[K, V] {
  def addBinding(key: K, value: V): this.type
  def addBinding(t: (K,V)): this.type = addBinding(t._1, t._2)
  def :+(key: K, value: V): this.type = addBinding(key, value)
  def :+(t: (K,V)): this.type  = addBinding(t)
}
