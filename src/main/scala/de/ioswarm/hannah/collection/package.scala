package de.ioswarm.hannah

/**
  * Created by andreas on 16.04.17.
  */
package object collection {

  type MultiMap[K,V] = Map[K, Set[V]]
  type ListMultiMap[K,V] = Map[K, List[V]]
  type SetMultiMap[K,V] = Map[K, Set[V]]

  implicit def ListMapToListMultiMap[K,V](map: Map[K, List[V]]): MultiMapWithList[K,V] = new MultiMapWithList(map)

  implicit def SetMapToSetMultiMap[K,V](map: Map[K, Set[V]]): MultiMapDefWithSet[K,V] = new MultiMapDefWithSet(map)

}

