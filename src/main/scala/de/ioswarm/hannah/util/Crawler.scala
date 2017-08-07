package de.ioswarm.hannah.util

/**
  * Created by andreas on 09.07.16.
  */
trait Crawler[T] {

  def +(n: Int): T = next(n)
  def ++(): T = next()
  def next(n: Int = 1): T

  def -(n: Int): T = prev(n)
  def --(): T = prev()
  def prev(n: Int = 1): T
}
