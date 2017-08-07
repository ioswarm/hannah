package de.ioswarm.hannah

import scala.language.implicitConversions

/**
  * Created by andreas on 02.07.16.
  */
package object time {

  def leapModifier(year: Int): Int = if ((year % 4 == 0) && ((year < 1582) || (!(year % 100 == 0)) || (year % 400 == 0))) 1 else 0
  def isLeapYear(year: Int): Boolean = leapModifier(year) == 1

  def monthDays(year: Int): Array[Int] = Array(31,28+leapModifier(year),31,30,31,30,31,31,30,31,30,31)

  def daysInYear(year: Int): Int = monthDays(year).reduceLeft(_ + _)

  def daysInMonth(year: Int, month: Int): Int = {
    require(month > 0 && month <= 12, "Month must between 1 and 12.")
    monthDays(year)(month-1)
  }

  def dayOfYear(year: Int, month: Int, dayOfMonth: Int): Int = {
    val mds = monthDays(year)
    require(month > 0 && month <= 12, "Month must between 1 and 12.")
    require(dayOfMonth<=mds(month-1))
    if (month == 1) dayOfMonth else mds.take(month-1).reduceLeft(_ + _)+dayOfMonth
  }

  def doomDay(year: Int): Int = {
    val cdd = Array(5,4,2,0)
    val cc: Int = year/100
    val yy = year%100
    val ccDoomDay = cdd(cc%4)
    val ret = if (yy == 0) ccDoomDay else if (yy%12 == 0) (ccDoomDay+yy/12-1)%7 else (ccDoomDay+yy/12+yy%12+((yy-1)%12)/4)%7
    if (cc%4 == 0 && yy != 0) (ret+1)%7 else ret
  }

  def dayOfWeek(year: Int, month: Int, dayOfMonth: Int): Int = (doomDay(year)+dayOfYear(year, month, dayOfMonth)%7)%7

  def calendarWeek(year: Int, month: Int, dayOfMonth: Int): Int = {
    val days = dayOfYear(year, month, dayOfMonth) + (if (doomDay(year) > 3) -1*(7-doomDay(year)) else doomDay(year))
    val week = if (days < 0) {
      if (doomDay(year) == 4 || doomDay(year-1) == 3) 53 else 52
    } else days/7+1
    if (days > 360 && week > 52) {
      if (doomDay(year) == 3) 53
      else if (doomDay(year+1) == 4) 53
      else 1
    } else week
  }

  def easter(year: Int): Int = {
    val M: Int = 15 + (if (year > 1582) (3*year/100+3)/4 - (8*year/100+13)/25 else 0)
    val S: Int = if (year > 1582) 2-(3*year/100+3)/4 else 0
    val A = year%19
    val D = (19*A+M)%30
    val R: Int = (D+A/11)/29
    val OG = 21+D-R
    OG + (7 - (OG-(7 - (year+year/4+S)%7))%7)
  }

  implicit def stringToDateType(any: String): StringToDateType = new StringToDateType(any)

  implicit def intToTemporal(any: Int): IntToTemporal = new IntToTemporal(any)
  implicit def longToTemporal(any: Long): LongToTemporal = new LongToTemporal(any)

  def easterDate(year: Int): Date = Date(year,3,1)+easter(year).days-1.days
  def firstOfYear(year: Int): Date = Date(year, 1, 1)
  def lastOfYear(year: Int): Date = Date(year, 12, 1)
  def firstOfMonth(year: Int, month: Int): Date = Date(year,month,1)
  def lastOfMonth(year: Int, month: Int): Date = Date(year,month,daysInMonth(year,month))
  def firstOfWeek(year: Int, week: Int): Date = {
    val dDay = Date(year,1,1)
    dDay + (if (dDay.week == week) 0 else week+(if (dDay.week == 1) -1 else 0)).weeks - dDay.weekDay.days
  }
  def lastOfWeek(year: Int, week: Int): Date = firstOfWeek(year, week)+6.days

  def weekOf(dateFacade: DateFacade[_]): Int = dateFacade.week
}

package time {

  private[time] trait ToDateType[T] {
    def d(): Date

    def t(): Time

    def dt(): DateTime
  }

  private[time] class StringToDateType(any: String) extends ToDateType[String] {
    def d(): Date = Date(any)

    def t(): Time = Time(any)

    def dt(): DateTime = DateTime(any)
  }

  private[time] trait ToTemporal[T] {
    def years(): TemporalUnit

    def months(): TemporalUnit

    def weeks(): TemporalUnit

    def days(): TemporalUnit

    def hours(): TemporalUnit

    def minutes(): TemporalUnit

    def seconds(): TemporalUnit

    def millis(): TemporalUnit
  }

  private[time] class IntToTemporal(any: Int) extends ToTemporal[Int] {
    def years(): TemporalUnit = TemporalUnit(years = any)

    def months(): TemporalUnit = TemporalUnit(months = any)

    def weeks(): TemporalUnit = TemporalUnit(weeks = any)

    def days(): TemporalUnit = TemporalUnit(days = any)

    def hours(): TemporalUnit = TemporalUnit(hours = any)

    def minutes(): TemporalUnit = TemporalUnit(minutes = any)

    def seconds(): TemporalUnit = TemporalUnit(seconds = any)

    def millis(): TemporalUnit = TemporalUnit(millis = any)
  }

  private[time] class LongToTemporal(any: Long) extends ToTemporal[Long] {
    def years(): TemporalUnit = TemporalUnit(years = any.toInt)

    def months(): TemporalUnit = TemporalUnit(months = any.toInt)

    def weeks(): TemporalUnit = TemporalUnit(weeks = any.toInt)

    def days(): TemporalUnit = TemporalUnit(days = any)

    def hours(): TemporalUnit = TemporalUnit(hours = any)

    def minutes(): TemporalUnit = TemporalUnit(minutes = any)

    def seconds(): TemporalUnit = TemporalUnit(seconds = any)

    def millis(): TemporalUnit = TemporalUnit(millis = any)
  }

}