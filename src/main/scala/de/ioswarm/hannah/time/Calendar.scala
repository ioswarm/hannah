package de.ioswarm.hannah.time

import de.ioswarm.hannah.util.Crawler
import scala.language.implicitConversions

/**
  * Created by andreas on 03.07.16.
  */
trait Calendar {

}

object Year {
  def apply(dateFacade: DateFacade[_]): Year = new Year(dateFacade)
  def apply(): Year = new Year()

  implicit def parseFromInt(value: Int): Year = new Year(value)
  implicit def parseFromDateFacade(date: DateFacade[_]): Year = new Year(date)
}

case class Year(year: Int) extends Crawler[Year] with Ordered[Year] {
  def this(dateFacade: DateFacade[_]) = this(dateFacade.year)
  def this() = this(new Date())

  def next(n: Int = 1): Year = new Year(year+n)
  def prev(n: Int = 1): Year = new Year(year-n)

  def compare(that: Year): Int = year.compareTo(that.year)

  def leap(): Boolean = isLeapYear(year)

  def firstOf: Day = Day(firstOfYear(year))
  def lastOf: Day = Day(lastOfYear(year))

  def months(): Seq[Month] = for (x <- 1 to 12) yield Month(this, x)
  def weeks(): Seq[Week] = for (x <- 1 to calendarWeek(year,12,31)) yield Week(this, x)
  def days(): Seq[Day] = for (x <- 1 to daysInYear(year)) yield Day(this, x)
}


object Month {
  def apply(dateFacade: DateFacade[_]): Month = new Month(dateFacade)
  def apply(month: Int): Month = new Month(month)
  def apply(): Month = new Month()

  implicit def parseFromDateFacade(date: DateFacade[_]): Month = new Month(date)
}

case class Month(year: Year, month: Int) extends Crawler[Month] with Ordered[Month] {
  def this(dateFacade: DateFacade[_]) = this(Year(dateFacade.year), dateFacade.month)
  def this(month: Int) = this(Year(new Date()), month)
  def this() = this(new Date())

  def next(n: Int = 1): Month = new Month(firstOf.date + n.months)
  def prev(n: Int = 1): Month = new Month(firstOf.date - n.months)

  def compare(that: Month): Int = {
    if (year == that.year) month.compareTo(that.month) else year.compareTo(that.year)
  }

  def firstOf: Day = Day(firstOfMonth(year.year,month))
  def lastOf: Day = Day(lastOfMonth(year.year, month))

  def weeks(): Seq[Week] = for (x <- weekOf(firstOf.date) to weekOf(lastOf.date)) yield Week(year, x)
  def days(): Seq[Day] = for (x <- 0 until daysInMonth(year.year, month)) yield firstOf + x

}


object Week {
  def apply(dateFacade: DateFacade[_]): Week = new Week(dateFacade)
  def apply(week: Int): Week = new Week(week)
  def apply(): Week = new Week()

  implicit def parseFromDateFacade(date: DateFacade[_]): Week = new Week(date)
}

case class Week(year: Year, week: Int) extends Crawler[Week] with Ordered[Week] {
  def this(dateFacade: DateFacade[_]) = this(Year(dateFacade.year), dateFacade.week)
  def this(week: Int) = this(Year(new Date), week)
  def this() = this(new Date())

  def next(n: Int = 1): Week = new Week(firstOf.date + n.weeks)
  def prev(n: Int = 1): Week = new Week(firstOf.date - n.weeks)

  def compare(that: Week): Int = {
    if (year == that.year) week.compareTo(that.week) else year.compareTo(that.year)
  }

  def firstOf: Day = Day(firstOfWeek(year.year, week))
  def lastOf: Day = Day(lastOfWeek(year.year, week))

  def days(): Seq[Day] = for (x <- 0 to 6) yield firstOf + x
}


object Day {
  def apply(year: Year, dayOfYear: Int) = new Day(year, dayOfYear)
  def apply() = new Date()

  implicit def parseFromDateFacade(date: DateFacade[_]): Day = new Day(date)
}

case class Day(date: Date) extends Crawler[Day] with Ordered[Day] {
  def this(dateFacade: DateFacade[_]) = this(new Date(dateFacade.year, dateFacade.month, dateFacade.dayOfMonth))
  def this(year: Year, dayOfYear: Int) = this(new Date(year.year, dayOfYear))
  def this() = this(new Date())

  def next(n: Int = 1): Day = Day(date + n.days)
  def prev(n: Int = 1): Day = Day(date - n.days)

  def compare(that: Day): Int = date.compare(that.date)
}

