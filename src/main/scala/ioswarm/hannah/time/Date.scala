package ioswarm.hannah.time

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import scala.language.implicitConversions

/**
  * Created by andreas on 29.06.16.
  */
trait DateFacade[T] {
  def age(): TemporalUnit

  def withDayOfMonth(dayOfMonth: Int): T
  def withDayOfYear(dayOfYear: Int): T
  def withMonth(month: Int): T
  def withYear(year: Int): T

  def dayOfMonth: Int
  def dayOfYear: Int
  def week: Int
  def month: Int
  def year: Int

  def firstDayOfMonth(): T
  def lastDayOfMonth(): T

  def weekDay: Int
}

sealed trait TemporalDateFacade extends Temporal[Date] with DateFacade[Date] {
  def date: LocalDate

  def plus(amount: Long) = Date(date.plusDays(amount))

  def plus(temporal: TemporalUnit) = Date(date.plusYears(temporal.years).plusMonths(temporal.months).plusDays(temporal.days))

  def minus(amount: Long) = Date(date.minusDays(amount))

  def minus(temporal: TemporalUnit) = Date(date.minusYears(temporal.years).minusMonths(temporal.months).minusDays(temporal.days))

  def format(pattern: String = "yyyy-MM-dd")(implicit locale: Locale = Locale.UK): String = {
    date.format(DateTimeFormatter.ofPattern(pattern, locale))
  }

  def until(datex: Date): TemporalUnit = TemporalUnit(date.until(datex.date))
  def age(): TemporalUnit = until(new Date())

  def withDayOfMonth(dayOfMonth: Int): Date = new Date(date.withDayOfMonth(dayOfMonth))
  def withDayOfYear(dayOfYear: Int): Date = new Date(date.withDayOfYear(dayOfYear))
  def withMonth(month: Int): Date = new Date(date.withMonth(month))
  def withYear(year: Int): Date = new Date(date.withYear(year))

  def dayOfMonth: Int = date.getDayOfMonth
  def dayOfYear: Int = date.getDayOfYear
  def week: Int = calendarWeek(year, month, dayOfMonth)
  def month: Int = date.getMonthValue
  def year: Int = date.getYear

  def firstDayOfMonth(): Date = withDayOfMonth(1)
  def lastDayOfMonth(): Date = withDayOfMonth(daysInMonth(year, month))

  def weekDay: Int = dayOfWeek(year, month, dayOfMonth)
}

object Date {
  def apply() = new Date()
  def apply(year: Int, month: Int, dayOfMonth: Int) = new Date(year, month, dayOfMonth)
  def apply(year: Int, dayOfYear: Int) = new Date(year, dayOfYear)
  def apply(text: String)(implicit datePattern: (String, Locale) = ("yyyy-MM-dd", Locale.UK)) = new Date(LocalDate.parse(text, DateTimeFormatter.ofPattern(datePattern._1).withLocale(datePattern._2)))

  implicit def parseFromString(text: String): Date = Date(text)
}

case class Date(date: LocalDate) extends TemporalDateFacade with Ordered[Date] {
  def this() = this(LocalDate.now())
  def this(year: Int, month: Int, dayOfMonth: Int) = this(LocalDate.of(year, month, dayOfMonth))
  def this(year: Int, dayOfYear: Int) = this(LocalDate.ofYearDay(year, dayOfYear))

  def compare(that: Date): Int = date.compareTo(that.date)

  override def toString = format()
}
