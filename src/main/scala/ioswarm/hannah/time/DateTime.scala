package ioswarm.hannah.time

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import scala.language.implicitConversions

/**
  * Created by andreas on 03.07.16.
  */
sealed trait DateTimeFacade[T] extends DateFacade[T] with TimeFacade[T] {

}

sealed trait TemporalDateTimeFacade extends Temporal[DateTime] with DateTimeFacade[DateTime] {
  def dateTime: LocalDateTime

  def plus(amount: Long) = DateTime(dateTime.plusDays(amount))

  def plus(temporal: TemporalUnit) = DateTime(dateTime.plusYears(temporal.years).plusMonths(temporal.months).plusDays(temporal.days).plusNanos(temporal.millis*1000000))

  def minus(amount: Long) = DateTime(dateTime.minusDays(amount))

  def minus(temporal: TemporalUnit) = DateTime(dateTime.minusYears(temporal.years).minusMonths(temporal.months).minusDays(temporal.days).minusNanos(temporal.millis*1000000))

  def format(pattern: String = "yyyy-MM-dd hh:mm:ss")(implicit locale: Locale = Locale.UK): String = {
    dateTime.format(DateTimeFormatter.ofPattern(pattern, locale))
  }

  def until(datex: DateTime): TemporalUnit = toDate.until(datex.toDate)+toTime.until(datex.toTime)
  def age(): TemporalUnit = until(new DateTime())

  def withDayOfMonth(dayOfMonth: Int): DateTime = new DateTime(dateTime.withDayOfMonth(dayOfMonth))
  def withDayOfYear(dayOfYear: Int): DateTime = new DateTime(dateTime.withDayOfYear(dayOfYear))
  def withMonth(month: Int): DateTime = new DateTime(dateTime.withMonth(month))
  def withYear(year: Int): DateTime = new DateTime(dateTime.withYear(year))
  def withHour(hour: Int): DateTime = new DateTime(dateTime.withHour(hour))
  def withMinute(minute: Int): DateTime = new DateTime(dateTime.withMinute(minute))
  def withSecond(second: Int): DateTime = new DateTime(dateTime.withSecond(second))
  def withMillis(millis: Int): DateTime = new DateTime(dateTime.withNano(millis*1000000))

  def dayOfMonth: Int = dateTime.getDayOfMonth
  def dayOfYear: Int = dateTime.getDayOfYear
  def week: Int = calendarWeek(year, month, dayOfMonth)
  def month: Int = dateTime.getMonthValue
  def year: Int = dateTime.getYear
  def hour: Int = dateTime.getHour
  def minute: Int = dateTime.getMinute
  def second: Int = dateTime.getSecond
  def millis: Int = dateTime.getNano/1000000

  def firstDayOfMonth(): DateTime = withDayOfMonth(1)
  def lastDayOfMonth(): DateTime = withDayOfMonth(daysInMonth(year, month))

  def toDate: Date = Date(year, month, dayOfMonth)
  def toTime: Time = Time(hour, minute, second, millis)

  def weekDay: Int = dayOfWeek(year, month, dayOfMonth)
}

object DateTime {
  def apply(): DateTime = new DateTime()
  def apply(year: Int, month: Int, dayOfMonth: Int): DateTime = new DateTime(year, month, dayOfMonth, 0, 0, 0, 0)
  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int, millis: Int): DateTime = new DateTime(year, month, dayOfMonth, hour, minute, second, millis)
  def apply(date: Date): DateTime = apply(year=date.year,month=date.month,dayOfMonth = date.dayOfMonth)
  def apply(time: Time): DateTime = apply().withHour(time.hour).withMinute(time.minute).withSecond(time.second).withMillis(time.millis)
  def apply(text: String)(implicit dateTimePattern: (String, Locale) = ("yyyy-MM-dd hh:mm:ss", Locale.UK)) = new DateTime(LocalDateTime.parse(text, DateTimeFormatter.ofPattern(dateTimePattern._1).withLocale(dateTimePattern._2)))

  implicit def fromString(text: String): DateTime = DateTime(text)
  implicit def fromDate(date: Date): DateTime = DateTime(date)
  implicit def fromTime(time: Time): DateTime = DateTime(time)
}

case class DateTime(dateTime: LocalDateTime) extends TemporalDateTimeFacade with Ordered[DateTime] {
  def this() = this(LocalDateTime.now())
  def this(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int, millis: Int) = this(LocalDateTime.of(year, month, dayOfMonth, hour, minute, second, millis*1000000))

  def compare(that: DateTime): Int = dateTime.compareTo(that.dateTime)

  override def toString = format()

}
