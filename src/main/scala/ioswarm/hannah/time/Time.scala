package ioswarm.hannah.time

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.Locale

import scala.language.implicitConversions

/**
  * Created by andreas on 02.07.16.
  */
trait TimeFacade[T] {
  def withHour(hour: Int): T
  def withMinute(minute: Int): T
  def withSecond(second: Int): T
  def withMillis(millis: Int): T

  def hour: Int
  def minute: Int
  def second: Int
  def millis: Int
}

sealed trait TemporalTimeFacade extends Temporal[Time] with TimeFacade[Time] {
  def time: LocalTime

  def plus(amount: Long) = Time(time.plus(amount, ChronoUnit.MILLIS))

  def plus(temporal: TemporalUnit) = Time(time.plus(temporal.millis, ChronoUnit.MILLIS))

  def minus(amount: Long) = Time(time.minus(amount, ChronoUnit.MILLIS))

  def minus(temporal: TemporalUnit) = Time(time.minus(temporal.millis, ChronoUnit.MILLIS))

  def until(timex: Time): TemporalUnit = TemporalUnit(seconds=time.until(timex.time, ChronoUnit.SECONDS))

  def format(pattern: String = "HH:mm:ss")(implicit locale: Locale = Locale.UK): String = {
    time.format(DateTimeFormatter.ofPattern(pattern, locale))
  }

  def withHour(hour: Int): Time = new Time(time.withHour(hour))
  def withMinute(minute: Int): Time = new Time(time.withMinute(minute))
  def withSecond(second: Int): Time = new Time(time.withSecond(second))
  def withMillis(millis: Int): Time = new Time(time.withNano(millis*1000000))

  def hour: Int = time.getHour
  def minute: Int = time.getMinute
  def second: Int = time.getSecond
  def millis: Int = time.getNano/1000000

}

object Time {
  def apply() = new Time()
  def apply(hour: Int, minute: Int, second: Int, millis: Int) = new Time(hour, minute, second, millis)
  def apply(hour: Int, minute: Int, second: Int) = new Time(hour, minute, second)
  def apply(hour: Int, minute: Int) = new Time(hour, minute)
  def apply(text: String)(implicit timePattern: (String, Locale) = ("hh:mm:ss", Locale.UK)) = new Time(LocalTime.parse(text, DateTimeFormatter.ofPattern(timePattern._1).withLocale(timePattern._2)))

  implicit def fromString(text: String): Time = Time(text)
}

case class Time(time: LocalTime) extends TemporalTimeFacade with Ordered[Time] {
  def this() = this(LocalTime.now())
  def this(hour: Int, minute: Int, second: Int, millis: Int) = this(LocalTime.of(hour, minute, second, millis*1000000))
  def this(hour: Int, minute: Int, second: Int) = this(hour, minute, second,0)
  def this(hour: Int, minute: Int) = this(hour, minute, 0)

  def compare(that: Time): Int = time.compareTo(that.time)

  override def toString = format()
}
