package de.ioswarm.hannah.time

import java.time.{Duration, Period}
import java.util.Locale

/**
  * Created by andreas on 30.06.16.
  */
trait Temporal[T <: AnyRef] {
  def +(amount: Long): T = plus(amount)
  def plus(amount: Long): T

  def +(temporal: TemporalUnit): T = plus(temporal)
  def plus(temporal: TemporalUnit): T

  def -(amount: Long): T = minus(amount)
  def minus(amount: Long): T

  def -(temporal: TemporalUnit): T = minus(temporal)
  def minus(temporal: TemporalUnit): T

  def until(to: T): TemporalUnit

  def format(pattern: String)(implicit locale: Locale = Locale.UK): String
}


sealed trait TemporalUnitFacade {
  def years: Int
  def months: Int
  def days: Long
  def millis: Long

  def +(temporal: TemporalUnit): TemporalUnit = new TemporalUnit(years+temporal.years,months+temporal.months, temporal.days+days, millis+temporal.millis)

  def -(temporal: TemporalUnit): TemporalUnit = new TemporalUnit(years-temporal.years,months-temporal.months, temporal.days-days, millis-temporal.millis)


}

object TemporalUnit {
  def apply(years: Int = 0, months: Int = 0, weeks: Int = 0, days: Long = 0, hours: Long = 0, minutes: Long = 0, seconds: Long = 0, millis: Long = 0): TemporalUnit = new TemporalUnit(years, months, weeks, days, hours, minutes, seconds, millis)
  def apply(period: Period): TemporalUnit = TemporalUnit(years = period.getYears, months = period.getMonths, days = period.getDays)
  def apply(duration: Duration): TemporalUnit = TemporalUnit(seconds=duration.getSeconds)
}

case class TemporalUnit(years: Int, months: Int, days: Long, millis: Long) extends TemporalUnitFacade {
  def this(years: Int, months: Int, weeks: Int, days: Long, hours: Long, minutes: Long, seconds: Long, millis: Long) = this(years+months/12,months%12,weeks*7+days,((hours*60+minutes)*60+seconds)*1000+millis)

}


