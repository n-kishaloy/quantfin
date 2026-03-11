package basic

import scala.math.*
import java.time.{LocalDate, Year}
import java.time.temporal.ChronoUnit.DAYS

object DoubleExtensions:
  extension (bas: Double)
    def pow(n: Double): Double = math.pow(bas, n)
    def sqrt: Double = math.sqrt(bas)
    def abs: Double = math.abs(bas)

import DoubleExtensions.*

def approx(x: Double, y: Double): Boolean =
  val mx = math.max(x.abs, y.abs); mx < 1e-8 || (x - y).abs / mx < 1e-6

def dateToFloat(dt: LocalDate): Double = dt.getYear.toDouble +
  (dt.getDayOfYear - 1).toDouble / dt.lengthOfYear.toDouble

def dateFromFloat(yr: Double): LocalDate =
  val y = yr.floor; val yp = y.toInt
  val daysOfYear = if Year.isLeap(yp) then 366.0 else 365.0
  val dys = ((yr - y) * daysOfYear).round
  if dys < daysOfYear then LocalDate.ofYearDay(yp, dys.toInt + 1)
  else LocalDate.of(yp + 1, 1, 1)

enum DayCount:
  case US30360, EU30360, ACTACT, ACT360, ACT365

import DayCount.*

def yearfrac(st: LocalDate, fin: LocalDate, dc: DayCount = US30360): Double =
  def datCf(y0: Int, m0: Int, d0: Int, y1: Int, m1: Int, d1: Int): Double =
    ((y1 - y0) * 360 + (m1 - m0) * 30 + (d1 - d0)).toDouble / 360.0

  def ym(dt: LocalDate): (Int, Int) = (dt.getYear, dt.getMonthValue)

  dc match
    case ACT360  => st.until(fin, DAYS).toDouble / 360.0
    case ACT365  => st.until(fin, DAYS).toDouble / 365.0
    case ACTACT  => dateToFloat(fin) - dateToFloat(st)
    case EU30360 =>
      val ((y0, m0), (y1, m1)) = (ym(st), ym(fin))
      val (d0, d1) = (st.getDayOfMonth, fin.getDayOfMonth)
      datCf(y0, m0, math.min(d0, 30), y1, m1, math.min(d1, 30))
    case US30360 =>
      val ((y0, m0), (y1, m1)) = (ym(st), ym(fin))
      var (d0, d1) = (st.getDayOfMonth, fin.getDayOfMonth)
      def lsFeb(y: Int, m: Int, d: Int): Boolean =
        m == 2 && d == (if Year.isLeap(y) then 29 else 28)
      if lsFeb(y0, m0, d0) then
        if lsFeb(y1, m1, d1) then d1 = 30
        d0 = 30
      if d1 == 31 && d0 >= 30 then d1 = 30
      if d0 == 31 then d0 = 30
      datCf(y0, m0, d0, y1, m1, d1)

def tmul(r: Double, n: Double): Double = (1.0 + r).pow(n)
def disFactor(r: Double, n: Double): Double = 1.0 / tmul(r, n)
def pv(r: Double, n: Double, f: Double): Double = f / tmul(r, n)
def fv(r: Double, n: Double, p: Double): Double = p * tmul(r, n)

def annuity(r: Double, n: Double, pt: Double, f: Double): Double =
  val rn = tmul(r, n); -pt / r * (1.0 - 1.0 / rn) - f / rn

def pmt(r: Double, n: Double, ann: Double, f: Double): Double =
  val rn = tmul(r, n); -(ann + f / rn) * r / (1.0 - 1.0 / rn)
