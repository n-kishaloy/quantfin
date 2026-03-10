package basic

import scala.math.*
import java.time.{LocalDate, Year}

object DoubleExtensions:
  extension (bas: Double)
    def pow(n: Double): Double = math.pow(bas, n)
    def sqrt: Double = math.sqrt(bas)
    def abs: Double = math.abs(bas)

import DoubleExtensions.*

def approx(x: Double, y: Double): Boolean =
  val mx = math.max(x.abs, y.abs); mx < 1e-8 || (x - y).abs / mx < 1e-6

def yearToFloat(dt: LocalDate): Double = dt.getYear.toDouble +
  (dt.getDayOfYear - 1).toDouble / dt.lengthOfYear.toDouble

def yearFromFloat(yr: Double): LocalDate =
  val y = yr.floor; val yp = y.toInt
  val daysOfYear = if Year.isLeap(yp) then 366.0 else 365.0
  val dys = ((yr - y) * daysOfYear).round
  if dys < daysOfYear then LocalDate.ofYearDay(yp, dys.toInt + 1)
  else LocalDate.of(yp + 1, 1, 1)

def tmul(r: Double, n: Double): Double = (1.0 + r).pow(n)
def disFactor(r: Double, n: Double): Double = 1.0 / tmul(r, n)
def pv(fv: Double, r: Double, n: Double): Double = fv / tmul(r, n)
def fv(pv: Double, r: Double, n: Double): Double = pv * tmul(r, n)

def annuity(r: Double, n: Double, pt: Double, fv: Double): Double =
  val rn = tmul(r, n); -pt / r * (1.0 - 1.0 / rn) - fv / rn

def pmt(r: Double, n: Double, ann: Double, fv: Double): Double =
  val rn = tmul(r, n); -(ann + fv / rn) * r / (1.0 - 1.0 / rn)
