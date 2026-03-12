// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import basic.*
import basic.DoubleExtensions.*
import java.time.LocalDate

import basic.DayCount.*

class Basic extends munit.FunSuite:
  test("Yearfrac"):
    assertEquals(
      dateFromFloat(dateToFloat(LocalDate.of(2024, 1, 1))),
      LocalDate.of(2024, 1, 1)
    )
    assertEquals(
      dateFromFloat(dateToFloat(LocalDate.of(2024, 12, 31))),
      LocalDate.of(2024, 12, 31)
    )
    assertEquals(
      dateFromFloat(dateToFloat(LocalDate.of(2023, 8, 15))),
      LocalDate.of(2023, 8, 15)
    )
    // println(dateFromFloat(dateToFloat(LocalDate.of(2024, 1, 1)) - 0.4 / 365.0))
    assertEquals(
      dateFromFloat(dateToFloat(LocalDate.of(2024, 2, 29))),
      LocalDate.of(2024, 2, 29)
    )
    assertEquals(
      List(
        ((2018, 2, 5), (2023, 5, 14)),
        ((2020, 2, 29), (2024, 2, 28)),
        ((2015, 8, 30), (2010, 3, 31)),
        ((2016, 2, 28), (2016, 10, 30)),
        ((2014, 1, 31), (2014, 8, 31)),
        ((2014, 2, 28), (2014, 9, 30)),
        ((2016, 2, 29), (2016, 6, 15))
      )
        .map((dt0, dt1) =>
          (
            LocalDate.of(dt0._1, dt0._2, dt0._3),
            LocalDate.of(dt1._1, dt1._2, dt1._3)
          )
        )
        .map((dt0, dt1) =>
          (
            yearfrac(dt0, dt1, US30360),
            yearfrac(dt0, dt1, ACTACT),
            yearfrac(dt0, dt1, ACT360),
            yearfrac(dt0, dt1, ACT365),
            yearfrac(dt0, dt1, EU30360)
          )
        ),
      List(
        (5.275, 5.26849315068489, 5.344444444444444, 5.271232876712329, 5.275),
        (3.9944444444444445, 3.997267759562874, 4.055555555555555, 4.0,
          3.9972222222222222),
        (-5.416666666666667, -5.416438356164235, -5.4944444444444445,
          -5.419178082191781, -5.416666666666667),
        (0.6722222222222223, 0.6693989071038686, 0.6805555555555556,
          0.6712328767123288, 0.6722222222222223),
        (0.5833333333333334, 0.5808219178081799, 0.5888888888888889,
          0.5808219178082191, 0.5833333333333334),
        (0.5833333333333334, 0.5863013698631221, 0.5944444444444444,
          0.5863013698630137, 0.5888888888888889),
        (0.2916666666666667, 0.2923497267761377, 0.2972222222222222,
          0.29315068493150687, 0.29444444444444445)
      )
    )

  test("PV and FV"):
    assertEquals(pv(0.09, 5.0, 10_000_000.0), 6_499_313.862983453)
    assertEquals(fv(0.09, 5.0, 6_499_313.862983453), 10_000_000.0)
    assertEquals(
      pmt(0.08 / 12.0, 30.0 * 12.0, -1000.0, 50.0),
      7.304096785187425
    )
    assertEquals(
      annuity(0.08 / 12.0, 30.0 * 12.0, 7.304096785187425, 50.0),
      -1000.0
    )
    assertEquals(
      xpv(0.08, LocalDate.of(2020, 2, 29), LocalDate.of(2024, 2, 28), 5.638),
      4.145870545134078
    )
    assert(
      approx(
        xfv(0.08, LocalDate.of(2020, 2, 29), LocalDate.of(2024, 2, 28), 5.638),
        fv(0.08, 3.9944444444444400000, 5.638)
      )
    )
  test("NPV and IRR"):
    assertEquals(
      npv(
        0.08,
        List(0.25, 6.25, 3.5, 4.5, 1.25),
        -0.45,
        List(-6.25, 1.2, 1.25, 3.6, 2.5)
      ),
      0.36962283798505946
    )
    assertEquals(
      xnpv(
        0.08,
        List(
          LocalDate.of(2012, 2, 25),
          LocalDate.of(2012, 6, 28),
          LocalDate.of(2013, 2, 15),
          LocalDate.of(2014, 9, 18),
          LocalDate.of(2015, 2, 20)
        ),
        LocalDate.of(2012, 1, 10),
        List(-15.0, 5.0, 25.0, -10.0, 50.0)
      ),
      44.165773653310936
    )
    assertEquals(newtRaph(x => (x - 3.0) * (x - 4.0), 2.0), Some(3.0))
    assertEquals(newtRaph(x => (x - 4.0) * (x - 4.0) + 5.0, 2.0), None)
    assertEquals(
      irr(
        List(0.125, 0.29760274, 0.49760274, 0.55239726, 0.812671233),
        List(-10.25, -2.5, 3.5, 9.5, 1.25)
      ),
      Some(0.31813386475204)
    )
    assertEquals(
      irr(
        List(0.125, 0.29760274, 0.49760274, 0.55239726, 0.812671233),
        List(10.25, 2.5, 3.5, 9.5, 1.25)
      ),
      None
    )
    assertEquals(
      xirr(
        List(
          LocalDate.of(2012, 2, 25),
          LocalDate.of(2012, 6, 28),
          LocalDate.of(2013, 2, 15),
          LocalDate.of(2014, 9, 18),
          LocalDate.of(2015, 2, 20)
        ),
        List(-115.0, 5.0, 25.0, -10.0, 200.0)
      ),
      Some(0.2784553815926181)
    )

class MyDrt extends munit.FunSuite:
  test("statements"):
    assert(true)
