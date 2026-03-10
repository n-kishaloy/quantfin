// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import basic.*
import basic.DoubleExtensions.*
import java.time.LocalDate

class Basic extends munit.FunSuite:
  test("Yearfrac"):
    assertEquals(
      yearFromFloat(yearToFloat(LocalDate.of(2024, 1, 1))),
      LocalDate.of(2024, 1, 1)
    )
    assertEquals(
      yearFromFloat(yearToFloat(LocalDate.of(2024, 12, 31))),
      LocalDate.of(2024, 12, 31)
    )
    assertEquals(
      yearFromFloat(yearToFloat(LocalDate.of(2023, 8, 15))),
      LocalDate.of(2023, 8, 15)
    )
    println(yearFromFloat(yearToFloat(LocalDate.of(2024, 1, 1)) - 0.4 / 365.0))
    assertEquals(
      yearFromFloat(yearToFloat(LocalDate.of(2024, 2, 29))),
      LocalDate.of(2024, 2, 29)
    )
  // test("PV and FV"):
  //   assertEquals(pv(100.0, 0.05, 1.0), 95.23809523809523)
  //   assertEquals(fv(95.23809523809523, 0.05, 1.0), 100.0)

class MyDrt extends munit.FunSuite:
  test("statements"):
    assert(true)
