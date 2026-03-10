import basic.*
import basic.DoubleExtensions.*
import scala.math.*

@main def hello(): Unit =
  println("Hello world!")
  println(2.0.pow(3) + tmul(0.05, 10))

def msg = "I was compiled by Scala 3. :)"
