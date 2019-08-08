import cats.instances.string._
import cats.syntax.semigroup._

object example extends App {
  println("Hello " |+| "Cats!")

  // Define a very simple JSON AST
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  case object JsNull extends Json

  // The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          ))
    }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  Json.toJson(Person("Dave", "dave@example.com"))

  Person("Dave", "dave@example.com").toJson(personWriter)

  // Exercises
  sealed trait Printable[A] {
    def format(in: A): String
  }

  object PrintableInstances {
    implicit val stringPrinter: Printable[String] =
      new Printable[String] {
        def format(value: String): String =
          value
      }

    implicit val intPrinter: Printable[Int] =
      new Printable[Int] {
        def format(value: Int): String =
          value.toString
      }
  }

  object Printable {
    def format[A](input: A)(implicit p: Printable[A]): String =
      p.format(input)

    def print[A](input: A)(implicit p: Printable[A]): Unit =
      println(format(input))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p.format(value)

      def print(implicit p: Printable[A]): Unit =
        println(format(p))
    }
  }

}
