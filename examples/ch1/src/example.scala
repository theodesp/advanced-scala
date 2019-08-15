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

  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
  val intAsString: String =
    showInt.show(123)
  val stringAsString: String =
    showString.show("abc")

  import cats.syntax.show._ // for show

  val shownInt = 123.show
  // shownInt: String = 123

  val shownString = "abc".show
  // shownString: String = abc

  import java.util.Date

  implicit val dateShow: Show[Date] =
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  println(Cat("Garfield", 38, "ginger and black").show)

  //List(1, 2, 3).map(Option(_)).filter(item => item == 1)

  import cats.Eq
  val eqInt = Eq[Int]

  eqInt.eqv(123, 123)
  eqInt.eqv(123, 234)

  import cats.syntax.eq._
  123 =!= 234

  import cats.instances.int._ // for Eq
  import cats.instances.option._ // for Eq

  (Some(1): Option[Int]) === (None: Option[Int])

  import cats.syntax.option._

  1.some === none[Int]
  1.some =!= none[Int]

  implicit val catsEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }

  val cat1 = Cat("Garfield", 38, "orange and black")
  // cat1: Cat = Cat(Garfield,38,orange and black)

  val cat2 = Cat("Heathcliff", 32, "orange and black")
  // cat2: Cat = Cat(Heathcliff,32,orange and black)

  cat1 === cat2
  // res17: Boolean = false

  cat1 =!= cat2
  // res18: Boolean = true

  import cats.instances.option._ // for Eq

  val optionCat1 = Option(cat1)
  // optionCat1: Option[Cat] = Some(Cat(Garfield,38,orange and black))

  val optionCat2 = Option.empty[Cat]
  // optionCat2: Option[Cat] = None

  optionCat1 === optionCat2
  // res19: Boolean = false

  optionCat1 =!= optionCat2
  // res20: Boolean = true

}
