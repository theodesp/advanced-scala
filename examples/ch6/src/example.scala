object ch6 extends App {
  import cats.syntax.either._ // for catchOnly

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(_ => s"Couldn't read $str")

  for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield (a + b + c)


  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  Semigroupal[Option].product(None, Some("abc"))
  // res1: Option[(Nothing, String)] = None

  Semigroupal[Option].product(Some(123), None)
  // res2: Option[(Int, Nothing)] = None

  Semigroupal.tuple3(Option(1), Option(2), Option(3))
  // res3: Option[(Int, Int, Int)] = Some((1,2,3))

  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
  // res4: Option[(Int, Int, Int)] = None

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  // res5: Option[Int] = Some(6)

  Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
  // res6: Option[Int] = None

  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._     // for tupled and mapN

  (Option(123), Option("abc")).tupled

  case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)

  import cats.Monoid
  import cats.instances.int._        // for Monoid
  import cats.instances.invariant._  // for Semigroupal
  import cats.instances.list._       // for Monoid
  import cats.instances.string._     // for Monoid
  import cats.syntax.apply._         // for imapN

  case class Cat2(
                   name: String,
                   yearOfBirth: Int,
                   favoriteFoods: List[String]
                 )

  val tupleToCat: (String, Int, List[String]) => Cat2 =
    Cat2.apply _

  val catToTuple: Cat2 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat2] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._ // for |+|

  val garfield   = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))

  garfield |+| heathcliff

  import cats.Semigroupal
  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.language.higherKinds

  val futurePair = Semigroupal[Future].
    product(Future("Hello"), Future(123))

  Await.result(futurePair, 1.second)
  // res1: (String, Int) = (Hello,123)

  import cats.Semigroupal
  import cats.instances.list._ // for Semigroupal

  Semigroupal[List].product(List(1, 2), List(3, 4))
  // res5: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))

  import cats.syntax.flatMap._ // for flatMap
  import cats.syntax.functor._ // for map
  import cats.Monad

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type AllErrorsOr[A] = Validated[List[String], A]

  Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  // res1: AllErrorsOr[(Nothing, Nothing)] = Invalid(List(Error 1, Error 2))

  val v = Validated.Valid(123)
  // v: cats.data.Validated.Valid[Int] = Valid(123)

  val i = Validated.Invalid(List("Badness"))
  // i: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))

  import cats.syntax.validated._ // for valid and invalid

  123.valid[List[String]]
  // res2: cats.data.Validated[List[String],Int] = Valid(123)

  List("Badness").invalid[Int]
  // res3: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  import cats.syntax.applicative._      // for pure
  import cats.syntax.applicativeError._ // for raiseError

  type ErrorsOr[A] = Validated[List[String], A]

  123.pure[ErrorsOr]
  // res5: ErrorsOr[Int] = Valid(123)

  List("Badness").raiseError[ErrorsOr, Int]
  // res6: ErrorsOr[Int] = Invalid(List(Badness))

  Validated.catchOnly[NumberFormatException]("foo".toInt)
  // res7: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.catchNonFatal(sys.error("Badness"))
  // res8: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeException: Badness)

  Validated.fromTry(scala.util.Try("foo".toInt))
  // res9: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.fromEither[String, Int](Left("Badness"))
  // res10: cats.data.Validated[String,Int] = Invalid(Badness)

  Validated.fromOption[String, Int](None, "Badness")
  // res11: cats.data.Validated[String,Int] = Invalid(Badness)

  type AllErrorsOr[A] = Validated[String, A]
  import cats.instances.string._ // for Semigroup

  Semigroupal[AllErrorsOr]

  import cats.syntax.apply._ // for tupled

  (
    "Error 1".invalid[Int],
    "Error 2".invalid[Int]
  ).tupled

  import cats.instances.vector._ // for Semigroupal

  (
    Vector(404).invalid[Int],
    Vector(500).invalid[Int]
  ).tupled
  // res15: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)] = Invalid(Vector(404, 500))

  import cats.data.NonEmptyVector

  (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  ).tupled
  // res16: cats.data.Validated[cats.data.NonEmptyVector[String],(Int, Int)] = Invalid(NonEmptyVector(Error 1, Error 2))

  123.valid.map(_ * 100)
  // res17: cats.data.Validated[Nothing,Int] = Valid(12300)

  "?".invalid.leftMap(_.toString)
  // res18: cats.data.Validated[String,Nothing] = Invalid(?)

  123.valid[String].bimap(_ + "!", _ * 100)
  // res19: cats.data.Validated[String,Int] = Valid(12300)

  "?".invalid[Int].bimap(_ + "!", _ * 100)
  // res20: cats.data.Validated[String,Int] = Invalid(?!)

  32.valid.andThen { a =>
    10.valid.map { b =>
      a + b
    }
  }
  // res21: cats.data.Validated[Nothing,Int] = Valid(42)

  import cats.syntax.either._ // for toValidated

  "Badness".invalid[Int]

  "Badness".invalid[Int].toEither
  // res23: Either[String,Int] = Left(Badness)

  "Badness".invalid[Int].toEither.toValidated
  // res24: cats.data.Validated[String,Int] = Invalid(Badness)

  "fail".invalid[Int].getOrElse(0)
  // res26: Int = 0

  "fail".invalid[Int].fold(_ + "!!!", _.toString)
  // res27: String = fail!!!

  case class User(name: String, age: Int)

  import cats.data.Validated

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  val getName = getValue("name") _
  // getName: FormData => FailFast[String] = <function1>

  getName(Map("name" -> "Dade Murphy"))
  // res29: FailFast[String] = Right(Dade Murphy)

  getName(Map())
  // res30: FailFast[String] = Left(List(name field not specified))

  import cats.syntax.either._ // for catchOnly

  type NumFmtExn = NumberFormatException

  def parseInt2(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumFmtExn](data.toInt).
      leftMap(_ => List(s"$name must be an integer"))

  parseInt2("age")("11")
  // res33: FailFast[Int] = Right(11)

  parseInt2("age")("foo")
  // res34: FailFast[Int] = Left(List(age must be an integer))

  def nonBlank(name: String)(data: String): FailFast[String] =
    Right(data).
      ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).
      ensure(List(s"$name must be non-negative"))(_ >= 0)

  nonBlank("name")("Dade Murphy")
  // res36: FailFast[String] = Right(Dade Murphy)

  nonBlank("name")("")
  // res37: FailFast[String] = Left(List(name cannot be blank))

  nonNegative("age")(11)
  // res38: FailFast[Int] = Right(11)

  nonNegative("age")(-1)
  // res39: FailFast[Int] = Left(List(age must be non-negative))

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt2("age")).
      flatMap(nonNegative("age"))

  readName(Map("name" -> "Dade Murphy"))
  // res41: FailFast[String] = Right(Dade Murphy)

  readName(Map("name" -> ""))
  // res42: FailFast[String] = Left(List(name cannot be blank))

  readName(Map())
  // res43: FailFast[String] = Left(List(name field not specified))

  readAge(Map("age" -> "11"))
  // res44: FailFast[Int] = Right(11)

  readAge(Map("age" -> "-1"))
  // res45: FailFast[Int] = Left(List(age must be non-negative))

  readAge(Map())
  // res46: FailFast[Int] = Left(List(age field not specified))

  import cats.instances.list._ // for Semigroupal
  import cats.syntax.apply._   // for mapN

  def readUser(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)

  readUser(Map("name" -> "Dave", "age" -> "37"))
  // res48: FailSlow[User] = Valid(User(Dave,37))

  readUser(Map("age" -> "-1"))
  // res49: FailSlow[User] = Invalid(List(name field not specified, age must be non-negative))
}