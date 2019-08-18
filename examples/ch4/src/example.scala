object ch4 extends App {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)


  /*
  the first call to parseInt returns a None or a Some;
  if it returns a Some, the flatMap method calls our function and passes us the integer aNum;
  the second call to parseInt returns a None or a Some;
  if it returns a Some, the flatMap method calls our function and passes us bNum;
  the call to divide returns a None or a Some, which is our result.
   */
  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum =>
        divide(aNum, bNum)
      }
    }

  stringDivideBy("6", "2")

  stringDivideBy("6", "0")

  import scala.language.higherKinds

  //trait Monad[F[_]] {
  //  def pure[A](value: A): F[A]
  //
  //  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  //
  //  def map[A, B](value: F[A])(func: A => B): F[B] =
  //    flatMap(value) { aVal =>
  //      pure(func(aVal))
  //    }
  //}

  import cats.Monad
  import cats.instances.option._ // for Monad
  import cats.instances.list._   // for Monad


  val opt1 = Monad[Option].pure(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))

  import cats.instances.option._   // for Monad
  import cats.syntax.applicative._ // for pure
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  1.pure[Option]
  1.pure[List]

  //def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  //  a.flatMap(x => b.map(y => x*x + y*y))

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  sumSquare(Option(3), Option(4))
  sumSquare(List(1, 2, 3), List(4, 5))

  import cats.Id

  sumSquare(3 : Id[Int], 4 : Id[Int])

  def pure[A](value: A): Id[A] =
    value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String)
    extends LoginError

  final case class PasswordIncorrect(username: String)
    extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  import cats.Eval

  val now = Eval.now(math.random + 1000)

  val later = Eval.later(math.random + 2000)

  val always = Eval.always(math.random + 3000)

  val x = Eval.now {
    println("Computing X")
    math.random
  }

  x.value
  x.value


  val greeting = Eval.
    always { println("Step 1"); "Hello" }.
    map { str => println("Step 2"); s"$str world" }

  greeting.value

  val saying = Eval.
    always { println("Step 1"); "The cat" }.
    map { str => println("Step 2"); s"$str sat on" }.
    memoize.
    map { str => println("Step 3"); s"$str the mat" }

  saying.value
  saying.value

  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  factorial(50000).value

  def foldRightEval[A, B](as: List[A], acc: Eval[B])
                         (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  import cats.data.Writer
  import cats.instances.vector._ // for Monoid

  Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)

  import cats.instances.vector._   // for Monoid
  import cats.syntax.applicative._ // for pure

  type Logged[A] = Writer[Vector[String], A]

  123.pure[Logged]

  import cats.syntax.writer._ // for tell

  Vector("msg1", "msg2", "msg3").tell

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult: Int =
    a.value

  val aLog: Vector[String] =
    a.written

  val (log, result) = b.run

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  writer1.run

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  writer2.run

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  writer3.run

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }
  writer4.run

  val writer5 = writer1.reset
  writer5.run
  val writer6 = writer1.swap
  writer6.run

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  //def factorial(n: Int): Int = {
  //  val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
  //  println(s"fact $n $ans")
  //  ans
  //}



  import cats.syntax.writer._ // for tell

  Vector("Message").tell

  import cats.instances.vector._ // for Monoid

  41.pure[Logged].map(_ + 1)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _   <- Vector(s"fact $n $ans").tell
    } yield ans



  factorial(5)

  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)
  // defined class Cat

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")

  greetKitty.run(Cat("Heathcliff", "junk food"))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne"))

  greetAndFeed(Cat("Heathcliff", "junk food"))

  import cats.data.Reader // for pure

  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  import cats.syntax.applicative._ // for pure

  def checkLogin(
                  userId: Int,
                  password: String): DbReader[Boolean] =
    for {
      username   <- findUsername(userId)
      passwordOk <- username.map { username =>
        checkPassword(username, password)
      }.getOrElse {
        false.pure[DbReader]
      }
    } yield passwordOk

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  checkLogin(1, "zerocool").run(db)
  // res10: cats.Id[Boolean] = true

  checkLogin(4, "davinci").run(db)
  // res11: cats.Id[Boolean] = false

  import cats.data.State

  val aa = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (state, result) = aa.run(10).value
  // state: Int = 10
  // result: String = The state is 10

  // Get the state, ignore the result:
  val state = aa.runS(10).value
  // state: Int = 10

  // Get the result, ignore the state:
  val result = aa.runA(10).value

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state, result) = both.run(20).value

  val getDemo = State.get[Int]

  getDemo.run(10).value

  val setDemo = State.set[Int](30)

  setDemo.run(10).value
  val pureDemo = State.pure[Int, String]("Result")

  pureDemo.run(10).value

  val inspectDemo = State.inspect[Int, String](_ + "!")

  inspectDemo.run(10).value

  val modifyDemo = State.modify[Int](_ + 1)

  modifyDemo.run(10).value


  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail!")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  evalOne("42").runA(Nil).value

  val program = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  program.runA(Nil).value

  import cats.syntax.applicative._ // for pure

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  val program1 = evalAll(List("1", "2", "+", "3", "*"))
  program1.runA(Nil).value

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 4 + *")

  import cats.Monad
  import scala.annotation.tailrec

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](opt: Option[A])
                     (fn: A => Option[B]): Option[B] =
      opt flatMap fn

    def pure[A](opt: A): Option[A] =
      Some(opt)

    @tailrec
    def tailRecM[A, B](a: A)
                      (fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None           => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  import cats.Monad

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])
                     (func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value)  =>
          func(value)
      }

    def tailRecM[A, B](arg: A)
                      (func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
                open: List[Tree[Either[A, B]]],
                closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)

          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)

          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)

          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(func(arg)), Nil).head
    }
  }
}