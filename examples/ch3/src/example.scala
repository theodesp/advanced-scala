import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val future: Future[String] =
  Future(123).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!")

Await.result(future, 1.second)

import cats.instances.function._ // for Functor
import cats.syntax.functor._     // for map

val func1: Int => Double =
  (x: Int) => x.toDouble

val func2: Double => Double =
  (y: Double) => y * 2

(func1 map func2)(1)
(func1 andThen func2)(1)
func2(func1(1))

//Kinds are like types for types.

//List    type constructor, takes one parameter
// List[A]  type, produced using a type parameter

// math.abs    function, takes one parameter
// math.abs(x) value, produced using a value parameter

import scala.language.higherKinds
import cats.Functor
import cats.instances.list._   // for Functor
import cats.instances.option._ // for Functor

val list1 = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
val option1 = Option(123)
val option2 = Functor[Option].map(option1)(_.toString)

// Lifting
val func = (x: Int) => x + 1
val liftedFunc = Functor[Option].lift(func)
liftedFunc(None)

import cats.instances.function._ // for Functor
import cats.syntax.functor._     // for map

val func11 = (a: Int) => a + 1
val func22 = (a: Int) => a * 2
val func3 = (a: Int) => a + "!"
val func4 = func11.map(func22).map(func3)

func4(123)

def doMath[F[_]](start: F[Int])
                (implicit functor: Functor[F]): F[Int] =
  start.map(n => n + 1 * 2)

import cats.instances.option._ // for Functor
import cats.instances.list._   // for Functor

doMath(Option(20))
// res3: Option[Int] = Some(22)

doMath(List(1, 2, 3))
// res4: List[Int] = List(3, 4, 5)

final case class Box[A](value: A)

val box = Box[Int](123)

implicit val boxFunctor: Functor[Box] =
  new Functor[Box] {
    def map[A, B](a: Box[A])(func: A => B): Box[B] =
      Box(func(a.value))
  }

box.map(value => value + 1)

//import scala.concurrent.{Future, ExecutionContext}
//
//implicit def futureFunctor
//(implicit ec: ExecutionContext): Functor[Future] =
//  new Functor[Future] {
//    def map[A, B](value: Future[A])(func: A => B): Future[B] =
//      value.map(func)
//  }

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

// Branch(Leaf(10), Leaf(20)).map(_ * 2) will not work as we need a Tree

implicit val treeFunctor: Functor[Tree] =
  new Functor[Tree] {
    def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
      tree match {
        case Branch(l, r)    => Branch(map(l)(func), map(r)(func))
        case Leaf(a) => Leaf(func(a))
      }
  }

Tree.leaf(100).map(_ * 2)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

// contravariant functor
// have F[B] and A => B and we get a F[A]

// example
trait Printable[A] {
  self => // to recognise outer printable from inner one

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}

def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

implicit val booleanPrintable: Printable[Boolean] =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

format("hello")
format(true)

implicit def boxPrintable[A](implicit p: Printable[A]) =
  new Printable[Box[A]] {
    def format(box: Box[A]): String =
      p.format(box.value)
  }

implicit def boxPrintable2[A](implicit p: Printable[A]) =
  p.contramap[Box[A]](_.value)

sealed trait Person {
  def name: String
}
// Teacher is a Person
final case class Teacher(name: String) extends Person
// Student is a Person
final case class Student(name: String) extends Person

// Classroom has a teacher and a list of students
case class Classroom(name: String, teacher: Teacher, students: List[Student])

// A Choice is part of a Question and has a label
case class Choice(label: String)
// A Question contains a list of choices.
final case class Question(id: String, choices: List[Choice], answer: Choice)
final case class Answer(questionId: String, answer: Choice)

// A Quiz contains a list of questions.
case class Quiz(id: String, questions: List[Question])
case class QuizAnswer(id: String, answers: List[Answer])

// Teachers can assign quizzes to students. So we associate a list of quizes with a
// Student. We also assing a unique id for each assignment
case class Assignment(id: String, quizzes: List[Quiz], student: Student)
case class Submission(assignmentId: String, quizAnswers: List[QuizAnswer])
case class Grade(assignmentId: String, score: Number)

val teacher = Teacher("MrKimble")
val student1 = Student("Alex")
val student2 = Student("Mike")
val classroom1 = Classroom("Class101", teacher, List(student1, student2))

val question1 = Question("q1", List(Choice("Choice1"), Choice("Choice2")), Choice("Choice1"))
val question2 = Question("q2", List(Choice("Choice1"), Choice("Choice2")), Choice("Choice2"))
val quiz = Quiz("qq1", List(question1, question2))

val assignment1 = Assignment("as1", List(quiz), student1)
val assignment2 = Assignment("as2", List(quiz), student2)

val answer1 = Answer("q1", Choice("Choice2"))
val answer2 = Answer("q2", Choice("Choice2"))

val answer3 = Answer("q1", Choice("Choice1"))
val answer4 = Answer("q2", Choice("Choice1"))
val quizAnswer1 = QuizAnswer("qq1", List(answer1, answer2))
val quizAnswer2 = QuizAnswer("qq2", List(answer3, answer4))

val submission1 = Submission("as1", List(quizAnswer1))
val submission2 = Submission("as2", List(quizAnswer2))

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))

      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

implicit val intCodec: Codec[Int] =
  stringCodec.imap(_.toInt, _.toString)

implicit val booleanCodec: Codec[Boolean] =
  stringCodec.imap(_.toBoolean, _.toString)

implicit val doubleCodec: Codec[Double] =
  new Codec[Double] {
    def encode(value: Double): String = value.toString
    def decode(value: String): Double = value.toDouble
  }

implicit val doubleCodec: Codec[Double] =
  stringCodec.imap[Double](_.toDouble, _.toString)


implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
  c.imap[Box[A]](Box(_), _.value)

import cats.Contravariant
import cats.Show
import cats.instances.string._

val showString = Show[String]

val showSymbol = Contravariant[Show].
  contramap(showString)((sym: Symbol) => s"'${sym.name}")

showSymbol.show('dave)

import cats.syntax.contravariant._ // for contramap

showString.contramap[Symbol](_.name).show('dave)

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

implicit val symbolMonoid: Monoid[Symbol] =
  Monoid[String].imap(Symbol.apply)(_.name)

Monoid[Symbol].empty

'a |+| 'few |+| 'words