import scalaz._, Scalaz._

package object asmfreef {
  object algebra {
    sealed trait AsmF[F[_]] extends Applicative[F] {
      def push(v: Int): F[Unit]

      def pop: F[Int]

      def sum: F[Int]

      def mul: F[Int]
    }
  }

  object dsl {
    import algebra._

    sealed trait Dsl[A] {
      def apply[F[_]: AsmF]: F[A]
    }

    def push(v: Int): Dsl[Unit] = new Dsl[Unit] { def apply[F[_]: AsmF] = implicitly[AsmF[F]].push(v) }

    def pop: Dsl[Int] = new Dsl[Int] { def apply[F[_]: AsmF] = implicitly[AsmF[F]].pop }

    def sum: Dsl[Int] = new Dsl[Int] { def apply[F[_]: AsmF] = implicitly[AsmF[F]].sum }

    def mul: Dsl[Int] = new Dsl[Int] { def apply[F[_]: AsmF] = implicitly[AsmF[F]].mul }

    implicit val ApplicativeDsl: Applicative[Dsl] = new Applicative[Dsl] {
      def point[A](a: => A): Dsl[A] = new Dsl[A] { def apply[F[_]: AsmF] = a.point[F] }
      def ap[A, B](fa: => Dsl[A])(f: => Dsl[A => B]): Dsl[B] = new Dsl[B] { 
        def apply[F[_]: AsmF] = fa.apply[F] <*> f.apply[F]
      }
    }
  }

  object printer {
    import algebra._
    import dsl._

    def print[A](p: Dsl[A]): String = {
      type Printer[A] = String

      p.apply(new AsmF[Printer] {
        def point[A](a: => A): Printer[A] = ""

        def ap[A, B](fa: => Printer[A])(f: => Printer[A => B]): Printer[B] = f + fa

        def push(v: Int) = "push " + v + "\n"

        def pop = "pop\n"

        def sum = "sum\n"

        def mul = "mul\n"
      })
    }
  }

  object evaluator {
    import algebra._
    import dsl._

    def evaluate[A](p: Dsl[A]): A = {
      type EvaluatorState = List[Int]

      type Evaluator[A] = State[List[Int], A]

      p.apply(new AsmF[Evaluator] {
        def point[A](a: => A): Evaluator[A] = a.point[Evaluator]

        def ap[A, B](fa: => Evaluator[A])(f: => Evaluator[A => B]): Evaluator[B] = fa <*> f

        def push(v: Int) = State.modify[EvaluatorState](v :: _)

        def pop = State.get[EvaluatorState].map(_.head)

        def sum = for {
          l <- State.get[EvaluatorState]
          (x1 :: x2 :: Nil, ys) = l.splitAt(2)
          v = x1 + x2
          _ <- State.put[EvaluatorState](v :: ys)
        } yield v

        def mul = for {
          l <- State.get[EvaluatorState]
          (x1 :: x2 :: Nil, ys) = l.splitAt(2)
          v = x1 * x2
          _ <- State.put[EvaluatorState](v :: ys)
        } yield v
      }).eval(Nil)
    }
  }

  object example {
    import algebra._
    import dsl._

    def program =
      push(1) *>
      push(2) *>
      sum     *>
      push(9) *>
      mul

    lazy val printed = printer.print(program)

    lazy val evaled = evaluator.evaluate(program)
  }
}