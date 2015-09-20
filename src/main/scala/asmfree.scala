import scalaz._, Scalaz._

package object asmfree {
  object algebra {
    sealed trait AsmF[A]

    case class AsmPush[A](value: Int, next: A) extends AsmF[A]
    case class AsmPop [A](next : Int => A)     extends AsmF[A]
    case class AsmSum [A](value: Int => A)     extends AsmF[A]
    case class AsmMul [A](value: Int => A)     extends AsmF[A]
  }

  object dsl {
    import algebra._

    type Dsl[A] = FreeAp[AsmF, A]

    private def lift[A](value: AsmF[A]): Dsl[A] = FreeAp.lift[AsmF, A](value)

    def push(value: Int): Dsl[Unit] = lift(AsmPush(value, ()))
    def pop             : Dsl[Int]  = lift(AsmPop(identity))
    def sum             : Dsl[Int]  = lift(AsmSum(identity))
    def mul             : Dsl[Int]  = lift(AsmMul(identity))
  }

  object printer {
    import algebra._
    import dsl.Dsl

    private type G[A] = List[String]

    def print[A](p: Dsl[A]): String = p.analyze[List[String]](new NaturalTransformation[AsmF, G] {
      def apply[A](p: AsmF[A]): G[A] = p match {
        case AsmPush(v, _) => ("push " + v) :: Nil
        case AsmPop(_)     => "pop" :: Nil
        case AsmSum(_)     => "sum" :: Nil
        case AsmMul(_)     => "mul" :: Nil
      }
    }).reverse.mkString("\n")
  }

  object evaluator {
    import algebra._
    import dsl._

    type EvaluatorState = List[Int]
    type Evaluator[A]   = State[EvaluatorState, A]

    def evaluate[A](p: Dsl[A]): A = evaluate0(p).eval(Nil)

    def evaluate0[A](p: Dsl[A]): Evaluator[A] = p.para(_.point[Evaluator], 
      new NaturalTransformation[({type λ[α] = (AsmF[α], FreeAp[AsmF, α => A])})#λ, 
                                ({type λ[α] = Evaluator[A]})#λ] {
        type F[A0] = (AsmF[A0], Dsl[A0 => A])
        type G[A0] = Evaluator[A]

        def take2: Evaluator[(Int, Int)] = for {
          l  <- State.get[EvaluatorState]
          h1 :: h2 :: Nil = l
          _  <- State.modify[EvaluatorState](_.drop(2))
        } yield (h1, h2)

        def apply[A0](ap: F[A0]): G[A0] = ap._1 match {
          case AsmPush(v,a0) => for {
                                  _  <- State.modify[EvaluatorState](v :: _)
                                  a  <- evaluate0(a0.point[Dsl].ap(ap._2))
                                } yield a

          case AsmPop (   n) => for {
                                  h  <- State.get[EvaluatorState].map(_.head)
                                  _  <- State.modify[EvaluatorState](_.tail)
                                  a  <- evaluate0(n(h).point[Dsl].ap(ap._2))
                                } yield a

          case AsmSum (v   ) => for {
                                  t  <- take2
                                  (h1, h2) = t
                                  a0 = v(h1 + h2)
                                  _  <- State.modify[EvaluatorState](a0.asInstanceOf[Int] :: _)
                                  a  <- evaluate0(a0.point[Dsl].ap(ap._2))
                                } yield a

          case AsmMul (v   ) => for {
                                  t  <- take2
                                  (h1, h2) = t
                                  a0 = v(h1 * h2)
                                  _  <- State.modify[EvaluatorState](a0.asInstanceOf[Int] :: _)
                                  a  <- evaluate0(a0.point[Dsl].ap(ap._2))
                                } yield a
        }
    })
  }

  object example {
    import dsl._

    lazy val program = 
      push(1) *>
      push(2) *>
      sum     *>
      push(9) *>
      mul

    lazy val printed = printer.print(program)

    lazy val evaled = evaluator.evaluate(program)
  }
}