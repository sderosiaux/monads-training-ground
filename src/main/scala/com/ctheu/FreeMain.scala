package com.ctheu

/*

https://blog.scalac.io/2016/06/02/overview-of-free-monad-in-cats.html

 */

import cats._
import cats.data.EitherK
import cats.free.Free
import cats.implicits._
import com.ctheu.cars.interpreters.CarCommandInterpreterId
import com.ctheu.cars.{Car, Cars}
import com.ctheu.users._
import com.ctheu.users.interpreters.{UsersInterpreterFuture, UsersInterpreterId, UsersInterpreterOption, UsersStateInterpreterId}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object FreeMain extends App {
  import com.ctheu.users.dsls.Dsl._
  import com.ctheu.users.dsls.DslK._
  import com.ctheu.cars.dsls.DslK._

  def finderProgram(who: String) = for {
    _ <- create("sd")
    _ <- create("po")
    _ <- rename("po", "poooo")
    _ <- addTitle("poooo", "M.")
    u <- find(who)
    l <- all()
    _ <- cleanup()
  } yield (u, l)

  {
    // Id interpreter
    val (sd, all) = finderProgram("sd").foldMap(UsersInterpreterId)
    println(sd)
    println(all)
    // Some(User(sd))
    // List(User(sd), User(M. poooo))
  }

  println()

  {
    // Future interpreter
    import scala.concurrent.ExecutionContext.Implicits._
    val (sd, all) = Await.result(finderProgram("sd").foldMap(UsersInterpreterFuture), Duration.Inf)
    println(sd)
    println(all)
    // Some(User(sd))
    // List(User(sd), User(M. poooo))
  }

  println()

  {
    val emptyAllProgram = all()
    import scala.concurrent.ExecutionContext.Implicits._

    // Option interpreter
    println(" id=" + emptyAllProgram.foldMap(UsersInterpreterId))
    println("fut=" + Await.result(emptyAllProgram.foldMap(UsersInterpreterFuture), Duration.Inf))
    println("opt=" + emptyAllProgram.foldMap(UsersInterpreterOption))

    //  id=List()
    // fut=List()
    // opt=None
  }

  println()

  {
    // UsersStateInterpreterId only (state is provided, not internal)
    println(create("sd").foldMap(UsersStateInterpreterId).run(List(User("jo"))).value)
    println(all().foldMap(UsersStateInterpreterId).run(List(User("jo"))).value)
    println(rename("sd", "toto").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)
    println(addTitle("sd", "M.").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)
    println(addTitle("sad", "M.").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)

    // (List(User(jo), User(sd)),User(sd))
    // (List(User(jo)),List(User(jo)))
    // (List(User(jo), User(toto)),Some(User(toto)))
    // (List(User(jo), User(M. sd)),Some(User(M. sd)))
    // (List(User(jo), User(sd)),None)
  }

  println()

  {
    // composition

    // coproduct
    type AllCommands[A] = EitherK[Cars.CarCommand, Users.UserCommand, A]

    def program(implicit CC: cars.dsls.DslK.CarCommands[AllCommands], UC: users.dsls.DslK.UserCommands[AllCommands]): Free[AllCommands, Car] = {
      for {
        u <- UC.create("sd")
        c1 <- CC.create()
        c2 <- CC.move(c1)
        c3 <- CC.addUser(c2, u)
      } yield c3
    }

    val fullInterpreter: AllCommands ~> Id = CarCommandInterpreterId or UsersInterpreterId // the order matters!
    println(program.foldMap(fullInterpreter))
    // Car(List(User(sd)),1)

  }
}
