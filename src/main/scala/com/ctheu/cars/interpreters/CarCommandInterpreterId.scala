package com.ctheu.cars.interpreters

import cats.{Id, ~>}
import com.ctheu.cars.{Car, Cars}
import com.ctheu.users.User
import monocle.macros.GenLens

object CarCommandInterpreterId extends (Cars.CarCommand ~> Id) {
  import Cars._

  val positionLens = GenLens[Car](_.position)
  val usersLens = GenLens[Car](_.users)

  override def apply[A](fa: CarCommand[A]): Id[A] = fa match {
    case Create() => Car()
    case Move(c: Car) => positionLens.set(c.position + 1)(c)
    case AddUser(c: Car, u: User) => usersLens.set(c.users :+ u)(c)
  }
}
