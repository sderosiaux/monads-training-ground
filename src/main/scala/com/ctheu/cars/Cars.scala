package com.ctheu.cars

import cats.free.Free
import com.ctheu.users.User

object Cars {
  type FreeCarCommand[A] = Free[CarCommand, A]

  sealed trait CarCommand[A]
  final case class Create() extends CarCommand[Car]
  final case class Move(c: Car) extends CarCommand[Car]
  final case class AddUser(c: Car, u: User) extends CarCommand[Car]
}
