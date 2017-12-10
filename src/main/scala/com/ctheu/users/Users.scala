package com.ctheu.users

import cats.data.State
import cats.free.Free

object Users {
  type FreeUserCommand[A] = Free[UserCommand, A]
  type UserCommandState[A] = State[List[User], A]

  sealed trait UserCommand[A]
  final case class Create(id: String) extends UserCommand[User]
  final case class Find(id: String) extends UserCommand[Option[User]]
  final case class Rename(id: String, newName: String) extends UserCommand[Option[User]]
  final case class All() extends UserCommand[List[User]]
  final case class Cleanup() extends UserCommand[Unit]
}
