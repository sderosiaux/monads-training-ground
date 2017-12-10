package com.ctheu.users.dsls

import cats.InjectK
import cats.free.Free
import com.ctheu.users.Users._

object DslK {
  class UserCommands[F[_]](implicit I: InjectK[UserCommand, F]) {
    def create(id: String) = Free.inject[UserCommand, F](Create(id))
    def rename(id: String, newName: String) = Free.inject[UserCommand, F](Rename(id, newName))
    def find(id: String) = Free.inject[UserCommand, F](Find(id))
    def all() = Free.inject[UserCommand, F](All())
    def cleanup() = Free.inject[UserCommand, F](Cleanup())
  }
  object UserCommands {
    implicit def userCommands[F[_]](implicit I: InjectK[UserCommand, F]) = new UserCommands[F]
  }
}
