package com.ctheu.cars.dsls

import cats.InjectK
import cats.free.Free
import com.ctheu.cars.Car
import com.ctheu.cars.Cars.{AddUser, CarCommand, Create, Move}
import com.ctheu.users.User

object DslK {
  class CarCommands[F[_]](implicit I: InjectK[CarCommand, F]) {
    def create() = Free.inject[CarCommand, F](Create())
    def move(c: Car): Free[F, Car] = Free.inject[CarCommand, F](Move(c))
    def addUser(c: Car, u: User): Free[F, Car] = Free.inject[CarCommand, F](AddUser(c, u))
  }
  object CarCommands {
    // simple constructor
    implicit def carCommands[F[_]](implicit I: InjectK[CarCommand, F]): CarCommands[F] = new CarCommands[F]
  }
}
