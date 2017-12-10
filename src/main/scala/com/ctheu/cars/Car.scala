package com.ctheu.cars

import com.ctheu.users.User

case class Car(users: List[User] = List(), position: Int = 0)
