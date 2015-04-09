import actormacro.actorCompanion

import akka.actor.{ActorSystem, Actor}

@actorCompanion class A(a: Int, b: String) extends Actor {
  override def receive = {
    case message =>
      println(message)
  }
}

object Main {
  implicit val system = ActorSystem.apply("foo")
  A.create(1, "a")
}
