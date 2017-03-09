package edu.elevator

object Contract {

  case class Floor(level: Int) extends AnyVal

  sealed trait Direction
  object Direction {
    case object Up extends Direction
    case object Down extends Direction
  }

  trait ElevatorControls {
    val elevatorId: Int
    def floor: Floor
    def status: ElevatorControls.Status
    def push(floor: Floor): Unit
    def enter(): Unit
    def leave(): Unit
  }

  object ElevatorControls {
    sealed trait Status
    case object Stopped extends Status
    case object Waiting extends Status
    case class Moving(direction: Direction) extends Status
  }

  sealed trait Event
  object Event {
    case object Wait extends Event
    case object Pick extends Event
    case object Move extends Event
    case object Stop extends Event
    case object Exit extends Event
  }

  type Callback = (Event, ElevatorControls) => Unit

  trait Subscription {
    val requestId: Long
    def onStateChange(callback: Callback): Unit
    def cancel: Boolean
  }

  trait ElevatorSystem {
    def call(floor: Floor, direction: Direction): Subscription
    def start(n: Int): Unit
    def start: Unit
    def stop: Unit
  }
}
