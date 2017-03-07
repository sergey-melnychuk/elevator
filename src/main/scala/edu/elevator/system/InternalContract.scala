package edu.elevator.system

object InternalContract {
  import edu.elevator.Contract._

  sealed trait Entry
  case class Call(floor: Floor, direction: Direction) extends Entry
  case class Stop(floor: Floor, elevatorId: Int) extends Entry

  case class Task(entry: Entry, subscription: InnerSubscription)
}
