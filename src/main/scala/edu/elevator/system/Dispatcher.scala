package edu.elevator.system

import edu.elevator.Contract._
import edu.elevator.system.InternalContract._

private [system]
class Dispatcher(minFloor: Int, maxFloor: Int, numberOfElevators: Int) {
  assert(minFloor < maxFloor)
  private[system] val numberOfFloors: Int = maxFloor - minFloor + 1

  private[system] val elevators: Array[InnerElevator] = (1 to numberOfElevators).toArray.map(id => {
    InnerElevator(id, this, minFloor, maxFloor, minFloor, Elevator.Waiting)
  })

  private[system] def elevator(id: Int): Elevator =
    if (id > 0 && id <= numberOfElevators) elevators(id)
    else throw new IllegalArgumentException(s"No such elevator: $id")

  private[system] def route(entry: Entry, elevators: Array[InnerElevator]): Int = entry match {
    case Call(Floor(_), Direction.Up) => 0
    case Call(Floor(_), Direction.Down) => 0
    case _ => 0
  }

  private[system] def accept(call: Call, subscription: InnerSubscription): Unit = {
    val idx = route(call, elevators)
    elevators(idx).accept(call, subscription)
  }

  private[system] def accept(stop: Stop, subscription: InnerSubscription): Unit = {
    elevators(stop.elevatorId).accept(stop, subscription)
  }

  private[system] def cancel(elevatorId: Int, subscription: InnerSubscription): Boolean = {
    val elevator = elevators(elevatorId)
    val affected = elevator.cancel(subscription)
    if (affected) {
      subscription.exit()
    }
    affected
  }

  def handle(request: Request): Subscription = {
    val elevatorId = route(Call(request.floor, request.direction), elevators)
    val subscription = new InnerSubscription(request, elevatorId, this)
    accept(Call(request.floor, request.direction), subscription)
    subscription
  }

  def hasTasks(): Boolean = elevators.exists(_.hasTasks())

  def move(): Unit = {
    elevators.foreach(e => {
      e.move()
      e.fire()
      e.check()
    })
  }

  def report(reporter: InnerElevator.Reporter): Unit = {
    elevators.foreach(_.report(reporter))
  }
}

private[system]
object Dispatcher {
  def apply(minFloor: Int, maxFloor: Int, numberOfElevators: Int): Dispatcher =
    new Dispatcher(minFloor, maxFloor, numberOfElevators)
}
