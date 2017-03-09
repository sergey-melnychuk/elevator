package edu.elevator.system

import edu.elevator.Contract._
import edu.elevator.system.InternalContract._

private [system]
class Dispatcher(minFloor: Int, maxFloor: Int, numberOfElevators: Int, router: Option[Router] = None) {
  assert(minFloor < maxFloor)
  private[system] val numberOfFloors: Int = maxFloor - minFloor + 1

  private[system] val elevators: Array[InternalElevator] = (1 to numberOfElevators).toArray.map(id => {
    InternalElevator(id, this, minFloor, maxFloor, minFloor, ElevatorControls.Waiting)
  })

  def floor(elevatorId: Int): Int = elevator(elevatorId).floor
  def state(elevatorId: Int): ElevatorControls.Status = elevator(elevatorId).status

  private[system] def elevator(id: Int): InternalElevator =
    if (id > 0 && id <= numberOfElevators) elevators(id)
    else throw new IllegalArgumentException(s"No such elevator: $id")

  private[system] def route(call: Call, elevators: Array[InternalElevator]): Int =
    router.map(_.route(call, elevators)).getOrElse(0)

  private[system] def submit(elevatorId: Int, call: Call, subscription: InternalSubscription): Unit = {
    elevators(elevatorId).accept(call, subscription)
  }

  private[system] def submit(stop: Stop, subscription: InternalSubscription): Unit = {
    elevators(stop.elevatorId).accept(stop, subscription)
  }

  private[system] def cancel(elevatorId: Int, subscription: InternalSubscription): Boolean = {
    val elevator = elevators(elevatorId)
    val affected = elevator.cancel(subscription)
    if (affected) {
      subscription.exit()
    }
    affected
  }

  private[system] def leave(elevatorId: Int, subscription: InternalSubscription): Unit = {
    elevators(elevatorId).leave(subscription)
  }

  private[system] def enter(elevatorId: Int, subscription: InternalSubscription): Unit = {
    elevators(elevatorId).enter(subscription)
  }

  def handle(request: Request): Subscription = {
    val elevatorId = route(Call(request.floor, request.direction), elevators)
    val subscription = new InternalSubscription(request, elevatorId, this)
    submit(elevatorId, Call(request.floor, request.direction), subscription)
    subscription
  }

  def hasTasks(): Boolean = elevators.exists(e => e.hasTasks() || e.nonEmpty())

  def move(): Unit = {
    elevators.foreach(e => {
      e.move()
      e.fire()
      e.check()
      println(e.report(InternalElevator.ConsoleReporter))
    })
  }

  def report(reporter: InternalElevator.Reporter): Unit = {
    elevators.foreach(_.report(reporter))
  }
}

private[system]
object Dispatcher {
  def apply(minFloor: Int, maxFloor: Int, numberOfElevators: Int): Dispatcher =
    new Dispatcher(minFloor, maxFloor, numberOfElevators)
  def apply(minFloor: Int, maxFloor: Int, numberOfElevators: Int, router: Router): Dispatcher =
    new Dispatcher(minFloor, maxFloor, numberOfElevators, Some(router))
}
