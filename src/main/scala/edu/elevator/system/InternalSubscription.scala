package edu.elevator.system

import java.util.concurrent.atomic.AtomicReference

import edu.elevator.Contract._
import edu.elevator.Contract.Elevator.Status
import edu.elevator.system.InternalContract.Stop

case class InnerSubscription(request: Request, elevatorId: Int, dispatcher: Dispatcher) extends Subscription {
  private val elevator = InnerSubscription.ElevatorProxy(dispatcher, elevatorId, this)
  private val callbackHolder: AtomicReference[Callback] = new AtomicReference[Callback](null)

  override val requestId: Long = request.id
  override def onStateChange(callback: Callback): Unit = callbackHolder.set(callback)
  override def cancel: Boolean = dispatcher.cancel(elevatorId, this)

  private def run(event: Event): Unit = {
    Option(callbackHolder.get).foreach(callback => {
      callback.apply(event, elevator)
    })
  }

  def tick(): Unit = run(Event.Wait)
  def pick(): Unit = run(Event.Pick)
  def move(): Unit = run(Event.Move)
  def exit(): Unit = run(Event.Exit)
}

object InnerSubscription {
  case class ElevatorProxy(dispatcher: Dispatcher, elevatorId: Int, subscription: InnerSubscription) extends Elevator {
    override val id = elevatorId
    override def floor: Floor = dispatcher.elevators(elevatorId).floor
    override def status: Status = dispatcher.elevators(elevatorId).status
    override def push(floor: Floor): Unit = dispatcher.accept(Stop(floor, elevatorId), subscription)
  }
}
