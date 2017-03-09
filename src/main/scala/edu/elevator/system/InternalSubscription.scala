package edu.elevator.system

import java.util.concurrent.atomic.AtomicReference

import edu.elevator.Contract._
import edu.elevator.Contract.ElevatorControls.Status
import edu.elevator.system.InternalContract.Stop

case class InternalSubscription(request: Request, elevatorId: Int, dispatcher: Dispatcher) extends Subscription {
  private val elevator = InternalSubscription.ElevatorProxy(elevatorId, dispatcher, this)
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
  def stop(): Unit = run(Event.Stop)
  def exit(): Unit = run(Event.Exit)
}

object InternalSubscription {
  case class ElevatorProxy(
    elevatorId: Int,
    dispatcher: Dispatcher,
    subscription: InternalSubscription
  ) extends ElevatorControls {
    override def floor: Floor = Floor(dispatcher.elevators(elevatorId).floor)
    override def status: Status = dispatcher.elevators(elevatorId).status
    override def push(floor: Floor): Unit = dispatcher.submit(Stop(floor, elevatorId), subscription)
    override def enter(): Unit = dispatcher.enter(elevatorId, subscription)
    override def leave(): Unit = dispatcher.leave(elevatorId, subscription)
  }
}
