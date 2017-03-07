package edu.elevator.system

import java.util.concurrent.atomic.AtomicLong

import edu.elevator.Contract
import edu.elevator.Contract._

class ElevatorSystem(minFloor: Int, maxFloor: Int, numberOfElevators: Int) extends Contract.ElevatorSystem {
  var requestIdCounter = new AtomicLong(0L)
  def requestId: Long = requestIdCounter.incrementAndGet()

  val tickCounter: AtomicLong = new AtomicLong(0L)
  val dispatcher: Dispatcher = Dispatcher(minFloor, maxFloor, numberOfElevators)

  override def call(floor: Floor, direction: Direction): Subscription = {
    val request = Request(requestId, floor, direction)
    if (Request.isValid(minFloor, maxFloor, request)) {
      dispatcher.handle(request)
    } else {
      new Subscription {
        override val requestId: Long = request.id
        override def onStateChange(callback: Callback) = ()
        override def cancel = false
      }
    }
  }

  override def start(): Unit = {
    while (dispatcher.hasTasks()) {
      dispatcher.move()
      tickCounter.incrementAndGet()
    }
    println("dispatcher completed in " + tickCounter.get() + " ticks")
  }

  override def start(n: Int): Unit = {
    var i = 0
    while (i < n) {
      i += 1
      dispatcher.move()
    }
  }

  override def stop: Unit = ()
}

object ElevatorSystem {
  def apply(minFloor: Int, maxFloor: Int, numberOfElevators: Int): ElevatorSystem =
    new ElevatorSystem(minFloor, maxFloor, numberOfElevators)
}
