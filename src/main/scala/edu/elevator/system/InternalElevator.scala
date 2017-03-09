package edu.elevator.system

import edu.elevator.Contract.Direction.{Down, Up}
import edu.elevator.Contract.ElevatorControls._
import edu.elevator.Contract._
import edu.elevator.system.InternalContract.{Call, Stop, Task}

import scala.collection.mutable

case class InternalElevator(id: Int, dispatcher: Dispatcher, minFloor: Int, maxFloor: Int, startFloor: Int, startStatus: Status) /*extends Elevator*/ {
  private val plan: Array[Set[Task]] = Array.fill(dispatcher.numberOfFloors){Set.empty[Task]}
  private val waiting: mutable.Set[InternalSubscription] = mutable.Set.empty[InternalSubscription]
  private val moving: mutable.Set[InternalSubscription] = mutable.Set.empty[InternalSubscription]
  private var currentFloor: Int = startFloor
  private var currentStatus: Status = startStatus

  def floor: Int = currentFloor
  def status: Status = currentStatus

  def enter(subscription: InternalSubscription): Boolean = {
    val call = plan.flatten.filter(_ match {
      case Task(Call(Floor(f), _), s) => (s.requestId == subscription.requestId) && (f == currentFloor)
      case _ => false
    })
    call.headOption
      .map(c => {
        moving += subscription
        waiting -= subscription
      })
      .nonEmpty
  }

  def leave(subscription: InternalSubscription): Boolean = {
    val matched: List[Stop] = plan.flatten.toList collect {
      case Task(stop @ Stop(Floor(f), elevatorId), s)
        if (s.requestId == subscription.requestId) =>
          stop
    }
    matched.foreach(_ match {
      case Stop(Floor(f), _) =>
        val idx = f - minFloor
        val before = plan(idx)
        val after = before.filter(_ match {
          case Task(Stop(Floor(_), _), s) => s.requestId != subscription.requestId
          case _ => true
        })
        plan.update(idx, after)
        before.foreach(_ match {
          case Task(Stop(Floor(_), _), s) if s.requestId == subscription.requestId =>
            s.exit()
            moving -= s
          case _ =>
        })
    })
    matched.nonEmpty
  }

  def accept(call: Call, subscription: InternalSubscription): Unit = {
    val f = call.floor.level - minFloor
    plan.update(f, plan(f) + Task(call, subscription))
    waiting += subscription
  }

  def accept(stop: Stop, subscription: InternalSubscription): Unit = {
    if (moving(subscription)) {
      val f = stop.floor.level - minFloor
      plan.update(f, plan(f) + Task(stop, subscription))
    } else {
      throw new IllegalStateException("Can't request stop before entering the elevator")
    }
  }

  def cancel(subscription: Subscription): Boolean = {
    val sizeBefore = plan.flatten.size
    plan.zipWithIndex.foreach {
      case (p, idx) =>
        plan.update(idx, p.filter(_.subscription != subscription))
    }
    val sizeAfter = plan.flatten.size
    sizeBefore != sizeAfter
  }

  def move(): Unit = currentStatus match {
    case Waiting | Stopped => ()
    case Moving(Up) if currentFloor < maxFloor =>
      currentFloor += 1
    case Moving(Down) if currentFloor > minFloor =>
      currentFloor -= 1
    case _ =>
      throw new IllegalStateException(s"Invalid move: status=${currentStatus} floor=${currentFloor}")
  }

  def fire(): Unit = {
    val idx = currentFloor - minFloor
    val set = plan(idx)
    val (stops, calls) =  set.partition({
      case Task(Stop(_, _), _) => true
      case Task(Call(_, _), _) => false
    })
    stops.map(_.subscription).foreach(_.stop())
    val picks =
      if (currentFloor == minFloor || currentFloor == maxFloor) calls
      else {
        currentStatus match {
          case Moving(Up) =>
            calls
              .filter(_ match {
                case Task(Call(_, Up), _) => true
                case _ => false
              })
          case Moving(Down) =>
            calls
              .filter(_ match {
                case Task(Call(_, Down), _) => true
                case _ => false
              })
          case _ => calls
        }
      }
    plan.update(idx, plan(idx) -- stops)
    picks.map(_.subscription).foreach(_.pick())
    plan.update(idx, plan(idx) -- stops -- picks)

    waiting.foreach(_.tick())
    moving.foreach(_.move())
  }

  def check(): Unit = {
    val idx = currentFloor - minFloor
    val tasks = plan.flatten
    if (tasks.isEmpty) {
      currentStatus = Waiting
    } else {
      val here = plan.flatten.toSet.filter(_ match {
        case Task(Call(Floor(f), _), _) => f == currentFloor
        case Task(Stop(Floor(f), _), _) => f == currentFloor
        case _ => false
      })
      val above = plan.drop(currentFloor - minFloor).flatten.toSet -- here
      val below = plan.take(currentFloor - minFloor).flatten.toSet -- here
      val newStatus = currentStatus match {
        case Waiting if above.isEmpty && below.isEmpty => Waiting
        case Waiting if above.nonEmpty => Moving(Up)
        case Waiting if below.nonEmpty => Moving(Down)
        case Moving(Up) if above.isEmpty && below.nonEmpty => Moving(Down)
        case Moving(Down) if below.isEmpty && above.nonEmpty => Moving(Up)
        case keep => keep
      }
      currentStatus = newStatus
    }
  }

  def hasTasks(): Boolean = plan.exists(_.nonEmpty)
  def nonEmpty(): Boolean = waiting.nonEmpty || moving.nonEmpty

  def getQueueSize(): Int = waiting.size + moving.size

  def report(reporter: InternalElevator.Reporter): String = {
    reporter.report(id, currentFloor, currentStatus, plan.toList)
  }
}

object InternalElevator {
  trait Reporter {
    def report(id: Int, floor: Int, status: ElevatorControls.Status, queue: List[Set[Task]]): String
  }

  object ConsoleReporter extends Reporter {
    override def report(id: Int, floor: Int, status: Status, queue: List[Set[Task]]): String = {
      val statusString = status match {
        case Moving(Up) => "up"
        case Moving(Down) => "down"
        case Waiting => "wait"
        case Stopped =>  "stop"
      }
      val queueString = queue.zipWithIndex.map {
        case (set, idx) =>
          val str = set.map({
            case Task(Call(Floor(f), Direction.Up), s) => s"(${s.requestId}) ${f} up"
            case Task(Call(Floor(f), Direction.Down), s) => s"(${s.requestId}) ${f} down"
            case Task(Stop(Floor(f), _), s) => s"(${s.requestId}) ${f} stop"
          }).mkString("[", ",", "]")
          s"$idx: $str"
      }
      s"Elevator: id=$id floor=$floor status=$statusString queue=$queueString"
    }
  }
}
