package edu.elevator.system

import edu.elevator.Contract.Direction.{Down, Up}
import edu.elevator.Contract.Elevator._
import edu.elevator.Contract.{Direction, Elevator, Floor, Subscription}
import edu.elevator.system.InternalContract.{Call, Stop, Task}

case class InnerElevator(id: Int, dispatcher: Dispatcher, minFloor: Int, maxFloor: Int, startFloor: Int, startStatus: Status) extends Elevator {
  private val plan: Array[Set[Task]] = Array.fill(dispatcher.numberOfFloors){Set.empty[Task]}
  private var currentFloor: Int = startFloor
  private var currentStatus: Status = startStatus

  override def floor: Floor = Floor(currentFloor)
  override def status: Status = currentStatus
  override def push(floor: Floor): Unit = ()

  def accept(call: Call, subscription: InnerSubscription): Unit = {
    val f = call.floor.level - minFloor
    plan.update(f, plan(f) + Task(call, subscription))
  }

  def accept(stop: Stop, subscription: InnerSubscription): Unit = {
    val f = stop.floor.level - minFloor
    plan.update(f, plan(f) + Task(stop, subscription))
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
    stops.map(_.subscription).foreach(_.exit())
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
    picks.map(_.subscription).foreach(_.pick())
    plan.update(idx, plan(idx) -- stops -- picks)

    val all = plan.flatten.toSet -- stops -- picks
    all.foreach(_ match {
      case Task(Stop(_, _), s) => s.move()
      case Task(Call(_, _), s) => s.tick()
    })
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

  def report(reporter: InnerElevator.Reporter): Unit = {
    reporter.report(id, currentFloor, currentStatus, plan.toList)
  }
}

object InnerElevator {
  trait Reporter {
    def report(id: Int, floor: Int, status: Elevator.Status, queue: List[Set[Task]]): Unit
  }

  object ConsoleReporter extends Reporter {
    override def report(id: Int, floor: Int, status: Status, queue: List[Set[Task]]): Unit = {
      val statusString = status match {
        case Moving(Up) => "up"
        case Moving(Down) => "down"
        case Waiting => "wait"
        case Stopped =>  "stop"
      }
      val queueString = queue.zipWithIndex.map {
        case (set, idx) =>
          val str = set.map({
            case Task(Call(Floor(f), Direction.Up), s) => s"U_${f}_${s.requestId}"
            case Task(Call(Floor(f), Direction.Down), s) => s"D_${f}_${s.requestId}"
            case Task(Stop(Floor(f), _), s) => s"S_${f}_${s.requestId}"
          }).mkString("[", ",", "]")
          s"$idx: $str"
      }
      println(s"Elevator: id=$id floor=$floor status=$statusString queue=$queueString")
    }
  }
}
