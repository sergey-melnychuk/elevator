package edu.elevator.system

import edu.elevator.Contract.{Direction, Floor}

private[system]
case class Request(id: Long, floor: Floor, direction: Direction)

object Request {
  def isValid(minFloor: Int, maxFloor: Int, request: Request): Boolean = (request.floor, request.direction) match {
    case (f, Direction.Up) if f.level == minFloor => true
    case (f, Direction.Down) if f.level == maxFloor => true
    case (f, _) if f.level > minFloor && f.level < maxFloor => true
    case _ => false
  }
}
