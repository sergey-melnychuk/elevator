import edu.elevator.system.ElevatorSystem

object Main extends App {

  import edu.elevator.Contract._

  val MinFloor = -2
  val MaxFloor = 10
  val NumberOfElevators = 1
  val system = ElevatorSystem(MinFloor, MaxFloor, NumberOfElevators)

  val goto = (tag: String, target: Floor) => {
    import edu.elevator.Contract.Event._
    var waits = 0
    var moves = 0
    (event: Event, elevator: Option[ElevatorControls]) =>
      event match {
        case Wait =>
          waits += 1
          println(tag + ": " + "waiting... elevator is on floor " + elevator.map(_.floor).getOrElse("?"))
          elevator.foreach(_.push(Floor(-100)))
        case Pick =>
          println(tag + ": " + "get into elevator on floor " + elevator.map(_.floor).getOrElse("?") + " and push " + target)
          elevator.foreach(_.enter())
          elevator.foreach(_.push(Floor(-100)))
        case Move =>
          moves += 1
          if (moves > 20) elevator.foreach(_.push(target))
          println(tag + ": " + "riding in the elevator on floor " + elevator.map(_.floor).getOrElse("?"))
        case Stop =>
          println(tag + ": " + "stopped on my request at floor " + elevator.map(_.floor).getOrElse("?"))
          elevator.foreach(_.leave())
        case Exit =>
          println(tag + ": " + "leaving elevator on floor " + elevator.map(_.floor).getOrElse("?") + ", summary: waits=" + waits + " moves=" + moves)
      }
    }

  system.call(Floor(MinFloor), Direction.Up).onStateChange(goto("001", Floor(MaxFloor)))
  system.call(Floor(MaxFloor), Direction.Down).onStateChange(goto("002", Floor(MinFloor)))
  system.call(Floor(MinFloor+2), Direction.Up).onStateChange(goto("003", Floor(MaxFloor-2)))
  system.call(Floor(MinFloor+7), Direction.Down).onStateChange(goto("004", Floor(MaxFloor-7)))

  system.start
  system.stop

}
