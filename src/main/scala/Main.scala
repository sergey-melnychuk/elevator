import edu.elevator.system.ElevatorSystem

object Main extends App {

  import edu.elevator.Contract._
  import edu.elevator.Contract.Event._

  val MinFloor = -2
  val MaxFloor = 10
  val NumberOfElevators = 1
  val system = ElevatorSystem(MinFloor, MaxFloor, NumberOfElevators)

  val goto = (tag: String, target: Floor) => {
    var waits = 0
    var moves = 0
    (event: Event, elevator: Elevator) =>
      event match {
        case Wait =>
          waits += 1
        //println(tag + ": " + "waiting... elevator is on floor " + elevator.floor)
        case Pick =>
          println(tag + ": " + "get into elevator on floor " + elevator.floor + " and push " + target)
          elevator.push(target)
        case Move =>
          moves += 1
        //println(tag + ": " + "riding in the elevator on floor " + elevator.floor)
        case Exit =>
          println(tag + ": " + "leaving elevator on floor " + elevator.floor)
          println(tag + ": " + "summary: waits=" + waits + " moves=" + moves)
        case _ =>
      }
    }

  system.call(Floor(MinFloor), Direction.Up).onStateChange(goto("001", Floor(MaxFloor)))
  system.call(Floor(MaxFloor), Direction.Down).onStateChange(goto("002", Floor(MinFloor)))
  system.call(Floor(0), Direction.Up).onStateChange(goto("003", Floor(5)))
  system.call(Floor(7), Direction.Down).onStateChange(goto("004", Floor(2)))

  system.start
  system.stop

}
