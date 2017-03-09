package edu.elevator.system

import edu.elevator.system.InternalContract.Call

private [system]
object Router {
  object ShortestQueue extends InternalContract.Router {
    override def route(call: Call, elevators: Array[InternalElevator]): Int = {
      val idx = elevators.zipWithIndex.sortBy(_._1.getQueueSize()).head._2
      println(s"call=${call} routed to elevator=${idx} ${elevators(idx).report(InternalElevator.ConsoleReporter)}")
      idx
    }
  }
}
