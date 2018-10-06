//@
package xyz.hyperreal.m68k

import java.io.{File, PrintStream}


object Main extends App {
  val program = "main"
  val timer = new dev.Timer( "interrupt", 1, 0xFFFFFF00 )
  val mem =
    new Memory {
      def init: Unit = {
        regions.clear
        add( new RAM("ram", 0x10000, 0x10000 + 2*1024*1024 - 1) )
        SREC( this, new File(s"tools/$program.srec") )
        add( timer )
      }
    }
  val cpu =
    new CPUWithServices( mem ) {
      trace = true
//      tracelimit = 50
      traceout = new PrintStream("trace")
      symbols = MapFileReader(io.Source.fromFile(s"tools/$program.map"))._2
      debug = DebugFileReader(io.Source.fromFile(s"tools/$program.debug"))
    }

  timer connect cpu
  cpu.reset
  cpu.run
  cpu.resetSignal
  if (cpu.traceout != Console.out) cpu.traceout.close
}