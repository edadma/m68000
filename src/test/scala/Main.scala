//@
package xyz.hyperreal.m68k

import java.io.{File, PrintStream}


object Main extends App {
  val program = "main"
  val mem =
    new Memory {
      def init: Unit = {
        regions.clear
        add( new RAM("ram", 0x10000, 0x10000 + 2*1024*1024 - 1) )
        SREC( this, new File(s"tools/$program.srec") )
      }
    }
  val cpu =
    new CPUWithServices( mem ) {
//      trace = true
      traceout = new PrintStream( "trace" )
      symbols = MapFileReader( io.Source.fromFile(s"tools/$program.map") )._2
      debug = DebugFileReader( io.Source.fromFile(s"tools/$program.debug") )
    }

  cpu.reset
  cpu.run
//  cpu.traceout.close
}