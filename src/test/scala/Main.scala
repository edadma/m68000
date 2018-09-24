//@
package xyz.hyperreal.m68k

import java.io.File
import java.time.LocalTime
import java.time.temporal.ChronoField


object Main extends App {
  val mem =
    new Memory {
      def init: Unit = {
        regions.clear
        add( new RAM("ram", 0x10000, 0x1FFFF) )
        SREC( this, new File("tools/strcmp.srec") )
      }
    }
  val cpu =
    new CPUWithServices( mem ) {
//      trace = true
      symbols = MapFileReader( io.Source.fromFile("tools/strcmp.map") )._2
    }

  cpu.reset
  cpu.run
}