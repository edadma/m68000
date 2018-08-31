//@
package xyz.hyperreal.m68k

import java.time.LocalTime
import java.time.temporal.ChronoField


object Main extends App {
  val mem =
    new Memory {
      def init: Unit = {
        regions.clear
        add( ROM("code",
          """
            |00020000 // SSP
            |00000400 // start
            |
            |0400:
            |  123C 0041                 //      move.b #'A', D1
            |  303C 0006                 //      move #6, D0
            |  4E4F                      //      trap #15
            |  FFFF FFFF                 //      SIMHALT             ; halt simulator
          """.stripMargin) )
        add( new RAM("ram", 0x10000, 0x1FFFF) )
        add( new StdIOChar(0x20000) )
      }
    }
  val cpu =
    new CPU( mem ) {
      override def lineF = {
        halt
        true
      }

      override def trap( vector: Int ) = {
        def prt =
          for (c <- A(1) to A(1) + D(1)&0xFFFF)
            print( memoryRead(c, ByteSize, false).toChar )

        def prtn = {
          def prtn( addr: Long ): Unit =
            memoryRead( addr, ByteSize, false ) match {
              case 0 =>
              case c =>
                print( c.toChar )
                prtn( addr + 1 )
            }

          prtn( A(1) )
        }

        vector match {
          case 15 =>
            D(0) match {
              case 0 =>
                prt
                println
              case 1 => prt
              case 2 =>
                val line = io.StdIn.readLine

                for ((c, i) <- line zipWithIndex)
                  memoryWrite( c, A(1) + i, ByteSize, false )

                memoryWrite( 0, A(1) + line.length, ByteSize, false )
                D(1) = line.length
              case 3 => print( D(1) )
              case 4 => D(1) = io.StdIn.readInt
              case 5 => D(1) = io.StdIn.readChar
              case 6 => print( D(1).toChar )
              case 8 => D(1) = LocalTime.now.get( ChronoField.MILLI_OF_DAY )/10
              case 9 => halt
              case 13 =>
                prtn
                println
              case 14 => prtn
              case _ => sys.error( s"unknown task number: ${D(0)}" )
            }

            true
          case _ => false
        }
      }
    }

  cpu.reset
  cpu.run
}