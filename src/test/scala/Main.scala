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
//        addHexdump( io.Source.fromFile("test/main.hex") )
        add( new RAM("ram", 0x10000, 0x1FFFF) )
        SREC( this, new File("test/main.srec") )
      }
    }
  val cpu =
    new CPU( mem ) {
//      trace = true

      override def illegal = {
        println( "illegal instruction" )
        halt
        true
      }

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
            D(0).toShort match {
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