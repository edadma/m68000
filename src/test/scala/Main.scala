package xyz.hyperreal.m68k


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

      override def trap( vector: Int ) =
        vector match {
          case 15 =>
            D(0) match {
              case 6 => print( D(1).toChar )
              case _ => sys.error( s"unknown task number: ${D(0)}" )
            }

            true
          case _ => false
        }
    }

  cpu.reset
  cpu.run
}