package xyz.hyperreal.m68k

import java.io.File


object Run {

  def apply( file: String ) =
    capture {
      val cpu =
        new CPU(
          new Memory {
            def init: Unit = {
              regions.clear
              add( new RAM("ram", 0x10000, 0x1FFFF) )
              SREC( this, new File(s"$file.srec") )
            }
          } )

      cpu.run
    }

}
