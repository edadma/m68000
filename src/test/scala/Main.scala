package xyz.hyperreal.m68k


object Main extends App {
  val mem = new Memory {
    def init: Unit = {
      regions.clear
      add( new ROM("code", 0, 0xFFFF) )
      add( new RAM("ram", 0x10000, 0x1FFFF) )
      add( new StdIOChar(0x20000) )
    }
  }
  val cpu = new CPU( mem )

  cpu.reset
  cpu.run
}