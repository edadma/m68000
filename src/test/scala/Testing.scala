package xyz.hyperreal.m68k


trait Testing {

  def test( test: CPU => Int ): (Int, String) = {
    val mem =
      new Memory {
        def init: Unit = {
          regions.clear
          add( new RAM("ram", 0x10000, 0x1FFFF) )
          add( ROM("rom", "0000 0000  0000 0000") )
        }
      }
    val cpu = new CPU( mem )

    (test( cpu ), (if (cpu.X) "x" else "") + (if (cpu.N) "n" else "") + (if (cpu.Z) "z" else "") + (if (cpu.V) "v" else "") + (if (cpu.C) "c" else ""))
  }

}

//    for (s <- results) {
//      val r = s.name.toLowerCase
//
//      if (r startsWith "d")
//        cpu.D
//    }
