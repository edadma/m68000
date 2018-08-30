package xyz.hyperreal.m68k


object Main extends App {

  println( Hex(
    """
      |0002: 1324 //asdf
      |
      |0008:
      |35 46
      |//qwer
    """.stripMargin
  ) map (_.toHexString) )

}