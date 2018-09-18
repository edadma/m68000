package xyz.hyperreal.m68k

import scala.collection.mutable.HashMap


object MapFileReader extends App {

  println( apply(io.Source.fromFile("test/main.map")) )

  val Label = " {16}([^ ]) {16}([^ ]+).*"r

  def apply( src: io.Source ): Map[String, Long] = {
    val map = new HashMap[String, Long]
    var started = false

    for (line <- src.getLines) {
      if (started) {
        if (line.nonEmpty) {
          if (!line.contains( ' ' ))
            return map.toMap
          else {
            line match {
              case Label( address, label )
            }
          }
        }
      } else if (line == "Linker script and memory map")
        started = true
    }

    sys.error( "error reading map file" )
  }

}