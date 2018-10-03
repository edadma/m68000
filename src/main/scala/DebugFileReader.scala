package xyz.hyperreal.m68k

import scala.collection.mutable.HashMap


object DebugFileReader extends App {

  println( apply(io.Source.fromFile("tools/main.debug")) )

  lazy val lineRegex = """ */\* file ([^ ]+) line ([^ ]+) addr 0x([^ ]+) \*/"""r

  def apply( src: io.Source ): Map[Long, (String, String)] = {
    val code = new HashMap[Long, (String, String)]

    for (line <- src.getLines)
      line match {
        case lineRegex( file, lineno, address ) => code(java.lang.Long.parseLong( address, 16 )) = (lineno, file)
        case _ =>
      }

    code.toMap
  }

}