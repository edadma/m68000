package xyz.hyperreal.m68k

import scala.collection.mutable.HashMap

import java.lang.Integer.parseInt


object DebugFileReader extends App {

  println( apply(io.Source.fromFile("tools/main.debug")) )

  lazy val codeRegex = """ */\* file ([^ ]+) line ([^ ]+) addr 0x([^ ]+) \*/"""r
  lazy val varRegex = """^(?:[^ ]| *static).+?([a-zA-Z_][a-zA-Z0-9_]*)[^ ]* /\* 0x([^ ]+) \*/;"""r

  def apply( src: io.Source ) = {
    val code = new HashMap[Int, (String, String)]
    val vars = new HashMap[Int, String]

    for (line <- src.getLines)
      line match {
        case codeRegex( file, lineno, address ) => code(parseInt( address, 16 )) = (lineno, file)
        case varRegex( name, address ) => vars(parseInt( address, 16 )) = name
        case _ =>
      }

    (code.toMap, vars.toMap)
  }

}