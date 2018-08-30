//@
package xyz.hyperreal.m68k

import scala.collection.mutable.ArrayBuffer


object Hex {

  val hexDigit = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') toSet

  def apply( src: io.Source ) = apply( src.getLines )

  def apply( src: String ) = apply( src.lines )

  def apply( src: Iterator[String] ) = {
    def skipline( s: Stream[Char] ): Stream[Char] =
      s match {
        case Stream.empty => s
        case '\n' #:: t => t
        case h #:: t => skipline( t )
      }

    val data = new ArrayBuffer[Byte]

    for ((line, i) <- src zipWithIndex) {
      def chars( s: Stream[Char] ): Unit =
        s match {
          case (' ' | '\t') #:: t => chars( t )
          case '/' #:: '/' #:: t => chars( skipline(t) )
          case d1 #:: d2 #:: t if hexDigit(d1) && hexDigit(d2) =>
            data += Integer.parseInt( d1 + d2.toString, 16 ).asInstanceOf[Byte]
            chars( t )
          case _ => sys.error( s"error on line ${i + 1}: $line" )
        }

      chars( line toStream )
    }

    data.toList
  }

}
