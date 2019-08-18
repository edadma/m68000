//@
package xyz.hyperreal.m68k

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.LazyList._


object Hex {

  val hexDigit = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') toSet

  def apply( src: io.Source ): List[Byte] = apply( src.getLines )

  def apply( src: String ): List[Byte] = apply( src.split("\n").toList.iterator )

  def apply( src: Iterator[String] ) = {
    def skipline( s: LazyList[Char] ): LazyList[Char] =
      s match {
        case LazyList() => s
        case '\n' #:: t => t
        case h #:: t => skipline( t )
      }

    val data = new ArrayBuffer[Byte]

    for ((line, i) <- src zipWithIndex) {
      def chars( s: LazyList[Char] ): Unit =
        s match {
          case LazyList() =>
          case (' ' | '\t') #:: t => chars( t )
          case d1 #:: d2 #:: d3 #:: d4 #:: ':' #:: t if hexDigit(d1) && hexDigit(d2) && hexDigit(d3) && hexDigit(d4) =>
            val addr = Integer.parseInt( d1.toString + d2 + d3 + d4, 16 )

            if (addr < data.length)
              sys.error( s"error on line ${i + 1}: $addr is below current size" )

            data ++= Iterator.fill( addr - data.length )( 0 )
            chars( t )
          case '/' #:: '/' #:: t => chars( skipline(t) )
          case d1 #:: d2 #:: t if hexDigit(d1) && hexDigit(d2) =>
            data += Integer.parseInt( s"$d1$d2", 16 ).asInstanceOf[Byte]
            chars( t )
          case _ => sys.error( s"error on line ${i + 1}: $line" )
        }

      chars( LazyList from line )
    }

    data.toList
  }

}
