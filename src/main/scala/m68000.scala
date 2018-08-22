//@
package xyz.hyperreal
import java.io.ByteArrayOutputStream


package object m68k {

	def boolean2int( b: Boolean ) = if (b) 1 else 0

  def dtol( d: Double ) = java.lang.Double.doubleToLongBits( d )

  def ltod( l: Long ) = java.lang.Double.longBitsToDouble( l )

  def hexByte( a: Int ) = "%02x".format( a&0xFF ).toUpperCase

  def hexShort( a: Int ) = hexByte( a>>8 ) + hexByte( a )

  def hexInt( a: Int ) = hexShort( a>>16 ) + hexShort( a )

  def hexLong( a: Long ) = hexInt( (a>>32).asInstanceOf[Int] ) + hexInt( a.asInstanceOf[Int] )

  def isHex( s: String ) = !s.isEmpty && s.forall( c => "0123456789abcdefABCDEF" contains c )

  def hex( s: String ) = Integer.parseInt( s, 16 )

  def ulong( v: Long ) =
    if (v < 0)
      BigInt( v&0x7FFFFFFFFFFFFFFFL ).setBit( 63 )
    else
      BigInt( v )

	def capture( code: => Unit ) = {
		val out = new ByteArrayOutputStream

		Console.withOut( out )( code )
		out.toString.trim
	}

}
