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

  def width( size: Size, aligned: Boolean ) =
    size match {
      case BitSize|ByteSize if aligned => 2
      case BitSize|ByteSize => 1
      case ShortSize => 2
      case IntSize => 4
    }

  def fromBCD( bcd: Int ) = (bcd >> 4)*10 + (bcd & 0x0F)

  def toBCD( n: Int ) = ((n/10) << 4) | (n%10)

  def bits( size: Size ) =
    size match {
      case ByteSize => 8
      case ShortSize => 16
      case IntSize => 32
    }

  def bit( cond: Boolean, n: Int ) = if (cond) 1<<n else 0

  def testBit( data: Int, bit: Int ) = (data&(1<<bit)) != 0

  def flipBit( data: Int, bit: Int ) = data^(1<<bit)

  def setBit( data: Int, bit: Int ) = data|(1<<bit)

  def clearBit( data: Int, bit: Int ) = data&(~(1<<bit))

  def cast( v: Int, size: Size ) =
    size match {
      case BitSize|ByteSize => v.asInstanceOf[Byte].asInstanceOf[Int]
      case ShortSize => v.asInstanceOf[Short].asInstanceOf[Int]
      case IntSize => v
    }

  def ones( a: Int ) = (for (i <- 0 until a) yield 1<<i) reduce (_ | _)

  def mnemonic( sym: String ) = s"$sym.${" "*(6 - sym.length)} "

  def mnemonic( sym: String, size: Size ) = {
    val s =
      size match {
        case ByteSize => "B"
        case ShortSize => "W"
        case IntSize => "L"
      }

    s"$sym.$s${" "*(4 - sym.length)} "
  }

}
