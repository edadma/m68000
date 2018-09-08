//@
package xyz.hyperreal.m68k

import scala.collection.mutable.{ListBuffer, PriorityQueue}


case class Interrupt( level: Int, vector: Option[Int] ) extends Ordered[Interrupt] {
  def compare( that: Interrupt ): Int = level - that.level
}

class CPU( private [m68k] val memory: Memory ) extends Addressing {

  private [m68k] val D = new Array[Int]( 8 )
  private [m68k] val A = new Array[Long]( 7 )
  private [m68k] var PC = 0L
  private [m68k] var USP = 0L
  private [m68k] var SSP = 0L
  private [m68k] var MSP = 0L
  private [m68k] var C = false
  private [m68k] var V = false
  private [m68k] var Z = false
  private [m68k] var N = false
  private [m68k] var X = false
  private [m68k] var SR = 0
  private [m68k] var instruction = 0
  private [m68k] var prog: Addressable = _

  private val interrupts = new PriorityQueue[Interrupt]
  private var interruptsAvailable = false

  var counter = 0L
  var trace = false

	protected [m68k] var running = false
  protected [m68k] var stopped = false

  private val opcodes = CPU.opcodeTable

  def interrupt( req: Interrupt ): Unit = synchronized {
    require( 1 <= req.level && req.level <= 7, s"interrupt level out of range: ${req.level}" )
    interrupts enqueue req
    interruptsAvailable = true
  }

  def service: Unit = synchronized {
    if (interrupts nonEmpty) {
      val req = interrupts.dequeue

      if (req.level == 7 || req.level > ((SR&SRBit.I)>>SRBit.I_shift))
        req match {
          case Interrupt( level, None ) => exception( VectorTable.autoVectors + level<<2 )
          case Interrupt( _, Some(vector) ) => exception( VectorTable.interruptVectors + vector<<2 )
        }
      else
        service
    }

    if (interrupts isEmpty)
      interruptsAvailable = false
  }

  def breakpoint( bkpt: Int ) = false

  def illegal = false

  def trap( vector: Int ) = false

  def lineA = false

  def lineF = false

  reset

  def jumpto(address: Long ): Unit = {
    prog = memory.find( address )
    PC = address
  }

  def supervisor =
    if ((SR&SRBit.S) == 0) {
      exception( VectorTable.privilegeViolation )
      false
    } else
      true

  def fromSR = SR | bit( X, CCR.X ) | bit( N, CCR.N ) | bit( Z, CCR.Z ) | bit( V, CCR.V ) | bit( C, CCR.C )

  def toSR( bits: Int ): Unit = {
    SR = bits&0xFF00
    toCCR( bits )
  }

  def toCCR( bits: Int ): Unit = {
    X = testBit( bits, CCR.X )
    N = testBit( bits, CCR.N )
    Z = testBit( bits, CCR.Z )
    V = testBit( bits, CCR.V )
    C = testBit( bits, CCR.C )
  }

	def isRunning = running

  def disassemble: Unit = {
//    if (memory.valid( pc )) {
//    val m = memory.find( pc )
//    val low = m.readByte( pc )
//    val (inst, disassembly) =
//      if ((low&3) == 3) {
//        val inst = m.readInt( pc, low )
//
//        (hexInt( inst ), opcodes32(inst&0x1FFFFFF).disassemble( this ))
//      } else {
//        val inst = m.readShort( pc, low )
//
//        (hexShort( inst ), opcodes16(inst).disassemble( this ))
//      }
//
//      printf( "%8x  %s  %s\n", pc, inst, disassembly )
//    } else
//      println( s"pc=${pc.toHexString}" )
  }

  def registers: Unit = {
//    def regs( start: Int ) {
//      for (i <- start until (start + 5 min 32))
//        printf( "%21s  ", s"${rx(i)}=${x(i).toHexString}" )
//
//      println
//    }
//
//    for (i <- 0 until 32 by 5)
//      regs( i )
  }

  def fregisters: Unit = {
//    def regs( start: Int ) {
//      for (i <- start until (start + 5 min 32))
//        printf( "%21s  ", s"${rf(i)}=${"%.2f".format(f(i))}" )
//
//      println
//    }
//
//    for (i <- 0 until 32 by 5)
//      regs( i )
  }

  def registersAll: Unit = {
    registers
    fregisters
  }

  def problem( error: String ) = {
    registers
    sys.error( s"error at ${PC.toHexString} (${"%08x".format(instruction)}): $error" )
  }

  def halt: Unit = {
    running = false
    stopped = false
  }

  def reset: Unit = {
    halt
    memory.reset
    resetSignal

    for (i <- 0 until 8)
      D(i) = 0

    for (i <- 0 until 7)
      A(i) = 0

    SR = SRBit.S|SRBit.I
    SSP = memoryReadAddress( VectorTable.SSP )
    jumpto( memoryReadAddress(VectorTable.PC) )
  }

  def resetSignal: Unit = {

  }

  def execute: Unit = {
    if (trace) {
      disassemble
      registers
    }

    instruction = fetchShort&0xFFFF
    opcodes(instruction)( this )
    counter += 1
  }

  def fetchByte = fetchShort.asInstanceOf[Byte].asInstanceOf[Int]

  def fetchShort = {
    val res = prog.readShort( PC )

    PC += 2
    res
  }

  def fetchInt = {
    val res = prog.readInt( PC )

    PC += 4
    res
  }

	def step =
		if (running)
			sys.error( "already running" )
		else {
			running = true
			execute
			halt
		}

  def run: Unit = {
    running = true

    while (running) {
      if (interruptsAvailable)
        service

      if (stopped)
        Thread.sleep( 10 )
      else
        execute
    }
  }

//  memory.problem = problem

  //
  // addressing
  //

  def memoryReadAddress( address: Long ) = memory.readInt( address )&0xFFFFFFFFL

  def memoryRead( address: Long, size: Size, aligned: Boolean ) =
    size match {
      case BitSize|ByteSize if aligned => memory.readShort( address ).asInstanceOf[Byte].asInstanceOf[Int]
      case BitSize|ByteSize => memory.readByte( address )
      case ShortSize => memory.readShort( address )
      case IntSize => memory.readInt( address )
    }

  def memoryWrite( data: Int, address: Long, size: Size, aligned: Boolean ) =
    size match {
      case BitSize|ByteSize if aligned => memory.writeShort( address, data )
      case BitSize|ByteSize => memory.writeByte( address, data )
      case ShortSize => memory.writeShort( address, data )
      case IntSize => memory.writeInt( address, data )
    }

  def readA( reg: Int ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 => MSP
      case 7 if (SR&SRBit.S) != 0 => SSP
      case 7 => USP
      case _ => A(reg)
    }

  def readAPredecrement( reg: Int, size: Size ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 =>
        MSP -= width( size, true )
        MSP
      case 7 if (SR&SRBit.S) != 0 =>
        SSP -= width( size, true )
        SSP
      case 7 =>
        USP -= width( size, true )
        USP
      case _ =>
        A(reg) -= width( size, false )
        A(reg)
    }

  def readAPostincrement( reg: Int, size: Size ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 =>
        val res = MSP

        MSP += width( size, true )
        res
      case 7 if (SR&SRBit.S) != 0 =>
        val res = SSP

        SSP += width( size, true )
        res
      case 7 =>
        val res = USP

        USP += width( size, true )
        res
      case _ =>
        val res = A(reg)

        A(reg) += width( size, false )
        res
    }

  def writeA( data: Long, reg: Int ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 => MSP = data
      case 7 if (SR&SRBit.S) != 0 => SSP = data
      case 7 => USP = data
      case _ => A(reg) = data
    }

  def readD( reg: Int, size: Size ) = cast( D(reg), size )

  def address( mode: Int, reg: Int ) =
    mode match {
      case AddressRegisterIndirect => readA( reg )
    }

  def read( mode: Int, reg: Int, size: Size ) = {
    mode match {
      case DataRegisterDirect if size == BitSize => D(reg)
      case DataRegisterDirect => readD( reg, size )
      case AddressRegisterDirect => cast( readA(reg).asInstanceOf[Int], size )
      case AddressRegisterIndirect => memoryRead( readA(reg), size, false )
      case AddressRegisterIndirectPostincrement => memoryRead( readAPostincrement(reg, size), size, reg == 7 )
      case AddressRegisterIndirectPredecrement => memoryRead( readAPredecrement(reg, size), size, reg == 7 )
      case OtherModes =>
        reg match {
          case ImmediateData => immediate( size )
        }
    }
  }

  def write( data: Int, mode: Int, reg: Int, size: Size ) {
    mode match {
      case DataRegisterDirect => writeD( data, reg, size )
      case AddressRegisterDirect => writeA( regwrite(data, readA(reg).asInstanceOf[Int], size)&0xFFFFFFFFL, reg )
      case AddressRegisterIndirect => memoryWrite( data, readA(reg), size, false )
      case AddressRegisterIndirectPostincrement => memoryWrite( data, readAPostincrement(reg, size), size, reg == 7 )
      case AddressRegisterIndirectPredecrement => memoryWrite( data, readAPredecrement(reg, size), size, reg == 7 )
      case OtherModes =>
      //        reg match {
      //        }
    }
  }

  def readWrite( mode: Int, reg: Int, size: Size )( op: Int => Int ) = {
    mode match {
      case DataRegisterDirect|AddressRegisterDirect|AddressRegisterIndirect => write( op(read(mode, reg, size)), mode, reg, size )
      case AddressRegisterIndirectPostincrement =>
        val addr = readAPostincrement(reg, size)

        memoryWrite( op(memoryRead(addr, size, reg == 7)), addr, size, reg == 7 )
      case AddressRegisterIndirectPredecrement =>
        val addr = readAPredecrement(reg, size)

        memoryWrite( op(memoryRead(addr, size, reg == 7)), addr, size, reg == 7 )
      case OtherModes =>
    }
  }

  def regwrite( data: Int, regcur: Int, size: Size ) =
    size match {
      case ByteSize => regcur&0xFFFFFF00 | data&0xFF
      case ShortSize => regcur&0xFFFF0000 | data&0xFFFF
      case IntSize => data
    }

  def writeD( data: Int, reg: Int, size: Size ) = D(reg) = regwrite( data, D(reg), size )

  def push( data: Int, size: Size ) = memoryWrite( data, readAPredecrement(7, size), size, true )

  def pushAddress( address: Long ) = push( address.asInstanceOf[Int], IntSize )

  def pop( size: Size ) = memoryRead( readAPostincrement(7, size), size, true )

  def popAddress = pop( IntSize )&0xFFFFFFFFL

  //
  // ALU
  //

  def flags( overflow: Int, carry: Int, extended: Boolean, res: Int, x: Boolean ): Int =
    flags( (overflow&0x80000000L) != 0, carry, extended, res, x )

  def flags( overflow: Boolean, carry: Int, extended: Boolean, res: Int, x: Boolean ) = {
    V = overflow
    C = (carry&0x80000000L) != 0

    if (x)
      X = C

    Z = if (extended) res == 0 && Z else res == 0
    N = res < 0
    res
  }

  def abcd( s: Int, d: Int ) = {
    val (r, c) =
      fromBCD( s ) + fromBCD( d ) match {
        case s if s > 99 => (s - 100, true)
        case s => (s, false)
      }

    C = c
    X = c

    if (r != 0)
      Z = false

    toBCD( r )
  }

  def sbcd( s: Int, d: Int ) = {
    val (r, c) =
      fromBCD( d ) - fromBCD( s ) match {
        case s if s < 0 => (100 + s, true)
        case s => (s, false)
      }

    C = c
    X = c

    if (r != 0)
      Z = false

    toBCD( r )
  }

  def add( s: Int, d: Int, extended: Boolean ) = {
    val r = s + d

    flags( s&d&(~r)|(~s)&(~d)&r, s&d|(~r)&d|s&(~r), extended, r, true )
  }

  def asl( r: Int, d: Int, size: Size ) =
    if (r == 0) {
      flags( 0, 0, false, d, false )
    } else {
      val res = d.toLong << r
      val mask = ones( r )
      val shifted = (res >> bits( size )).toInt & mask

      flags( shifted == 0 || shifted == mask, ~(d - r + 1), false, res.toInt, false )
    }

  def asr( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d << (bits(size) - r), false, d >> r, false )

  def lsr( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d << (bits(size) - r), false, d >>> r, false )

  def lsl( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d - r + 1, false, d << r, false )

  def rol( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d - r + 1, false, (d << r) | (d >>> (bits(size) - r)), false )

  def ror( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d << (bits(size) - r), false, (d >>> r) | (d << (bits(size) - r)), false )

  def roxl( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d - r + 1, false, (d << r) | (d >>> (bits(size) - r)), false )

  def roxr( r: Int, d: Int, size: Size ) =
    if (r == 0)
      flags( 0, 0, false, d, false )
    else
      flags( 0, d << (bits(size) - r), false, (d >>> r) | (d << (bits(size) - r)), false )

  def and( s: Int, d: Int ) = {
    flags( 0, 0, false, s & d, false )
  }

  def eor( s: Int, d: Int ) = {
    flags( 0, 0, false, s ^ d, false )
  }

  def or( s: Int, d: Int ) = {
    flags( 0, 0, false, s | d, false )
  }

  def subtract( s: Int, d: Int, extended: Boolean ) = {
    val r = d - s

    flags( (~s)&d&(~r) | s&(~d)&r, s&(~d) | r&(~d) | s&r, extended, r, true )
  }

  def neg( d: Int, extended: Boolean ) = {
    val r = -d - (if (extended) 1 else 0)

    flags( d&r, d|r, extended, r, true )
  }

  def immediate( size: Size ) =
    size match {
      case ByteSize => fetchByte
      case ShortSize => fetchShort
      case IntSize => fetchInt
    }

  def testcc( cond: Int ) =
    cond match {
      case Conditional.True => true
      case Conditional.False => false
      case Conditional.High => !C && !Z
      case Conditional.LowSame => C || Z
      case Conditional.CarryClear => !C
      case Conditional.CarrySet => C
      case Conditional.NotEqual => !Z
      case Conditional.Equal => Z
      case Conditional.OverflowClear => !V
      case Conditional.OverflowSet => V
      case Conditional.Plus => !N
      case Conditional.Minus => N
      case Conditional.GreaterEqual => N && V || !N && !V
      case Conditional.LessThan => N && !V || !N && V
      case Conditional.GreaterThan => N && V && !Z || !N && !V && !Z
      case Conditional.LessEqual => Z || N && !V || !N && V
    }

  def exception( vector: Int ): Unit = {
    push( fromSR, ShortSize )
    SR |= SRBit.S
    SR &= ~SRBit.T
    pushAddress( PC )
    jumpto( memoryRead(vector, IntSize, false) )
  }

}

object CPU {

  private val opcodes = Array.fill[Instruction]( 0x10000 )( ILLEGAL )
  private var built = false

  private def populate( pattern: String, inst: Map[Char, Int] => Instruction ) =
    for ((idx, m) <- generate( pattern ))
      opcodes(idx) = inst( m )

  private def populate( insts: List[(String, Map[Char, Int] => Instruction)] ): Unit =
    for ((p, c) <- insts)
      populate( p, c )

  def addqsize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 0 => ByteSize
      case 1 => ShortSize
      case 2 => IntSize
    }

  def addasize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 3 => ShortSize
      case 7 => IntSize
    }

  def chksize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 3 => ShortSize
      case 2 => IntSize
    }

  def movesize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 1 => ByteSize
      case 3 => ShortSize
      case 2 => IntSize
    }

  def extsize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 2 => ByteSize
      case 3 => ShortSize
      case 7 => IntSize
    }

  def opcodeTable: IndexedSeq[Instruction] = synchronized {
    if (!built) {
      populate(
        List[(String, Map[Char, Int] => Instruction)](
          "1100 yyy 10000 r xxx" -> (o => new ABCD( o('y'), o('r'), o('x') )),
          "1101 rrr 0 ss eee aaa; s:0-2" -> (o => new ADD( o('r'), 0, addqsize(o), o('e'), o('a') )),
          "1101 rrr 1 ss eee aaa; s:0-2; e:2-7" -> (o => new ADD( o('r'), 1, addqsize(o), o('e'), o('a') )),
          "1101 rrr sss eee aaa; s:3,7" -> (o => new ADDA( o('r'), addasize(o), o('e'), o('a') )),
          "00000110 ss eee aaa; s:0-2; e:0-7-1" -> (o => new ADDI( addqsize(o), o('e'), o('a') )),
          "0101 ddd 0 ss eee aaa; s:0-2" -> (o => new ADDQ( o('d') + 1, addqsize(o), o('e'), o('a') )),
          "00000110 ss eee aaa; s:0-2" -> (o => new ADDI( addqsize(o), o('e'), o('a') )),
          "1100 rrr d ss eee aaa; s:0-2" -> (o => new AND( o('r'), o('d'), addqsize(o), o('e'), o('a') )),
          "00000010 ss eee aaa; s:0-2" -> (o => new ANDI( addqsize(o), o('e'), o('a') )),
          "0000001000111100" -> (_ => ANDItoCCR),
          "0000001001111100" -> (_ => ANDItoSR),
          "1110000 d 11 eee aaa; e:2-7" -> (o => new ASMem( o('d'), o('e'), o('a') )),
          "1110 ccc d ss i 00 rrr; s:0-2" -> (o => new ASReg( o('c'), o('d'), addqsize(o), o('i'), o('r') )),
          "0110 cccc dddddddd" -> (o => new Bcc( o('c'), o('d') )),
          "0000 rrr 101 eee aaa" -> (o => new BCHG( Some(o('r')), o('e'), o('a') )),
          "0000100001 eee aaa" -> (o => new BCHG( None, o('e'), o('a') )),
          "0000 rrr 110 eee aaa" -> (o => new BCLR( Some(o('r')), o('e'), o('a') )),
          "0000100010 eee aaa" -> (o => new BCLR( None, o('e'), o('a') )),
          "0100100001001 vvv" -> (o => new BKPT( o('v') )),
          "0000 rrr 111 eee aaa" -> (o => new BSET( Some(o('r')), o('e'), o('a') )),
          "0000100011 eee aaa" -> (o => new BSET( None, o('e'), o('a') )),
          "01100001 dddddddd" -> (o => new BSR( o('d') )),
          "0000 rrr 100 eee aaa" -> (o => new BTST( Some(o('r')), o('e'), o('a') )),
          "0000100000 eee aaa" -> (o => new BTST( None, o('e'), o('a') )),
          "0100 rrr ss 0 eee aaa; s:2,3" -> (o => new CHK( o('r'), chksize(o), o('e'), o('a') )),
          "01000010 ss eee aaa; s:0-2" -> (o => new CLR( addqsize(o), o('e'), o('a') )),
          "1011 rrr sss eee aaa; s:0-2" -> (o => new CMP( o('r'), addqsize(o), o('e'), o('a') )),
          "1011 rrr sss eee aaa; s:3,7" -> (o => new CMPA( o('r'), addasize(o), o('e'), o('a') )),
          "00001100 ss eee aaa; s:0-2" -> (o => new CMPI( addqsize(o), o('e'), o('a') )),
          "1011 xxx 1 ss 001 yyy; s:0-2" -> (o => new CMPM( addqsize(o), o('x'), o('y') )),
          "0101 cccc 11001 rrr" -> (o => new DBcc( o('c'), o('r') )),
          "1000 rrr 111 eee aaa; e:0-7-1" -> (o => new DIVS( o('r'), o('e'), o('a') )),
          "1000 rrr 011 eee aaa; e:0-7-1" -> (o => new DIVU( o('r'), o('e'), o('a') )),
          "1011 rrr 1 ss eee aaa; s:0-2; e:0-7-1" -> (o => new EOR( o('r'), addqsize(o), o('e'), o('a') )),
          "00001010 ss eee aaa; s:0-2" -> (o => new EORI( addqsize(o), o('e'), o('a') )),
          "0000101000111100" -> (_ => EORItoCCR),
          "0000101001111100" -> (_ => EORItoSR),
          "1100 xxx 1 ooooo yyy; o:8,9,17" -> (o => new EXG( o('x'), o('o'), o('y') )),
          "0100100 sss 000 rrr; s:2,3,7" -> (o => new EXT( extsize(o), o('r') )),
          "0100111011 eee aaa" -> (o => new JMP( o('e'), o('a') )),
          "0100111010 eee aaa" -> (o => new JSR( o('e'), o('a') )),
          "0100 rrr 111 eee aaa" -> (o => new LEA( o('r'), o('e'), o('a') )),
          "1010 xxxxxxxxxxxx" -> (_ => LINEA),
          "1111 xxxxxxxxxxxx" -> (_ => LINEF),
          "0100111001010 rrr" -> (o => new LINK( o('r') )),
          "1110001 d 11 eee aaa; e:2-7" -> (o => new LSMem( o('d'), o('e'), o('a') )),
          "1110 ccc d ss i 01 rrr; s:0-2" -> (o => new LSReg( o('c'), o('d'), addqsize(o), o('i'), o('r') )),
          "00 ss vvv uuu xxx yyy; s:1-3; u:0-7-1" -> (o => new MOVE( movesize(o), o('v'), o('u'), o('x'), o('y') )),
          "00 ss rrr 001 eee aaa; s:2,3" -> (o => new MOVEA( chksize(o), o('r'), o('e'), o('a') )),
          "0100000011 eee aaa; e:0-7-1" -> (o => new MOVEfromSR( o('e'), o('a') )),
          "0100010011 eee aaa; e:0-7-1" -> (o => new MOVEtoCCR( o('e'), o('a') )),
          "0100011011 eee aaa; e:0-7-1" -> (o => new MOVEtoSR( o('e'), o('a') )),
          "0111 rrr 0 dddddddd" -> (o => new MOVEQ( o('r'), o('d') )),
          "010011100110 d rrr" -> (o => new MOVEUSP( o('d'), o('r') )),
          "1100 rrr 111 eee aaa; e:0-7-1" -> (o => new MULS( o('r'), o('e'), o('a') )),
          "1100 rrr 011 eee aaa; e:0-7-1" -> (o => new MULU( o('r'), o('e'), o('a') )),
          "0100100000 eee aaa; e:0-7-1" -> (o => new NBCD( o('e'), o('a') )),
          "01000100 ss eee aaa; s:0-2; e:0-7-1" -> (o => new NEG( addqsize(o), o('e'), o('a') )),
          "01000000 ss eee aaa; s:0-2; e:0-7-1" -> (o => new NEGX( addqsize(o), o('e'), o('a') )),
          "0100111001110001" -> (_ => NOP),
          "01000110 ss eee aaa; s:0-2; e:0-7-1" -> (o => new NOT( addqsize(o), o('e'), o('a') )),
          "00000000 ss eee aaa; s:0-2" -> (o => new ORI( addqsize(o), o('e'), o('a') )),
          "0000000000111100" -> (_ => ORItoCCR),
          "0000000001111100" -> (_ => ORItoSR),
          "0100100001 eee aaa" -> (o => new PEA( o('e'), o('a') )),
          "0100111001110000" -> (_ => RESET),
          "1110011 d 11 eee aaa; e:2-7" -> (o => new ROMem( o('d'), o('e'), o('a') )),
          "1110 ccc d ss i 11 rrr; s:0-2" -> (o => new ROReg( o('c'), o('d'), addqsize(o), o('i'), o('r') )),
          "0100111001110011" -> (_ => RTE),
          "0100111001110111" -> (_ => RTR),
          "0100111001110101" -> (_ => RTS),
          "1000 yyy 10000 r xxx" -> (o => new SBCD( o('y'), o('r'), o('x') )),
          "0100111001110001" -> (_ => STOP),
          "1001 rrr 0 ss eee aaa; s:0-2" -> (o => new SUB( o('r'), 0, addqsize(o), o('e'), o('a') )),
          "1001 rrr 1 ss eee aaa; s:0-2; e:2-7" -> (o => new SUB( o('r'), 1, addqsize(o), o('e'), o('a') )),
          "0100100001000 rrr" -> (o => new SWAP( o('r') )),
          "0100101011 eee aaa" -> (o => new TAS( o('e'), o('a') )),
          "010011100100 vvvv" -> (o => new TRAP( o('v') )),
          "0100111001110110" -> (_ => TRAPV),
          "01001010 ss eee aaa; s:0-2" -> (o => new TST( addqsize(o), o('e'), o('a') )),
          "0100111001011 rrr" -> (o => new UNLK( o('r') )),
        ) )
      built = true
    }

    opcodes
  }

  private def generate( pattern: String ) = {
    case class Variable( v: Char, seq: collection.Seq[Int], bits: List[Int] )

    val Range = "([a-zA-Z]):(?:([0-9]+)-([0-9]+)((?:-[0-9]+)*)|([0-9]+(?:,[0-9]+)*))"r
    val p = pattern replace (" ", "") split ";"

    require( p.nonEmpty, "empty pattern" )

    val bits = p(0)

    require( bits.length > 0, "pattern should comprise at least one bit" )
    require( bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'), "pattern should comprise only 0's, 1's, letters or -'s" )

    val ranges = Map[Char, collection.Seq[Int]]( p drop 1 map {
      case Range( v, lower, upper, null, null ) => v.head -> (lower.toInt to upper.toInt)
      case Range( v, lower, upper, exceptions, null ) =>
        val remove = exceptions split "-" drop 1 map (_.toInt)

        v.head -> (lower.toInt to upper.toInt).filterNot (remove contains _)
      case Range( v, null, null, null, list ) => v.head -> (list split "," map (_.toInt)).toSeq
    }: _* )

    val (constant, variables) = {
      def scan( acc: Int, pos: Int, chars: List[Char], vars: Map[Char, List[Int]] ): (Int, Map[Char,List[Int]]) =
        chars match {
          case Nil => (acc, vars)
          case '0' :: t => scan( acc, pos << 1, t, vars )
          case '1' :: t => scan( acc | pos, pos << 1, t, vars )
          case v :: t if vars contains v => scan( acc, pos << 1, t, vars + (v -> (vars(v) :+ pos)) )
          case v :: t => scan( acc, pos << 1, t, vars + (v -> List(pos)) )
        }

      scan( 0, 1, bits.reverse.toList, Map() )
    }

    val enumeration = new ListBuffer[(Int, Map[Char, Int])]

    def enumerate( acc: Int, vars: List[Variable], vals: Map[Char, Int] ): Unit =
      vars match {
        case Nil => enumeration += ((acc, vals))
        case v :: t =>
          for (i <- v.seq)
            enumerate( acc|int2bits(0, i, v.bits), t, vals + (v.v -> i) )
      }

    def int2bits( res: Int, n: Int, bits: List[Int] ): Int =
      bits match {
        case Nil => res
        case b :: t if (n&1) > 0 => int2bits( res|b, n >> 1, t )
        case b :: t => int2bits( res, n >> 1, t )
      }

    enumerate( constant, variables.toList map {
      case (v, b) =>
        if (ranges contains v) {
          require( ranges(v).last < (1 << b.length), "second value of range must be less than 2^#bits" )
          Variable( v, ranges(v), b )
        } else
          Variable( v, 0 until (1 << b.length), b )
      }, Map() )
    enumeration.toList
  }

}