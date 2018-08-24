//@
package xyz.hyperreal.m68k

import scala.collection.mutable.ListBuffer


class CPU( private [m68k] val memory: Memory,
           private [m68k] val breakpoint: Int => Unit = _ => {},
           private [m68k] val trap: Int => Boolean = _ => false )
  extends Addressing {

  private [m68k] val D = new Array[Int]( 8 )
  private [m68k] val A = new Array[Long]( 7 )
  private [m68k] var PC = 0L
  private [m68k] var SP = 0L
  private [m68k] var SSP = 0L
  private [m68k] var MSP = 0L
  private [m68k] var C = false
  private [m68k] var V = false
  private [m68k] var Z = false
  private [m68k] var N = false
  private [m68k] var X = false
  private [m68k] var SR = 0
  private [m68k] var VBR = 0
  private [m68k] var instruction = 0
  private [m68k] val FP = new Array[Double]( 8 )
  private [m68k] var FPCR = 0
  private [m68k] var FPSR = 0
  private [m68k] var prog: Addressable = _

  var counter = 0L
  var trace = false

	protected var running = false

  private val opcodes = CPU.opcodeTable

  jump( 0L )

  def jump( address: Long ): Unit = {
    prog = memory.find( address )
    PC = address
  }

  def sr = {
    SR
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
  }

  def reset: Unit = {
    halt
    memory.reset

    for (i <- 0 until 8) {
      D(i) = 0
      FP(i) = 0
    }

    for (i <- 0 until 7)
      A(i) = 0

    jump( memory.code )
    running = false
    FPCR = 0
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
			running = false
		}

  def run: Unit = {
    running = true

    while (running)
      execute

    running = false
  }

  memory.problem = problem

  //
  // addressing
  //

  def cast( v: Int, size: Size ) =
    size match {
      case ByteSize => v.asInstanceOf[Byte].asInstanceOf[Int]
      case ShortSize => v.asInstanceOf[Short].asInstanceOf[Int]
      case IntSize => v
    }

  def memoryRead( address: Long, size: Size, aligned: Boolean ) =
    size match {
      case ByteSize if aligned => memory.readShort( address ).asInstanceOf[Byte].asInstanceOf[Int]
      case ByteSize => memory.readByte( address )
      case ShortSize => memory.readShort( address )
      case IntSize => memory.readInt( address )
    }

  def memoryWrite( data: Int, address: Long, size: Size, aligned: Boolean ) =
    size match {
      case ByteSize if aligned => memory.writeShort( address, data )
      case ByteSize => memory.writeByte( address, data )
      case ShortSize => memory.writeShort( address, data )
      case IntSize => memory.writeInt( address, data )
    }

  def width( size: Size, aligned: Boolean ) =
    size match {
      case ByteSize if aligned => 2
      case ByteSize => 1
      case ShortSize => 2
      case IntSize => 4
    }

  def readA( reg: Int ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 => MSP
      case 7 if (SR&SRBit.S) != 0 => SSP
      case 7 => SP
      case _ => A(reg)
    }

  def writeA( data: Long, reg: Int ) =
    reg match {
      case 7 if (SR&(SRBit.S|SRBit.M)) != 0 => MSP = data
      case 7 if (SR&SRBit.S) != 0 => SSP = data
      case 7 => SP = data
      case _ => A(reg) = data
    }

  def read( mode: Int, reg: Int, size: Size ) = {
    mode match {
      case DataRegisterDirect => cast( D(reg), size )
      case AddressRegisterDirect => cast( readA(reg).asInstanceOf[Int], size )
      case AddressRegisterIndirect => memoryRead( A(reg), size, false )
      case OtherModes =>
        reg match {
          case ImmediateData => immediate( size )
        }
    }
  }

  def regwrite( data: Int, regcur: Int, size: Size ) =
    size match {
      case ByteSize => regcur&0xFFFFFF00 | data&0xFF
      case ShortSize => regcur&0xFFFF0000 | data&0xFFFF
      case IntSize => data
    }

  def write( data: Int, mode: Int, reg: Int, size: Size ) {
    mode match {
      case DataRegisterDirect => D(reg) = regwrite( data, D(reg), size )
      case AddressRegisterDirect => writeA( regwrite(data, readA(reg).asInstanceOf[Int], size)&0xFFFFFFFFL, reg )
      case AddressRegisterIndirect => memoryWrite( data, A(reg), size, false )
      case OtherModes =>
//        reg match {
//        }
    }
  }

  //
  // ALU
  //

  def flags( overflow: Int, carry: Int, extended: Boolean, res: Int, x: Boolean ) = {
    V = (overflow&0x80000000L) != 0
    C = (carry&0x80000000L) != 0

    if (x)
      X = C

    Z = if (extended) res == 0 && Z else res == 0
    N = res < 0
    res
  }

  def add( s: Int, d: Int, extended: Boolean ) = {
    val r = s + d

    flags( s&d&(~r)|(~s)&(~d)&r, s&d|(~r)&d|s&(~r), extended, r, true )
  }

  def immediate( size: Size ) =
    size match {
      case ByteSize => fetchByte
      case ShortSize => fetchShort
      case IntSize => fetchInt
    }

}

object CPU {

  private val opcodes = Array.fill[Instruction]( 0x10000 )( IllegalInstruction )
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

  def movesize( operands: Map[Char, Int] ) =
    operands('s') match {
      case 1 => ByteSize
      case 3 => ShortSize
      case 2 => IntSize
    }

  def opcodeTable: IndexedSeq[Instruction] = synchronized {
    if (!built) {
      populate(
        List[(String, Map[Char, Int] => Instruction)](
//          "1101 rrr ooo eee aaa" -> (o => ADD( ),
//          "00000110 ss eee aaa" -> ADDI,
          "1101 rrr ooo eee aaa" -> (o => new ADDA( addasize(o), o('e'), o('a'), o('r') )),
          "00000110 ss eee aaa" -> (o => new ADDI( addqsize(o), o('e'), o('a') )),
          "0101 ddd 0 ss eee aaa" -> (o => new ADDQ( o('d') + 1, addqsize(o), o('e'), o('a') )),
          "0100100001001 vvv" -> (o => new BKPT( o('v') )),
          "00 ss vvv uuu xxx yyy" -> (o => new MOVE( movesize(o), o('v'), o('u'), o('x'), o('y') )),
          "0111 rrr 0 dddddddd" -> (o => new MOVEQ( o('r'), o('d') )),
          "010011100100 vvvv" -> (o => new TRAP( o('v') ))
        ) )
      built = true
    }

    opcodes
  }

  private def generate( pattern: String ) = {
    case class Variable( v: Char, lower: Int, upper: Int, bits: List[Int] )

    val Range = "([a-zA-Z]):([0-9]+)-([0-9]+)"r
    val p = pattern replace (" ", "") split ";"

    require( p.nonEmpty, "empty pattern" )

    val bits = p(0)

    require( bits.length > 0, "pattern should comprise at least one bit" )
    require( bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'), "pattern should comprise only 0's, 1's, letters or -'s" )

    val ranges = Map( (p drop 1 map {case Range( v, l, u ) => v(0) -> (l.toInt, u.toInt)}): _* )

    require( ranges forall {case (_, (l, u)) => 0 <= l && l <= u}, "first value of range must be less than or equal to second and be non-negative" )

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
          for (i <- v.lower to v.upper)
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
          require( ranges(v)._2 < (1 << b.length), "second value of range must be less than 2^#bits" )
          Variable( v, ranges(v)._1, ranges(v)._2, b )
        } else
          Variable( v, 0, (1 << b.length) - 1, b )
      }, Map() )
    enumeration.toList
  }

}