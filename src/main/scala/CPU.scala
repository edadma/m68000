//@
package xyz.hyperreal.m68000

import scala.collection.mutable.ListBuffer


class CPU( private [m68000] val memory: Memory ) {

  private [m68000] val D = new Array[Int]( 8 )
  private [m68000] val A = new Array[Int]( 8 )
  private [m68000] var pc = 0L
  private [m68000] var C = false
  private [m68000] var V = false
  private [m68000] var Z = false
  private [m68000] var N = false
  private [m68000] var X = false
  private [m68000] var VBR = 0
  private [m68000] var instruction = 0
  private [m68000] val f = new Array[Double]( 8 )
  private [m68000] var fcsr = 0
  private [m68000] var disp = 0

  var counter = 0L
  var trace = false

	protected var running = false

  private val opcodes = CPU.opcodeTable

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
    sys.error( s"error at ${pc.toHexString} (${"%08x".format(instruction)}): $error" )
  }

  def halt: Unit = {
    running = false
  }

  def reset: Unit = {
    halt
    memory.reset

    for (i <- 0 until 16) {
      D(i) = 0
      A(i) = 0
      f(i) = 0
    }

    pc = memory.code
    running = false
    fcsr = 0
  }

  def fetch: Unit = instruction = memory.find( pc ).readInt( pc )

  def execute: Unit = {
    if (trace) {
      disassemble
      registers
    }

    val m = memory.find( pc )
    val low = m.readByte( pc )

    instruction = m.readShort( pc, low )&0xFFFF
    disp = 2
    opcodes(instruction)( this )

    pc += disp
    counter += 1
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

  def cast( v: Int, size: Int ) =
    size match {
      case 0 => v.asInstanceOf[Byte].asInstanceOf[Int]
      case 1 => v.asInstanceOf[Short].asInstanceOf[Int]
      case 2 => v
    }

  def read( mode: Int, reg: Int, size: Int ) = {
    val value =
      mode match {
        // Data Register Direct
        case 0 => cast( D(reg), size )
        // Address Register Direct
        case 1 => cast( A(reg), size )
        // Address Register Indirect
        case 2 =>
          size match {
            case 0 => memory.readByte( A(reg) )
            case 1 => memory.readShort( A(reg) )
            case 2 => memory.readInt( A(reg) )
          }
      }
  }

  //
  // ALU
  //

  def flags( overflow: Int, carry: Int, zero: Boolean, res: Int ): Unit = {
    V = (overflow&0x80000000L) != 0
    C = (carry&0x80000000L) != 0
    X = C
    Z = zero
    N = res < 0
  }

  def add( s: Int, d: Int, extended: Boolean ) = {
    val r = s + d
    val z = if (extended) r == 0 && Z else r == 0

    flags( s&d&(~r)|(~s)&(~d)&r, s&d|(~r)&d|s&(~r), z, r )
    r
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

  def opcodeTable: IndexedSeq[Instruction] = synchronized {
    if (!built) {
      populate(
        List[(String, Map[Char, Int] => Instruction)](
//          "1101 rrr ooo eee aaa" -> (o => ADD( ),
//          "00000110 ss eee aaa" -> ADDI,
          "0101 ddd 0 ss eee aaa" -> (o => new ADDQ( o('d') + 1, o('s'), o('e'), o('a') ))
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