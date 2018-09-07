//@
package xyz.hyperreal.m68k


abstract class Instruction extends (CPU => Unit) {

  def disassemble( cpu: CPU ): String

}

object ILLEGAL extends Instruction {

  def apply( cpu: CPU ) =
    if (!cpu.illegal)
      cpu.exception( VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ABCD( x: Int, r: Int, y: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    if (r == 0)
      cpu.writeD( cpu.abcd(cpu.readD(x, ByteSize), cpu.readD(y, ByteSize)), x, ByteSize )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, x, ByteSize )( cpu.add(_, cpu.read(AddressRegisterIndirectPredecrement, y, ByteSize), false) )
  }

  def disassemble( cpu: CPU ) = s"ABCD"

}

class ADD( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.add(cpu.read(mode, reg, size), cpu.readD(reg, size), false), reg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.add(_, cpu.readD(reg, size), false) )
  }

  def disassemble( cpu: CPU ) = s"ADD"

}

class ADDA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.add(cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], false), areg )
  }

  def disassemble( cpu: CPU ) = s"ADDA"

}

class ADDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.add(_, cpu.immediate(size), false) )
  }

  def disassemble( cpu: CPU ) = s"ADDI"

}

class ADDQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.add(_, data, false) )
  }

  def disassemble( cpu: CPU ) = s"ADDQ"

}

class AND( dreg: Int, dest: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dest == 0)
      cpu.writeD( cpu.and(cpu.read(mode, reg, size), cpu.readD(dreg, size)), reg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.and(_, cpu.readD(dreg, size)) )
  }

  def disassemble( cpu: CPU ) = s"AND"

}

class ANDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.and(_, cpu.immediate(size)) )
  }

  def disassemble( cpu: CPU ) = s"ANDI"

}

object ANDItoCCR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.fetchByte

    if (!testBit( imm, CCR.X ))
      cpu.X = false

    if (!testBit( imm, CCR.N ))
      cpu.N = false

    if (!testBit( imm, CCR.Z ))
      cpu.Z = false

    if (!testBit( imm, CCR.V ))
      cpu.V = false

    if (!testBit( imm, CCR.V ))
      cpu.V = false
  }

  def disassemble( cpu: CPU ) = s"ANDI"

}

object ANDItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR & cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"ANDI"

}

class ASMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( x => if (dir == 0) cpu.asr(1, x, ShortSize) else cpu.asl(1, x, ShortSize) )
  }

  def disassemble( cpu: CPU ) = s"AS"

}

class ASReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.asr(c, operand, size) else cpu.asl(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = s"AS"

}

class Bcc( cond: Int, disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val pc = cpu.PC

    if (cpu.testcc( cond ))
      cpu.jumpto( pc + disp match {
        case 0 => cpu.fetchShort
        case -1 => cpu.fetchInt//020
        case _ => disp
      } )
  }

  def disassemble( cpu: CPU ) = s"Bcc"

}

class BCHG( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.readWrite( mode, reg, BitSize ){ x =>
      cpu.Z = testBit( x, bit )
      flipBit(x, bit)
    }
  }

  def disassemble( cpu: CPU ) = s"BCHG"

}

class BCLR( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.readWrite( mode, reg, BitSize ) { x =>
      cpu.Z = testBit( x, bit )
      clearBit( x, bit )
    }
  }

  def disassemble( cpu: CPU ) = s"BCLR"

}

class BKPT( bkpt: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (!cpu.breakpoint( bkpt ))
      cpu.exception( VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ) = s"BKPT"

}

class BSET( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.readWrite( mode, reg, BitSize ) { x =>
      cpu.Z = testBit( x, bit )
      setBit(x, bit)
    }
  }

  def disassemble( cpu: CPU ) = s"BSET"

}

class BSR( disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val jump =
      disp match {
        case 0 => cpu.fetchShort
        case -1 => cpu.fetchInt//020
        case _ => disp
      }

    cpu.pushAddress( cpu.PC )
    cpu.jumpto( jump )
  }

  def disassemble( cpu: CPU ) = s"BSR"

}

class BTST( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val data = cpu.read( mode, reg, BitSize )
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.Z = testBit( data, bit )
  }

  def disassemble( cpu: CPU ) = s"BTST"

}

class CHK( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val upper = cpu.read( mode, reg, size )
    val d = cpu.readD( reg, size )

    if (d < 0) {
      cpu.N = true
      cpu.exception( VectorTable.CHKInstruction )
    } else if (d > upper) {
      cpu.N = false
    }
  }

  def disassemble( cpu: CPU ) = s"CHK"

}

class CLR( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( 0, mode, reg, size )
    cpu.Z = true
    cpu.N = false
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = s"CLR"

}

class CMP( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cast(cpu.D(reg), size), cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = s"CMP"

}

class CMPA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.readA(areg).asInstanceOf[Int], cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = s"CMPA"

}

class CMPI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.immediate(size), cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = s"CMPI"

}

class CMPM( size: Size, rx: Int, ry: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.read(AddressRegisterIndirectPostincrement, rx, size), cpu.read(AddressRegisterIndirectPostincrement, ry, size), false )
  }

  def disassemble( cpu: CPU ) = s"CMPM"

}

class DBcc( cond: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val pc = cpu.PC

    if (cpu.testcc( cond ))
      cpu.PC += 2
    else {
      val res = cpu.readD( reg, ShortSize ) - 1

      cpu.writeD( res, reg, ShortSize )

      if (res == -1)
        cpu.PC += 2
      else
        cpu.jumpto( pc + cpu.fetchShort )
    }
  }

  def disassemble( cpu: CPU ) = s"DBcc"

}

class DIVS( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val a = cpu.D(dreg)
    val b = cpu.read( mode, reg, ShortSize )

    if (b == 0)
      cpu.exception( VectorTable.integerDivideByZero )
    else {
      val q = a/b

      if (q < Short.MinValue || q > Short.MaxValue)
        cpu.V = true
      else {
        cpu.N = q < 0
        cpu.Z = q == 0
        cpu.C = false
        cpu.V = false
        cpu.D(dreg) = ((a%b)<<16)|(q&0xFFFF)
      }
    }
  }

  def disassemble( cpu: CPU ) = s"DIVS"

}

class DIVU( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val a = cpu.D(dreg)&0xFFFFFFFFL
    val b = cpu.read( mode, reg, ShortSize )&0xFFFF

    if (b == 0)
      cpu.exception( VectorTable.integerDivideByZero )
    else {
      val q = a/b

      if (q < Short.MinValue || q > Short.MaxValue)
        cpu.V = true
      else {
        cpu.N = q < 0
        cpu.Z = q == 0
        cpu.C = false
        cpu.V = false
        cpu.D(dreg) = ((a%b).asInstanceOf[Int]<<16)|(q.asInstanceOf[Int]&0xFFFF)
      }
    }
  }

  def disassemble( cpu: CPU ) = s"DIVU"

}

class EOR( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.eor(_, cpu.readD(reg, size)) )
  }

  def disassemble( cpu: CPU ) = s"EOR"

}

class EORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.eor(_, cpu.immediate(size)) )
  }

  def disassemble( cpu: CPU ) = s"EORI"

}

object EORItoCCR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.fetchByte

    if (testBit( imm, CCR.X ))
      cpu.X ^= true

    if (testBit( imm, CCR.N ))
      cpu.N ^= true

    if (testBit( imm, CCR.Z ))
      cpu.Z ^= true

    if (testBit( imm, CCR.V ))
      cpu.V ^= true

    if (testBit( imm, CCR.V ))
      cpu.V ^= true
  }

  def disassemble( cpu: CPU ) = s"EORI"

}

object EORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR ^ cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"EORI"

}

class EXG( rx: Int, mode: Int, ry: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    mode match {
      case 0x08 =>
        val temp = cpu.D(rx)

        cpu.D(rx) = cpu.D(ry)
        cpu.D(ry) = temp
      case 0x09 =>
        val temp = cpu.readA(rx)

        cpu.writeA( cpu.readA(ry), rx )
        cpu.writeA( temp, ry )
      case 0x11 =>
        val temp = cpu.D(rx)

        cpu.D(rx) = cpu.readA(ry).asInstanceOf[Int]
        cpu.writeA( temp, ry )
    }
  }

  def disassemble( cpu: CPU ) = s"EXG"

}

class EXT( size: Size, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val (res, s) =
      size match {
        case ByteSize => (cpu.readD( reg, ByteSize ), ShortSize)
        case ShortSize => (cpu.readD( reg, ShortSize ), IntSize)
        case IntSize => (cpu.readD( reg, ByteSize ), IntSize)
      }

    cpu.N = res < 0
    cpu.Z = res == 0
    cpu.writeD( res, reg, s )
  }

  def disassemble( cpu: CPU ) = s"EXT"

}

class JMP( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.jumpto( cpu.read(mode, reg, IntSize) )
  }

  def disassemble( cpu: CPU ) = s"JMP"

}

class JSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val addr = cpu.read( mode, reg, IntSize )

    cpu.pushAddress( cpu.PC )
    cpu.jumpto( addr )
  }

  def disassemble( cpu: CPU ) = s"JMP"

}

class LEA( areg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.address(mode, reg), areg )
  }

  def disassemble( cpu: CPU ) = s"LEA"

}

object LINEA extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.lineA)
      cpu.exception( VectorTable.lineA )
  }

  def disassemble( cpu: CPU ) = s"LINEA"

}

object LINEF extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.lineF)
      cpu.exception( VectorTable.lineF )
  }

  def disassemble( cpu: CPU ) = s"LINEF"

}

class LINK( reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.pushAddress( cpu.readA(reg) )

    val sp = cpu.readA( 7 )

    cpu.writeA( sp, reg )
    cpu.writeA( sp + cpu.immediate(ShortSize), 7 )
  }

  def disassemble( cpu: CPU ) = s"LINK"

}

class LSMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.lsr(1, o, ShortSize) else cpu.lsl(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = s"LS"

}

class LSReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.lsr(c, operand, size) else cpu.lsl(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = s"LS"

}

class MOVE( size: Size, dreg: Int, dmode: Int, smode: Int, sreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.flags(0, 0, false, cpu.read(smode, sreg, size), false), dmode, dreg, size )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MOVEA( size: Size, areg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.read(mode, reg, size), areg )
  }

  def disassemble( cpu: CPU ) = s"MOVEA"

}

class MOVEfromSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.fromSR, mode, reg, ShortSize )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MOVEtoCCR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.read(mode, reg, ByteSize) )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MOVEtoSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      cpu.toSR( cpu.read(mode, reg, ShortSize) )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MOVEQ( reg: Int, data: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(reg) = cpu.flags( 0, 0, false, data, false )
  }

  def disassemble( cpu: CPU ) = s"MOVEQ"

}

class MOVEUSP( dir: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      if (dir == 0)
        cpu.USP = cpu.readA( reg )
      else
        cpu.writeA( cpu.USP, reg )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MULS( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val res = cpu.read( mode, reg, ShortSize ) * cpu.readD( dreg, ShortSize )

    cpu.N = res < 0
    cpu.Z = res == 0
    cpu.V = false
    cpu.C = false
    cpu.D(dreg) = res
  }

  def disassemble( cpu: CPU ) = s"MULS"

}

class MULU( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(dreg) = cpu.flags( 0, 0, false,
      (cpu.read( mode, reg, ShortSize )&0xFFFF)*(cpu.readD( dreg, ShortSize )&0xFFFF), false )
  }

  def disassemble( cpu: CPU ) = s"MULU"

}

class NEG( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size)( cpu.neg(_, false) )
  }

  def disassemble( cpu: CPU ) = s"NEG"

}

class NEGX( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size)( cpu.neg(_, true) )
  }

  def disassemble( cpu: CPU ) = s"NEGX"

}

object NOP extends Instruction {

  def apply( cpu: CPU ): Unit = {
  }

  def disassemble( cpu: CPU ) = s"NOP"

}

class NOT( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( x => cpu.flags(0, 0, false, ~x, false) )
  }

  def disassemble( cpu: CPU ) = s"NOT"

}

class OR( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.eor(cpu.read(mode, reg, size), cpu.readD(reg, size)), reg, size )
    else
      cpu.write( cpu.eor(cpu.read(mode, reg, size), cpu.readD(reg, size)), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"EOR"

}

class ORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.or(cpu.read(mode, reg, size), cpu.immediate(size)), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"ORI"

}

object ORItoCCR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.fetchByte

    if (testBit( imm, CCR.X ))
      cpu.X |= true

    if (testBit( imm, CCR.N ))
      cpu.N |= true

    if (testBit( imm, CCR.Z ))
      cpu.Z |= true

    if (testBit( imm, CCR.V ))
      cpu.V |= true

    if (testBit( imm, CCR.V ))
      cpu.V |= true
  }

  def disassemble( cpu: CPU ) = s"ORI"

}

object ORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR | cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"ORI"

}

class PEA( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.pushAddress( cpu.address(mode, reg) )
  }

  def disassemble( cpu: CPU ) = s"PEA"

}

object RESET extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor)
      cpu.resetSignal

  def disassemble( cpu: CPU ) = s"RESET"

}

class ROMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val operand = cpu.read( mode, reg, ShortSize )

    cpu.write( if (dir == 0) cpu.ror(1, operand, ShortSize) else cpu.rol(1, operand, ShortSize), mode, reg, ShortSize )
  }

  def disassemble( cpu: CPU ) = s"RO"

}

class ROReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.ror(c, operand, size) else cpu.rol(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = s"RO"

}

class ROXMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val operand = cpu.read( mode, reg, ShortSize )

    cpu.write( if (dir == 0) cpu.ror(1, operand, ShortSize) else cpu.rol(1, operand, ShortSize), mode, reg, ShortSize )
  }

  def disassemble( cpu: CPU ) = s"ROX"

}

class ROXReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.ror(c, operand, size) else cpu.rol(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = s"ROX"

}

object RTE extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor) {
      cpu.toSR( cpu.pop(ShortSize) )
      cpu.jumpto( cpu.popAddress )
    }

  def disassemble( cpu: CPU ) = s"RTE"

}

object RTR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.pop(ShortSize) )
    cpu.jumpto( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = s"RTR"

}

object RTS extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.jumpto( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = s"RTS"

}

class SUB( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.subtract(cpu.read(mode, reg, size), cpu.readD(reg, size), false), reg, size )
    else
      cpu.write( cpu.subtract(cpu.read(mode, reg, size), cpu.readD(reg, size), false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"SUB"

}

class SWAP( reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val v = cpu.D(reg)

    cpu.D(reg) = (v<<16) | (v>>>16)
    cpu.N = testBit( v, 15 )
    cpu.Z = v == 0
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = s"SWAP"

}

class TAS( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val v = cpu.read( mode, reg, ByteSize )

    cpu.N = testBit( v, 7 )
    cpu.Z = v == 0
    cpu.V = false
    cpu.C = false
    cpu.write( v|0x80, mode, reg, ByteSize )
  }

  def disassemble( cpu: CPU ) = s"TAS"

}

class TRAP( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.trap( vector )) {
      cpu.exception( VectorTable.TRAPInstruction + (vector<<2) )
    }
  }

  def disassemble( cpu: CPU ) = s"TRAP"

}

object TRAPV extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.V) {
      cpu.exception( VectorTable.TRAPVInstruction )
    }
  }

  def disassemble( cpu: CPU ) = s"TRAPV"

}

class TST( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val v = cpu.read( mode, reg, size )

    cpu.N = v < 0
    cpu.Z = v == 0
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = s"TST"

}

class UNLK( reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.readA(reg), 7 )
    cpu.writeA( cpu.popAddress, reg )
  }

  def disassemble( cpu: CPU ) = s"UNLK"

}
