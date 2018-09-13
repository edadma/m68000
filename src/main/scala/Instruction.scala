//@
package xyz.hyperreal.m68k


abstract class Instruction extends (CPU => Unit) with Addressing {

  def disassemble( cpu: CPU ): String

}

object ILLEGAL extends Instruction {

  def apply( cpu: CPU ) =
    if (!cpu.illegal)
      cpu.exception( VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ABCD( y: Int, r: Int, x: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    if (r == 0)
      cpu.writeD( cpu.abcd(cpu.readD(x, ByteSize), cpu.readD(y, ByteSize)), x, ByteSize )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, x, ByteSize )( cpu.abcd(_, cpu.read(AddressRegisterIndirectPredecrement, y, ByteSize)) )
  }

  def disassemble( cpu: CPU ) = "ABCD   " + (if (r == 0) s"D$y, D$x" else s"-(A$y), -(A$x)")

}

class ADD( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.add(cpu.read(mode, reg, size), cpu.readD(reg, size), false), reg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.add(_, cpu.readD(reg, size), false) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "ADD", size, mode, reg, dir, dreg )

}

class ADDA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.add(cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], false), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "ADDA", size, mode, reg, areg )

}

class ADDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.add(_, cpu.immediate(size), false) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "ADDI", size, mode, reg )

}

class ADDQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.add(_, data, false) )
  }


  def disassemble( cpu: CPU ) = mnemonic( "ADDQ", size ) + s"#$data, ${cpu.operand( size, mode, reg )}"

}

class AND( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.and(cpu.read(mode, reg, size), cpu.readD(dreg, size)), reg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.and(_, cpu.readD(dreg, size)) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "AND", size, mode, reg, dir, dreg )

}

class ANDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.and(_, cpu.immediate(size)) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "ANDI", size, mode, reg )

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

  def disassemble( cpu: CPU ) = s"ANDI   #${cpu.fetchByte}, CCR"

}

object ANDItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      cpu.toSR( cpu.fromSR & cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"ANDI   #${cpu.fetchShort}, SR"

}

class ASMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( x => if (dir == 0) cpu.asr(1, x, ShortSize) else cpu.asl(1, x, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"AS$d    ${cpu.operand( ShortSize, mode, reg )}"
  }

}

class ASReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD( dreg, size )

    cpu.writeD( if (dir == 0) cpu.asr(c, operand, size) else cpu.asl(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'
    val c = if (ir == 0) s"#$count" else s"D$count"

    mnemonic( s"AS$d", size ) + s"$c, D$dreg"
  }

}

class Bcc( cond: Int, disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val pc = cpu.PC

    if (cpu.testcc( cond ))
      cpu.jumpto( pc + cpu.displacement(disp) )
  }

  def disassemble( cpu: CPU ) = mnemonic( s"B${Conditional( cond )}" ) + (cpu.PC + cpu.displacement(disp))

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
      flipBit( x, bit )
    }
  }

  def disassemble( cpu: CPU ) =
    "BCHG   " +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

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

  def disassemble( cpu: CPU ) =
    "BCLR   " +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

}

class BKPT( bkpt: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (!cpu.breakpoint( bkpt ))
      cpu.exception( VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ) = s"BKPT   #$bkpt"

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

  def disassemble( cpu: CPU ) =
    "BSET   " +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

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

  def disassemble( cpu: CPU ) = s"BSR    ${cpu.PC + cpu.displacement(disp)}"

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

  def disassemble( cpu: CPU ) =
    "BTST   " +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

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

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "CHK", size, mode, reg, dreg )

}

class CLR( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( 0, mode, reg, size )
    cpu.Z = true
    cpu.N = false
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = cpu.unary( "CLR", size, mode, reg )

}

class CMP( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cast(cpu.D(reg), size), cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "CMP", size, mode, reg, dreg )

}

class CMPA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.readA(areg).asInstanceOf[Int], cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "CMPA", size, mode, reg, areg )

}

class CMPI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.immediate(size), cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "CMPI", size, mode, reg )

}

class CMPM( size: Size, rx: Int, ry: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.read(AddressRegisterIndirectPostincrement, rx, size), cpu.read(AddressRegisterIndirectPostincrement, ry, size), false )
  }

  def disassemble( cpu: CPU ) = mnemonic( "CMPM", size ) + s"(A$ry)+, (A$rx)+"

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

  def disassemble( cpu: CPU ) = mnemonic( s"DB${Conditional( cond )}" ) + s"D$reg, ${cpu.PC + cpu.fetchShort}"

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

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "DIVS", ShortSize, mode, reg, dreg )

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

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "DIVU", ShortSize, mode, reg, dreg )

}

class EOR( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.eor(_, cpu.readD(reg, size)) )
  }

  def disassemble( cpu: CPU ) = cpu.binarySrcD( "EOR", size, mode, reg, dreg )

}

class EORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.eor(_, cpu.immediate(size)) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "EORI", size, mode, reg )

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

  def disassemble( cpu: CPU ) = s"EORI   #${cpu.fetchByte}, CCR"

}

object EORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR ^ cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"EORI   #${cpu.fetchShort}, SR"

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

  def disassemble( cpu: CPU ) =
    "EXG    " +
      (mode match {
        case 0x08 => s"D$rx, D$ry"
        case 0x09 => s"A$rx, A$ry"
        case 0x11 => s"D$rx, A$ry"
      })

}

class EXT( size: Size, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val (res, s) =
      size match {
        case ShortSize => (cpu.readD( reg, ByteSize ), ShortSize)
        case IntSize => (cpu.readD( reg, ShortSize ), IntSize)
//        case ByteSize => (cpu.readD( reg, ByteSize ), IntSize)
      }

    cpu.N = res < 0
    cpu.Z = res == 0
    cpu.writeD( res, reg, s )
  }

  def disassemble( cpu: CPU ) = mnemonic( "EXT", size ) + s"D$reg"

}

class JMP( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.jumpto( cpu.read(mode, reg, IntSize) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "JMP", mode, reg )

}

class JSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val addr = cpu.read( mode, reg, IntSize )

    cpu.pushAddress( cpu.PC )
    cpu.jumpto( addr )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "JSR", mode, reg )

}

class LEA( areg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.address(mode, reg), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "LEA", mode, reg, areg )

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

  def disassemble( cpu: CPU ) = s"LINK   A$reg, #${cpu.immediate(ShortSize)}"

}

class LSMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.lsr(1, o, ShortSize) else cpu.lsl(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"LS$d    ${cpu.operand( ShortSize, mode, reg )}"
  }

}

class LSReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.lsr(c, operand, size) else cpu.lsl(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'
    val c = if (ir == 0) s"#$count" else s"D$count"

    mnemonic( s"LS$d", size ) + s"$c, D$dreg"
  }

}

class MOVE( size: Size, dreg: Int, dmode: Int, smode: Int, sreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.flags(0, 0, false, cpu.read(smode, sreg, size), false), dmode, dreg, size )
  }

  def disassemble( cpu: CPU ) = mnemonic( "MOVE", size ) + s"${cpu.operand( size, smode, sreg )}, ${cpu.operand( size, dmode, dreg )}"

}

class MOVEA( size: Size, areg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.read(mode, reg, size), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "MOVEA", size, mode, reg, areg )

}

class MOVEM( dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def regs( cpu: CPU ) = {
    val list = cpu.fetchShort

    0 until 16 flatMap { i =>
      if (testBit(list, i))
        List(i)
      else
        Nil
    } toList
  }

  def apply( cpu: CPU ): Unit = {
    dir match {
      case 0 =>
        mode match {
          case AddressRegisterIndirectPredecrement =>
            regs( cpu ) map { idx =>
              if (idx < 8)
                cpu.D( 7 - idx )
              else
                cpu.readA( 7 - (idx - 8) ).asInstanceOf[Int]
            } foreach (cpu.write( _, mode, reg, size ))
          case _ =>
            val rs =
              regs( cpu ) map { idx =>
                if (idx < 8)
                  cpu.D( idx )
                else
                  cpu.readA( idx - 8 ).asInstanceOf[Int]
              }
            var addr = cpu.address( mode, reg )

            for (r <- rs) {
              cpu.memoryWrite( r, addr, size, false )
              addr += width( size, false )
            }
        }
      case 1 =>
        mode match {
          case AddressRegisterIndirectPostincrement =>
            regs( cpu ) foreach { idx =>
              if (idx < 8)
                cpu.D(idx) = cpu.read(mode, reg, size)
              else
                cpu.writeA(cpu.read(mode, reg, size), idx - 8)
            }
          case _ =>
            var addr = cpu.address(mode, reg)

            for (idx <- regs( cpu )) {
              if (idx < 8)
                cpu.D(idx) = cpu.memoryRead(addr, size, false)
              else
                cpu.writeA(cpu.memoryRead(addr, size, false), idx - 8)

              addr += width(size, false)
            }
        }
    }
  }

  def disassemble( cpu: CPU ) = {
    val rs =
      mode match {
        case AddressRegisterIndirectPredecrement =>
          regs(cpu) map { idx => if (idx < 8) s"D${7 - idx}" else s"A${7 - (idx - 8)}" }
        case _ =>
          regs(cpu) map { idx => if (idx < 8) s"D$idx" else s"A${idx - 8}" }
      }

    mnemonic( "MOVEM", size ) +
      (if (dir == 0)
        ranges( rs ) + ", " + cpu.operand( mode, reg )
      else
        cpu.operand( mode, reg ) + ", " + ranges( rs ))
  }

}

class MOVEfromSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.fromSR, mode, reg, ShortSize )
  }

  def disassemble( cpu: CPU ) = s"MOVE   SR, ${cpu.operand( mode, reg )}"

}

class MOVEtoCCR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.read(mode, reg, ByteSize) )
  }

  def disassemble( cpu: CPU ) = s"MOVE   ${cpu.operand( mode, reg )}, CCR"

}

class MOVEtoSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      cpu.toSR( cpu.read(mode, reg, ShortSize) )
  }

  def disassemble( cpu: CPU ) = s"MOVE   ${cpu.operand( mode, reg )}, SR"

}

class MOVEQ( reg: Int, data: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(reg) = cpu.flags( 0, 0, false, data, false )
  }

  def disassemble( cpu: CPU ) = s"MOVEQ  #$data, D$reg"

}

class MOVEUSP( dir: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      if (dir == 0)
        cpu.USP = cpu.readA( reg )
      else
        cpu.writeA( cpu.USP, reg )
  }

  def disassemble( cpu: CPU ) = "MOVE   " + (if (dir == 0) s"A$reg, USP" else s"USP, A$reg")

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

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "MULS", ShortSize, mode, reg, dreg )

}

class MULU( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(dreg) = cpu.flags( 0, 0, false,
      (cpu.read( mode, reg, ShortSize )&0xFFFF)*(cpu.readD( dreg, ShortSize )&0xFFFF), false )
  }

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "MULU", ShortSize, mode, reg, dreg )

}

class NBCD( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ByteSize )( cpu.sbcd(_, 0) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "NBCD", mode, reg )

}

class NEG( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size)( cpu.neg(_, false) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "NEG", size, mode, reg )

}

class NEGX( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size)( cpu.neg(_, true) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "NEGX", size, mode, reg )

}

object NOP extends Instruction {

  def apply( cpu: CPU ): Unit = {
  }

  def disassemble( cpu: CPU ) = "NOP"

}

class NOT( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( x => cpu.flags(0, 0, false, ~x, false) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "NOT", size, mode, reg )

}

class OR( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.eor(cpu.read(mode, reg, size), cpu.readD(reg, size)), reg, size )
    else
      cpu.write( cpu.eor(cpu.read(mode, reg, size), cpu.readD(reg, size)), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "OR", size, mode, reg, dir, dreg )

}

class ORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.or(_, cpu.immediate(size)) )
  }

  def disassemble( cpu: CPU ) = cpu.unary( "ORI", size, mode, reg )

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

  def disassemble( cpu: CPU ) = s"ORI    #${cpu.fetchByte}, CCR"

}

object ORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR | cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"ORI    #${cpu.fetchShort}, SR"

}

class PEA( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.pushAddress( cpu.address(mode, reg) )
  }

  def disassemble( cpu: CPU ) = s"PEA    ${cpu.operand( mode, reg )}"

}

object RESET extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor)
      cpu.resetSignal

  def disassemble( cpu: CPU ) = "RESET"

}

class ROMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.ror(1, o, ShortSize) else cpu.rol(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"RO$d    ${cpu.operand( ShortSize, mode, reg )}"
  }

}

class ROReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.ror(c, operand, size) else cpu.rol(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'
    val c = if (ir == 0) s"#$count" else s"D$count"

    mnemonic( s"RO$d", size ) + s"$c, D$dreg"
  }

}

class ROXMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.ror(1, o, ShortSize) else cpu.rol(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"ROX$d    ${cpu.operand( ShortSize, mode, reg )}"
  }

}

class ROXReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.ror(c, operand, size) else cpu.rol(c, operand, size), dreg, size )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'
    val c = if (ir == 0) s"#$count" else s"D$count"

    mnemonic( s"ROX$d", size ) + s"$c, D$dreg"
  }

}

object RTE extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor) {
      cpu.toSR( cpu.pop(ShortSize) )
      cpu.jumpto( cpu.popAddress )
    }

  def disassemble( cpu: CPU ) = "RTE"

}

object RTR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.pop(ShortSize) )
    cpu.jumpto( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = "RTR"

}

object RTS extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.jumpto( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = "RTS"

}

class SBCD( y: Int, r: Int, x: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    if (r == 0)
      cpu.writeD( cpu.sbcd(cpu.readD(x, ByteSize), cpu.readD(y, ByteSize)), x, ByteSize )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, x, ByteSize )( cpu.sbcd(_, cpu.read(AddressRegisterIndirectPredecrement, y, ByteSize)) )
  }

  def disassemble( cpu: CPU ) = "SBCD   " + (if (r == 0) s"D$y, D$x" else s"-(A$y), -(A$x)")

}

object STOP extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor) {
      cpu.toSR( cpu.fetchShort )
      cpu.stopped = true
    }

  def disassemble( cpu: CPU ) = s"STOP   #${cpu.fetchShort}"

}

class SUB( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.subtract(cpu.read(mode, reg, size), cpu.readD(reg, size), false), reg, size )
    else
      cpu.readWrite( mode, reg, size)( cpu.subtract(_, cpu.readD(reg, size), false) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "SUB", size, mode, reg, dir, dreg )

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

  def disassemble( cpu: CPU ) = s"SWAP   D$reg"

}

class TAS( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ByteSize ) { v =>
      cpu.N = testBit( v, 7 )
      cpu.Z = v == 0
      cpu.V = false
      cpu.C = false
      v|0x80
    }
  }

  def disassemble( cpu: CPU ) = s"TAS    ${cpu.operand( ByteSize, mode, reg )}"

}

class TRAP( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.trap( vector )) {
      cpu.exception( VectorTable.TRAPInstruction + (vector<<2) )
    }
  }

  def disassemble( cpu: CPU ) = s"TRAP   #$vector"

}

object TRAPV extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.V) {
      cpu.exception( VectorTable.TRAPVInstruction )
    }
  }

  def disassemble( cpu: CPU ) = "TRAPV"

}

class TST( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val v = cpu.read( mode, reg, size )

    cpu.N = v < 0
    cpu.Z = v == 0
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = cpu.unary( "TST",size, mode, reg )

}

class UNLK( reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.readA(reg), 7 )
    cpu.writeA( cpu.popAddress, reg )
  }

  def disassemble( cpu: CPU ) = s"UNLK   A$reg"

}
