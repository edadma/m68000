//@
package xyz.hyperreal.m68k


case class Assembly( mnmonic: String, size: Option[Size], operands: List[Int] )

abstract class Instruction extends (CPU => Unit) with Addressing {

  var opcode = 0

  val assembly: Assembly = null

  def disassemble( cpu: CPU ): String

}

object ILLEGAL extends Instruction {

  def apply( cpu: CPU ) =
    if (!cpu.illegal)
      cpu.exception( -1, VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ABCD( y: Int, r: Int, x: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    if (r == 0)
      cpu.writeD( cpu.abcd(cpu.readD(x, ByteSize), cpu.readD(y, ByteSize)), x, ByteSize )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, x, ByteSize )( cpu.abcd(_, cpu.read(AddressRegisterIndirectPredecrement, y, ByteSize)) )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("ABCD")}" + (if (r == 0) s"D$y, D$x" else s"-(A$y), -(A$x)")

}

class ADD( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.add(cpu.read(mode, reg, size), cpu.readD(dreg, size), size, false), dreg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.add(_, cpu.readD(dreg, size), size, false) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "ADD", size, mode, reg, dir, dreg )

}

class ADDA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.add(cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], size, false), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "ADDA", size, mode, reg, areg )

}

class ADDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.immediate(size)

    cpu.readWrite( mode, reg, size )( cpu.add(_, imm, size, false) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "ADDI", size, mode, reg )

}

class ADDQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.add(_, data, size, false) )
  }

  def disassemble( cpu: CPU ) = mnemonic( "ADDQ", size ) + s"#$data, ${cpu.operand(mode, reg, size)}"

}

class ADDX( regx: Int, size: Size, rm: Int, regy: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (rm == 0)
      cpu.writeD( cpu.addx(cpu.readD(regx, size), cpu.readD(regy, size), size), regx, size )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, regy, size )( cpu.addx(_, cpu.read(AddressRegisterIndirectPredecrement, regx, size), size) )
  }

  def disassemble( cpu: CPU ) =
    if (rm == 0)
      mnemonic( "ADDX", size ) + s"D$regy, D$regx"
    else
      mnemonic( "ADDX", size ) + cpu.operand(AddressRegisterIndirectPredecrement, regy, size ) +
        ", " + cpu.operand(AddressRegisterIndirectPredecrement, regx, size )

}

class AND( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.and(cpu.read(mode, reg, size), cpu.readD(dreg, size)), dreg, size )
    else
      cpu.readWrite( mode, reg, size )( cpu.and(_, cpu.readD(dreg, size)) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "AND", size, mode, reg, dir, dreg )

}

class ANDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.immediate(size)

    cpu.readWrite( mode, reg, size )( cpu.and(_, imm) )
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

  def disassemble( cpu: CPU ) = s"${mnemonic("ANDI")}#${cpu.fetchByte}, CCR"

}

object ANDItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      cpu.toSR( cpu.fromSR & cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("ANDI")}#${cpu.fetchShort}, SR"

}

class ASMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( x => if (dir == 0) cpu.asr(1, x, ShortSize) else cpu.asl(1, x, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"AS$d    ${cpu.operand(mode, reg, ShortSize)}"
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
    val addr = cpu.displacement( disp )

    if (cpu.testcc( cond ))
      cpu.jumpTo( addr )
  }

  def disassemble( cpu: CPU ) = {
    mnemonic( s"B${Conditional( cond )}" ) + cpu.relative(disp)
  }

}

class BCHG( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit = {
      val b =
        breg match {
          case None => cpu.fetchByte
          case Some(r) => cpu.D(r)
        }

      if (mode > 2) b & 7 else b & 0x1F
    }

    cpu.readWrite( mode, reg, BitSize ){ x =>
      cpu.Z = !testBit( x, bit )
      flipBit( x, bit )
    }
  }

  def disassemble( cpu: CPU ) =
    s"${mnemonic("BCHG")}" +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

}

class BCLR( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit = {
      val b =
        breg match {
          case None => cpu.fetchByte
          case Some(r) => cpu.D(r)
        }

      if (mode > 2) b & 7 else b & 0x1F
    }

    cpu.readWrite( mode, reg, BitSize ) { x =>
      cpu.Z = !testBit( x, bit )
      clearBit( x, bit )
    }
  }

  def disassemble( cpu: CPU ) =
    s"${mnemonic("BCLR")}" +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

}

class BKPT( bkpt: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (!cpu.breakpoint( bkpt ))
      cpu.exception( -1, VectorTable.illegalInstruction )

  def disassemble( cpu: CPU ) = s"${mnemonic("BKPT")}#$bkpt"

}

class BSET( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val bit = {
      val b =
        breg match {
          case None => cpu.fetchByte
          case Some(r) => cpu.D(r)
        }

      if (mode > 2) b & 7 else b & 0x1F
    }

    cpu.readWrite( mode, reg, BitSize ) { x =>
      cpu.Z = !testBit( x, bit )
      setBit(x, bit)
    }
  }

  def disassemble( cpu: CPU ) =
    s"${mnemonic("BSET")}" +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

}

class BSR( disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val addr = cpu.displacement( disp )

    cpu.pushAddress( cpu.PC )
    cpu.jumpTo( addr )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("BSR")}${cpu.relative(disp)}"

}

class BTST( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val data = cpu.read( mode, reg, BitSize )
    val bit = {
      val b =
        breg match {
          case None => cpu.fetchByte
          case Some(r) => cpu.D(r)
        }

      if (mode > 2) b & 7 else b & 0x1F
    }

    cpu.Z = !testBit( data, bit )
  }

  def disassemble( cpu: CPU ) =
    s"${mnemonic("BTST")}" +
      (breg match {
        case None => s"#${cpu.fetchByte}, ${cpu.operand( mode, reg )}"
        case Some( r ) => s"D$r, ${cpu.operand( mode, reg )}"
      })

}

class CHK( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val upper = cpu.read( mode, reg, size )
    val d = cpu.readD( dreg, size )

    if (d < 0) {
      cpu.N = true
      cpu.exception( -1, VectorTable.CHKInstruction )
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
    cpu.subtract( cpu.read(mode, reg, size), cast(cpu.D(dreg), size), size, false )
  }

  def disassemble( cpu: CPU ) = cpu.binaryDstD( "CMP", size, mode, reg, dreg )

}

class CMPA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], size, false )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "CMPA", size, mode, reg, areg )

}

class CMPI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.immediate(size), cpu.read(mode, reg, size), size, false )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "CMPI", size, mode, reg )

}

class CMPM( size: Size, rx: Int, ry: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.read(AddressRegisterIndirectPostincrement, ry, size), cpu.read(AddressRegisterIndirectPostincrement, rx, size), size, false )
  }

  def disassemble( cpu: CPU ) = mnemonic( "CMPM", size ) + s"(A$ry)+, (A$rx)+"

}

class DBcc( cond: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.testcc( cond ))
      cpu.PC += 2
    else {
      val res = cpu.readD( reg, ShortSize ) - 1

      cpu.writeD( res, reg, ShortSize )

      if (res == -1)
        cpu.PC += 2
      else
        cpu.jumpTo( cpu.PC + cpu.fetchShort )
    }
  }

  def disassemble( cpu: CPU ) = mnemonic( s"DB${Conditional( cond )}" ) + s"D$reg, ${cpu.target( cpu.PC + cpu.fetchShort )}"

}

class DIVS( dreg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val a = cpu.D(dreg)
    val b = cpu.read( mode, reg, ShortSize )

    if (b == 0)
      cpu.exception( -1, VectorTable.integerDivideByZero )
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
      cpu.exception( -1, VectorTable.integerDivideByZero )
    else {
      val q = a/b

      if (q > 0xFFFF)
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
    cpu.readWrite( mode, reg, size )( cpu.eor(_, cpu.readD(dreg, size)) )
  }

  def disassemble( cpu: CPU ) = cpu.binarySrcD( "EOR", size, mode, reg, dreg )

}

class EORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.immediate(size)

    cpu.readWrite( mode, reg, size )( cpu.eor(_, imm) )
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

  def disassemble( cpu: CPU ) = s"${mnemonic("EORI")}#${cpu.fetchByte}, CCR"

}

object EORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR ^ cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("EORI")}#${cpu.fetchShort}, SR"

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
    s"${mnemonic("EXG")}" +
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
    cpu.jumpTo( cpu.address(mode, reg) )
  }

  def disassemble( cpu: CPU ) = mnemonic( "JMP" ) + cpu.targetea( mode, reg )

}

class JSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val addr = cpu.address( mode, reg )

    cpu.pushAddress( cpu.PC )
    cpu.jumpTo( addr )
  }

  def disassemble( cpu: CPU ) = mnemonic( "JSR" ) + cpu.targetea( mode, reg )

}

class LEA( areg: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.address(mode, reg), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "LEA", mode, reg, areg, false )

}

object LINEA extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.lineA)
      cpu.exception( -1, VectorTable.lineA )
  }

  def disassemble( cpu: CPU ) = s"LINEA"

}

object LINEF extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.lineF)
      cpu.exception( -1, VectorTable.lineF )
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

  def disassemble( cpu: CPU ) = s"${mnemonic("LINK")}A$reg, #${cpu.immediate(ShortSize)}"

}

class LSMem( dir: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.lsr(1, o, ShortSize) else cpu.lsl(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"LS$d    ${cpu.operand(mode, reg, ShortSize)}"
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

  def disassemble( cpu: CPU ) = mnemonic( "MOVE", size ) + s"${cpu.operand(smode, sreg, size)}, ${cpu.operand(dmode, dreg, size)}"

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
                cpu.readA( 7 - idx ).asInstanceOf[Int]
              else
                cpu.D( 7 - (idx - 8) )
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
            val rs = regs( cpu )
            var addr = cpu.address(mode, reg)

            for (idx <- rs) {
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
          regs(cpu) map { idx => if (idx < 8) s"A${7 - idx}" else s"D${7 - (idx - 8)}" }
        case _ =>
          regs(cpu) map { idx => if (idx < 8) s"D$idx" else s"A${idx - 8}" }
      }

    mnemonic( "MOVEM", size ) +
      (if (dir == 0)
        ranges( rs sorted ) + ", " + cpu.operand( mode, reg )
      else
        cpu.operand( mode, reg ) + ", " + ranges( rs ))
  }

}

class MOVEfromSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.fromSR, mode, reg, ShortSize )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("MOVE")}SR, ${cpu.operand( mode, reg )}"

}

class MOVEtoCCR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.read(mode, reg, ByteSize) )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("MOVE")}${cpu.operand( mode, reg )}, CCR"

}

class MOVEtoSR( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      cpu.toSR( cpu.read(mode, reg, ShortSize) )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("MOVE")}${cpu.operand( mode, reg, ShortSize )}, SR"

}

class MOVEQ( reg: Int, data: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(reg) = cpu.flags( 0, 0, false, data, false )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("MOVEQ")}#$data, D$reg"

}

class MOVEUSP( dir: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.supervisor)
      if (dir == 0)
        cpu.USP = cpu.readA( reg )
      else
        cpu.writeA( cpu.USP, reg )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("MOVE")}" + (if (dir == 0) s"A$reg, USP" else s"USP, A$reg")

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
      cpu.writeD( cpu.or(cpu.read(mode, reg, size), cpu.readD(dreg, size)), dreg, size )
    else
      cpu.readWrite( mode, dreg, size )( cpu.or(_, cpu.readD(dreg, size)) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "OR", size, mode, reg, dir, dreg )

}

class ORI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.immediate(size)

    cpu.readWrite( mode, reg, size )( cpu.or(_, imm) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "ORI", size, mode, reg )

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

  def disassemble( cpu: CPU ) = s"${mnemonic("ORI")}#${cpu.fetchByte}, CCR"

}

object ORItoSR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toSR( cpu.fromSR | cpu.fetchShort )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("ORI")}#${cpu.fetchShort}, SR"

}

class PEA( mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.pushAddress( cpu.address(mode, reg) )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("PEA")}${cpu.operand( mode, reg, locals = false )}"

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

    s"RO$d    ${cpu.operand(mode, reg, ShortSize)}"
  }

}

class ROReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) if (count == 0) 8 else count else cpu.readD( count, size )
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
    cpu.readWrite( mode, reg, ShortSize )( o => if (dir == 0) cpu.roxr(1, o, ShortSize) else cpu.roxl(1, o, ShortSize) )
  }

  def disassemble( cpu: CPU ) = {
    val d = if (dir == 0) 'R' else 'L'

    s"ROX$d    ${cpu.operand(mode, reg, ShortSize)}"
  }

}

class ROXReg( count: Int, dir: Int, size: Size, ir: Int, dreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val c = if (ir == 0) if (count == 0) 8 else count else cpu.readD( count, size )
    val operand = cpu.readD(dreg, size)

    cpu.writeD( if (dir == 0) cpu.roxr(c, operand, size) else cpu.roxl(c, operand, size), dreg, size )
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
      cpu.jumpTo( cpu.popAddress )
      cpu.toSR( cpu.pop(ShortSize) )
    }

  def disassemble( cpu: CPU ) = "RTE"

}

object RTR extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.toCCR( cpu.pop(ShortSize) )
    cpu.jumpTo( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = "RTR"

}

object RTS extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.jumpTo( cpu.popAddress )
  }

  def disassemble( cpu: CPU ) = "RTS"

}

class SBCD( y: Int, r: Int, x: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    if (r == 0)
      cpu.writeD( cpu.sbcd(cpu.readD(y, ByteSize), cpu.readD(x, ByteSize)), x, ByteSize )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, x, ByteSize )( cpu.sbcd(cpu.read(AddressRegisterIndirectPredecrement, y, ByteSize), _) )
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("SBCD")}" + (if (r == 0) s"D$y, D$x" else s"-(A$y), -(A$x)")

}

class Scc( cond: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( if (cpu.testcc( cond )) 0xFF else 0, mode, reg, ByteSize )
  }

  def disassemble( cpu: CPU ) = {
    mnemonic( s"S${Conditional( cond )}" ) + cpu.operand( mode, reg )
  }

}

object STOP extends Instruction {

  def apply( cpu: CPU ): Unit =
    if (cpu.supervisor) {
      cpu.toSR( cpu.fetchShort )
      cpu.stopped = true
    }

  def disassemble( cpu: CPU ) = s"${mnemonic("STOP")}#${cpu.fetchShort}"

}

class SUB( dreg: Int, dir: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dir == 0)
      cpu.writeD( cpu.subtract(cpu.read(mode, reg, size), cpu.readD(dreg, size), size, false), dreg, size )
    else
      cpu.readWrite( mode, reg, size)( cpu.subtract(cpu.readD(dreg, size), _, size, false) )
  }

  def disassemble( cpu: CPU ) = cpu.binary( "SUB", size, mode, reg, dir, dreg )

}

class SUBA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.subtract(cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], size, false), areg )
  }

  def disassemble( cpu: CPU ) = cpu.binaryA( "SUBA", size, mode, reg, areg )

}

class SUBI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val imm = cpu.immediate(size)

    cpu.readWrite( mode, reg, size )( cpu.subtract(imm, _, size, false) )
  }

  def disassemble( cpu: CPU ) = cpu.immediate( "SUBI", size, mode, reg )

}

class SUBQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.readWrite( mode, reg, size )( cpu.subtract(data, _, size, false) )
  }

  def disassemble( cpu: CPU ) = mnemonic( "SUBQ", size ) + s"#$data, ${cpu.operand(mode, reg, size)}"

}

class SUBX( regx: Int, size: Size, rm: Int, regy: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (rm == 0)
      cpu.writeD( cpu.subtractx(cpu.readD(regx, size), cpu.readD(regy, size), size), regy, size )
    else
      cpu.readWrite( AddressRegisterIndirectPredecrement, regy, size )( cpu.subtractx(cpu.read(AddressRegisterIndirectPredecrement, regx, size), _, size) )
  }

  def disassemble( cpu: CPU ) =
    if (rm == 0)
      mnemonic( "SUBX", size ) + s"D$regx, D$regy"
    else
      mnemonic( "SUBX", size ) + cpu.operand(AddressRegisterIndirectPredecrement, regx, size ) +
        ", " + cpu.operand(AddressRegisterIndirectPredecrement, regy, size )

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

  def disassemble( cpu: CPU ) = s"${mnemonic("SWAP")}D$reg"

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

  def disassemble( cpu: CPU ) = s"${mnemonic("TAS")}${cpu.operand(mode, reg, ByteSize)}"

}

class TRAP( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (!cpu.trap( vector )) {
      cpu.exception( -1, VectorTable.TRAPInstruction + (vector<<2) )
    }
  }

  def disassemble( cpu: CPU ) = s"${mnemonic("TRAP")}#$vector"

}

object TRAPV extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.V) {
      cpu.exception( -1, VectorTable.TRAPVInstruction )
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

  def disassemble( cpu: CPU ) = s"${mnemonic("UNLK")}A$reg"

}
