//@
package xyz.hyperreal.m68k


abstract class Instruction extends (CPU => Unit) {

  def disassemble( cpu: CPU ): String

  def illegal( cpu: CPU ) = cpu.problem( "illegal instruction" )

}

object IllegalInstruction extends Instruction {

  def apply( cpu: CPU ) = illegal( cpu )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ADD( dreg: Int, dest: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dest == 0)
      cpu.dregwrite( cpu.add(cpu.read(mode, reg, size), cpu.readD(reg, size), false), reg, size )
    else
      cpu.write( cpu.add(cpu.read(mode, reg, size), cpu.readD(reg, size), false), mode, reg, size )

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
    cpu.write( cpu.add(cpu.read(mode, reg, size), cpu.immediate(size), false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"ADDI"

}

class ADDQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.add(cpu.read(mode, reg, size), data, false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"ADDQ"

}

class AND( dreg: Int, dest: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (dest == 0)
      cpu.dregwrite( cpu.and(cpu.read(mode, reg, size), cpu.readD(dreg, size), false), reg, size )
    else
      cpu.write( cpu.and(cpu.read(mode, reg, size), cpu.readD(dreg, size), false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"AND"

}

class ANDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.and(cpu.read(mode, reg, size), cpu.immediate(size), false), mode, reg, size )
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

class Bcc( cond: Int, disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val pc = cpu.PC

    if (cpu.testcc( cond ))
      cpu.jump( pc + disp match {
        case 0 => cpu.fetchShort
        case -1 => cpu.fetchInt//020
        case _ => disp
      } )
  }

  def disassemble( cpu: CPU ) = s"Bcc"

}

class BCHG( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val data = cpu.read( mode, reg, BitSize )
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.Z = testBit( data, bit )
    cpu.write( flipBit(data, bit), mode, reg, BitSize )
  }

  def disassemble( cpu: CPU ) = s"BCHG"

}

class BCLR( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val data = cpu.read( mode, reg, BitSize )
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.Z = testBit( data, bit )
    cpu.write( clearBit(data, bit), mode, reg, BitSize )
  }

  def disassemble( cpu: CPU ) = s"BCLR"

}

class BKPT( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = cpu.breakpoint( vector )

  def disassemble( cpu: CPU ) = s"BKPT"

}

class BSET( breg: Option[Int], mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val data = cpu.read( mode, reg, BitSize )
    val bit =
      breg match {
        case None => cpu.fetchByte
        case Some( b ) => b
      }

    cpu.Z = testBit( data, bit )
    cpu.write( setBit(data, bit), mode, reg, BitSize )
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

    cpu.push( cpu.PC.asInstanceOf[Int], IntSize )
    cpu.jump( jump )
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
    cpu.add( cpu.immediate(size), cpu.read(mode, reg, size), false )
  }

  def disassemble( cpu: CPU ) = s"CMPI"

}

class CMPM( size: Size, rx: Int, ry: Int ) extends Instruction with Addressing {

  def apply( cpu: CPU ): Unit = {
    cpu.add( cpu.read(AddressRegisterIndirectPostincrement, rx, size), cpu.read(AddressRegisterIndirectPostincrement, ry, size), false )
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

      cpu.dregwrite( res, reg, ShortSize )

      if (res == -1)
        cpu.PC += 2
      else
        cpu.jump( pc + cpu.fetchShort )
    }
  }

  def disassemble( cpu: CPU ) = s"DBcc"

}

class EOR( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.eor(cpu.read(mode, reg, size), cpu.readD(dreg, size), false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = s"EOR"

}

class MOVE( size: Size, dreg: Int, dmode: Int, smode: Int, sreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.flags(0, 0, false, cpu.read(smode, sreg, size), false), dmode, dreg, size )
  }

  def disassemble( cpu: CPU ) = s"MOVE"

}

class MOVEQ( reg: Int, data: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(reg) = cpu.flags( 0, 0, false, data, false )
  }

  def disassemble( cpu: CPU ) = s"MOVEQ"

}

class TRAP( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    //cpu.SSP -= 2
    if (!cpu.trap( vector )) {
      cpu.SSP -= 4
      cpu.memoryWrite( cpu.PC.asInstanceOf[Int], cpu.SSP, IntSize, true )
      cpu.jump( cpu.VBR + (vector<<5) )
    }
  }

  def disassemble( cpu: CPU ) = s"TRAP"

}