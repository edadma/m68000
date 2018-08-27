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
      cpu.dregwrite( cpu.add(cpu.read(mode, reg, size), cast(cpu.D(dreg), size), false), reg, size )
    else
      cpu.write( cpu.add(cpu.read(mode, reg, size), cast(cpu.D(dreg), size), false), mode, reg, size )

  }

  def disassemble( cpu: CPU ) = "ADD"

}

class ADDA( areg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.add(cpu.read(mode, reg, size), cpu.readA(areg).asInstanceOf[Int], false), areg )
  }

  def disassemble( cpu: CPU ) = "ADDA"

}

class ADDI( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.add(cpu.read(mode, reg, size), cpu.immediate(size), false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = "ADDI"

}

class ADDQ( data: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.add(cpu.read(mode, reg, size), data, false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = "ADDQ"

}

class Bcc( cond: Int, disp: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    if (cpu.testcc( cond ))
      cpu.jump( disp match {
        case 0 => cpu.fetchShort
        case -1 => cpu.fetchInt//020
        case _ => disp
      } )
  }

  def disassemble( cpu: CPU ) = "Bcc"

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

  def disassemble( cpu: CPU ) = "BCHG"

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

  def disassemble( cpu: CPU ) = "BCLR"

}

class BKPT( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = cpu.breakpoint( vector )

  def disassemble( cpu: CPU ) = "BKPT"

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

  def disassemble( cpu: CPU ) = "BSET"

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

  def disassemble( cpu: CPU ) = "BSR"

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

  def disassemble( cpu: CPU ) = "BTST"

}

class CHK( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    val upper = cpu.read( mode, reg, size )
    val d = cast( cpu.D(dreg), size )

    if (d < 0) {
      cpu.N = true
      cpu.exception( VectorTable.CHKInstruction )
    } else if (d > upper) {
      cpu.N = false
    }
  }

  def disassemble( cpu: CPU ) = "CHK"

}

class CLR( size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( 0, mode, reg, size )
    cpu.Z = true
    cpu.N = false
    cpu.V = false
    cpu.C = false
  }

  def disassemble( cpu: CPU ) = "CLR"

}

class CMP( dreg: Int, size: Size, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.subtract( cpu.read(mode, reg, size), cpu.immediate(size), false )
  }

  def disassemble( cpu: CPU ) = "CMP"

}

class MOVE( size: Size, dreg: Int, dmode: Int, smode: Int, sreg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.flags(0, 0, false, cpu.read(smode, sreg, size), false), dmode, dreg, size )
  }

  def disassemble( cpu: CPU ) = "MOVE"

}

class MOVEQ( reg: Int, data: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.D(reg) = cpu.flags( 0, 0, false, data, false )
  }

  def disassemble( cpu: CPU ) = "MOVEQ"

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

  def disassemble( cpu: CPU ) = "TRAP"

}