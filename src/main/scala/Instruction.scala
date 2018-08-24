package xyz.hyperreal.m68k


abstract class Instruction extends (CPU => Unit) {

  def disassemble( cpu: CPU ): String

  def illegal( cpu: CPU ) = cpu.problem( "illegal instruction" )

}

object IllegalInstruction extends Instruction {

  def apply( cpu: CPU ) = illegal( cpu )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ADDA( size: Size, mode: Int, reg: Int, areg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.writeA( cpu.read(mode, reg, size), areg )
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
    if (cpu.test( cond ))
      cpu.jump( disp match {
        case 0 => cpu.fetchShort
        case -1 => cpu.fetchInt//020
        case _ => disp
      } )
  }

  def disassemble( cpu: CPU ) = "Bcc"

}

class BKPT( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = cpu.breakpoint( vector )

  def disassemble( cpu: CPU ) = "BKPT"

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