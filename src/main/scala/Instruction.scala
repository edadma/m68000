package xyz.hyperreal.m68k


abstract class Instruction extends (CPU => Unit) {

  def disassemble( cpu: CPU ): String

  def illegal( cpu: CPU ) = cpu.problem( "illegal instruction" )

}

object IllegalInstruction extends Instruction {

  def apply( cpu: CPU ) = illegal( cpu )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

class ADDQ( data: Int, size: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    cpu.write( cpu.add(cpu.read(mode, reg, size), data, false), mode, reg, size )
  }

  def disassemble( cpu: CPU ) = "ADDQ"

}

class BKPT( vector: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = cpu.breakpoint( vector )

  def disassemble( cpu: CPU ) = "BKPT"

}