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
    cpu.add( )
  }

  def disassemble( cpu: CPU ) = "ADDQ"

}