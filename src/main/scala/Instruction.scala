package xyz.hyperreal.m68000


abstract class Instruction extends (CPU => Unit) {

  def disassemble( cpu: CPU ): String

  def illegal( cpu: CPU ) = cpu.problem( "illegal instruction" )

}

object IllegalInstruction extends Instruction {

  def apply( cpu: CPU ) = illegal( cpu )

  def disassemble( cpu: CPU ): String = "ILLEGAL"

}

object ALU {

  def flags( cpu: CPU, overflow: Int, carry: Int, zero: Boolean, res: Int ): Unit = {
    cpu.V = (overflow&0x80000000L) != 0
    cpu.C = (carry&0x80000000L) != 0
    cpu.X = cpu.C
    cpu.Z = zero
    cpu.N = res < 0
  }

  def add( cpu: CPU, s: Int, d: Int, extended: Boolean ) = {
    val r = s + d
    val z = if (extended) r == 0 && cpu.Z else r == 0

    flags( cpu, s&d&(~r)|(~s)&(~d)&r, s&d|(~r)&d|s&(~r), z, r )
    r
  }

}

class ADDQ( data: Int, size: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {
    ALU.add( cpu, )
  }

  def disassemble( cpu: CPU ) = "ADDQ"

}