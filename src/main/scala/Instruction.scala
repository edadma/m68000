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

  def flags( cpu: CPU, overflow: Int, carry: Int, zero: Boolean ): Unit = {
    cpu.V = (res&0xFFFFFFFF00000000L) != 0
    cpu.C = cpu.V
    cpu.X = cpu.C
    cpu.Z = (res&0xFFFFFFFF) == 0
    cpu.N = res < 0
  }

  def add( cpu: CPU, a: Int, b: Int ) = {
    val res = a.asInstanceOf[Long] + b

    flags( cpu, res )
    res.asInstanceOf[Int]
  }

}

class ADDQ( data: Int, size: Int, mode: Int, reg: Int ) extends Instruction {

  def apply( cpu: CPU ): Unit = {

  }

  def disassemble( cpu: CPU ) = "ADDQ"

}