//@
package xyz.hyperreal.m68k.dev

import xyz.hyperreal.m68k.Device


class Clock( val start: Int ) extends Device {

  val name = "RTC"
  val size = 5

  var control = 0
  var seconds = 0
  var minutes = 0
  var hours = 0
  var date = 0
  var month = 0
  var year = 0
  var day = 0

  def writeByte( addr: Int, value: Int ): Unit = {
    addr - start match {
      case Register.control =>
        value match {
          case Control.set =>
        }
    }
  }

  def readByte( addr:  Int ): Int = ???

  object Control {
    val set = 0
    val read = 1
  }

  object Register {

    val control = 0x00
    val seconds = 0x01
    val minutes = 0x02
    val hours = 0x03
    val date = 0x04
    val month = 0x05
    val year = 0x06
    val day = 0x08

  }

}