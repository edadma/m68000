//@
package xyz.hyperreal.m68k

import java.io.{File, OutputStream, PrintStream}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap


class Emulator {

  val mem =
    new Memory {
      def init {
        removeDevices
        regions.clear
        add( new ROM("program", 0, 0xFFFF) )
        add( new RAM("ram", 0x10000, 0x10000 + 2*1024*1024 - 1) )
      }
    }
  val cpu = new CPUWithServices( mem )

  private val registry = new HashMap[String, (String, Memory, CPU) => Unit]

  register( "_ram_",
    (p: String, mem: Memory, cpu: CPU) => {
      mem.removeRAM

      val block = """(\p{XDigit}+)\-(\p{XDigit}+)"""r

      for ((m, ind) <- block findAllMatchIn p zipWithIndex)
        mem add new RAM( "main" + ind, hex(m group 1), hex(m group 2) )
    }	)
  register( "_rom_",
    (p: String, mem: Memory, cpu: CPU) => {
      mem.removeROM

      val block = """(\p{XDigit}+)\-(\p{XDigit}+)"""r

      for ((m, ind) <- block findAllMatchIn p zipWithIndex)
        mem add new ROM( "main" + ind, hex(m group 1), hex(m group 2) )
    }	)

  var dumpcur: Int = 0
  var discur: Int = 0
  var symbols = Map[String, Any]()
  var reverseSymbols = Map[Any, String]()
  var segments = TreeMap[Int, (String, Int)]()

  def register( name: String, installer: (String, Memory, CPU) => Unit ) {
    if (registry contains name)
      sys.error( "device installer already registered: " + name )

    registry(name) = installer
  }

  def deregister( name: String ) {
    if (!(registry contains name))
      sys.error( "device installer not registered: " + name )

    registry -= name
  }

  def reregister( name: String, installer: (String, Memory, CPU) => Unit ) {
    if (!(registry contains name))
      sys.error( "device installer not registered: " + name )

    registry(name) = installer
  }

  def run {
    cpu.run
    cpu.resetSignal
  }

  def reset = {
    cpu.reset
    discur = cpu.memoryReadAddress( VectorTable.PC )
  }

  def step = cpu.step

  //	def stop = cpu.stop

  def readByte( addr: Int ) = mem.readByte( addr )

  def readWord( addr: Int ) = mem.readShort( addr )

  def program( addr: Int, b: Int ) = mem.programByte( addr, b )

  def display( label: String ) =
    label indexOf '.' match {
      case -1 => label
      case dot => label substring dot
    }

  //	def reference( target: Int, zp: Boolean ) =
  //		reverseSymbols get target match {
  //			case None => "$" + (if (zp) hexByte( target ) else hexWord( target ))
  //			case Some( l ) => display( l )
  //		}

  def target( ref: String ) =
    if (isHex( ref ))
      hex( ref )
    else
      -1
  //			symbols get (if (ref endsWith ":") ref dropRight 1 else ref) match {
  //				case Some( t: Int ) => t
  //				case None => sys.error( "unknown label: " + ref )
  //				case Some( s ) => sys.error( "symbol not an integer: " + s )
  //			}

  def disassemble( start: Int, lines: Int, out: PrintStream ) {
    if (start > -1)
      discur = start

    val cur = discur

    def disassemble( out: PrintStream ) {
      val pc = cpu.PC

      for (_ <- 1 to lines) {
        cpu.PC = discur
        discur += cpu.disassemble( false, out )
      }

      cpu.PC = pc
    }

    disassemble( new PrintStream(new OutputStream() {
                def write( b: Int ) {
                    //DO NOTHING
                }
            }))
    discur = cur
    disassemble( out )
  }

  def load( file: String ) {
    if (cpu.isRunning)
      sys.error( "can't load while running" )

    mem.removeROM
    mem.reset
    SREC( mem, new File(file + ".srec") )
    cpu.symbols = MapFileReader(io.Source.fromFile(s"$file.map"))._2

    val (code, vars) = DebugFileReader(io.Source.fromFile(s"$file.debug"))

    cpu.debug = code
    cpu.symbols ++= vars
    //		clearBreakpoints
    reset
  }

  //	def save( file: String ) = 	SREC.write( mem, new File(file), file.getBytes.toVector )

  def dump( start: Int, lines: Int, out: PrintStream ) = {
    val addr =
      if (start == -1)
        dumpcur - dumpcur%16
      else
        start - start%16

    def printByte( b: Option[Int] ) =
      if (b isEmpty)
        out.print( "-- " )
      else
        out.print( "%02x ".format(b.get&0xFF).toUpperCase )

    def printChar( c: Option[Int] ) = out.print( if (c.nonEmpty && ' ' <= c.get && c.get <= '~') c.get.asInstanceOf[Char] else '.' )

    def read( addr: Int ) =
      if (mem.addressable( addr ) && mem.memory( addr ))
        Some( mem.readByte(addr) )
      else
        None

    for (line <- addr until ((addr + 16*lines) min ADDRESS_RANGE) by 16) {
      out.print( "%8x  ".format(line).toUpperCase )

      for (i <- line until ((line + 16) min ADDRESS_RANGE)) {
        if (i%16 == 8)
          out.print( ' ' )

        printByte( read(i) )
      }

      val bytes = ((line + 16) min 0x10000) - line

      out.print( " "*(((16 - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)).toInt) )

      for (i <- line until ((line + 16) min ADDRESS_RANGE))
        printChar( read(i) )

      out.println
    }

    dumpcur = addr + 16*lines
  }

  //	def clearBreakpoints = cpu.breakpoints = Set[Int]()
  //
  //	def setBreakpoint( addr: Int ) = cpu.breakpoints += addr
  //
  //	def clearBreakpoint( addr: Int ) = cpu.breakpoints -= addr
  //
  //	def breakpoints = cpu.breakpoints.toList map (b => (b, (if (reverseSymbols contains b) reverseSymbols(b) else "")))
}