//@
package xyz.hyperreal.m68k

import java.io.{File, PrintStream}
import java.nio.file.{Files, Paths}

import jline.console.ConsoleReader
import jline.console.history.FileHistory
import xyz.hyperreal.args.Options


object Main extends App {

  val reader = new ConsoleReader
  val out = new PrintStream( reader.getTerminal.wrapOutIfNeeded(System.out), true )
  lazy val emu = new Emulator
  var enterREPL = true
  val aRegRegex = "a([0-7])"r
  val dRegRegex = "d([0-7])"r

  Options( args ) {
    case "--help" :: _ =>
      println(
        """
          |Motorola 68000 Emulator v0.1
          |Usage:  --help      display this help and exit
          |        -l <file>   load hexdump <file> and enter REPL
          |        -le <file>  load hexdump <file> and execute
        """.trim.stripMargin )
      enterREPL = false
      Nil
    case "-l" :: file :: _ =>
      load( file )
      Nil
    case "-le" :: file :: _ =>
      load( file )
      emu.run( out )
      enterREPL = false
      Nil
    case o :: _ if o startsWith "-" =>
      println( "bad option: " + o )
      enterREPL = false
      Nil
    case _ :: t =>
      t
  }

  if (enterREPL)
    REPL

  def load( file: String ) = emu.load( file )

  //	def save( file: String ) = emu.save( file )

  def waitUntilRunning = {
    while (!emu.cpu.isRunning) {}
  }

  def waitWhileRunning = {
    while (emu.cpu.isRunning) {}
  }

  def REPL {
    var line: String = null
    var reload = ""

    val historyFile = new File( System.getProperty("user.home") + "/.m68k-repl-history" )

    if (!historyFile.exists)
      historyFile.createNewFile

    val history = new FileHistory( historyFile )

    sys.ShutdownHookThread {
      history.flush
    }

    reader.setBellEnabled( false )
    reader.setPrompt( "> " )
    reader.setHistory( history )

//    emu.reregister( "_stdioInt_",
//      (p: String, mem: Memory, cpu: CPU) => {
//        mem add new JLineInt( hex(p), reader )
//      } )
//    emu.reregister( "_stdioHex_",
//      (p: String, mem: Memory, cpu: CPU) => {
//        mem add new JLineHex( hex(p), reader )
//      } )

    def registers = {
      emu.cpu.registers( out )
      emu.cpu.disassemble( true, out )
    }

    def dump( start: Int, lines: Int ) = emu.dump( start, lines, out )

    def disassemble( start: Int, lines: Int ) = emu.disassemble( start, lines, out )

    //		def printBreakpoints = out.println( mach.breakpoints map {case (b, l) => hexShort(b) + (if (l != "") "/" + l else "")} mkString " " )

    def runAndWait {
      emu.run( out )
      waitUntilRunning
      waitWhileRunning
      registers
    }

    out.println( "Motorola 68000 Emulator v0.1" )
    out.println( "Type 'help' for list of commands." )
    out.println

    def interp( command: String ) {
      val com = command.trim split "\\s+" toList

      try {
        com match {
          case List( "breakpoint"|"b" ) =>
            emu.breakpoints( out )
          case List( "breakpoint"|"b", "-" ) =>
            emu.cpu.clearBreakpoints
          case List( "breakpoint"|"b", bp ) if bp startsWith "-" =>
            emu.cpu.clearBreakpoint( emu.target(bp drop 1) )
            emu.breakpoints( out )
          case List( "breakpoint"|"b", bp ) =>
            emu.cpu.setBreakpoint( emu.target(bp) )
            emu.breakpoints( out )
          case List( "disassemble"|"u", addr )  =>
            disassemble( emu.target( addr ), 15 )
          case List( "disassemble"|"u" )  =>
            disassemble( -1, 15 )
          case List( "clear"|"c", addr1, addr2 ) =>
            for (i <- hex( addr1 ) until hex( addr2 ))
              emu.mem.programByte( i, 0 )
          case List( "clear"|"c" ) =>
            emu.mem.clearRAM
          case List( "drop"|"dr", region ) =>
            emu.mem.remove( region )
            out.println( emu.mem )
          case List( "dump"|"d", addr ) =>
            dump( emu.target(addr), 10 )
          case List( "dump"|"d" ) =>
            dump( -1, 10 )
          case List( "execute"|"e", addr2 ) if addr2 startsWith "/" =>
            emu.cpu.setSingleShotBreakpoint( emu.target(addr2 drop 1) )
            emu.run( out )
            registers
          case List( "execute"|"e", addr ) =>
            emu.cpu.jumpTo( emu.target(addr) )
            emu.run( out )
            registers
          case List( "execute"|"e" ) =>
            emu.run( out )
            registers
          case List( "execute"|"e", addr1, addr2 ) =>
            emu.cpu.jumpTo( emu.target(addr1) )
            emu.cpu.setSingleShotBreakpoint( emu.target(if (addr2 startsWith "/") addr2 drop 1 else addr2) )
            emu.run( out )
            registers
          case List( "execute&wait"|"ew", addr ) =>
            emu.cpu.jumpTo( emu.target(addr) )
            runAndWait
          case List( "execute&wait"|"ew" ) =>
            runAndWait
          case List( "help"|"h" ) =>
            println(
              """
                |breakpoint (b) [-]<addr>*        set/clear breakpoint at <addr> (cleared if <addr> preceeded by '-')
                |disassemble (u) [<addr>*]        print disassembled code at <addr> or where left off
                |clear (c) [<addr1>* <addr2>*]    clear RAM, optionally from <addr1> up to but not including <addr2>
                |drop (dr) <region>               drop memory <region>
                |dump (d) [<addr>*]               print memory at <addr> or where left off
                |execute (e) [<addr>*]            execute instructions starting from current PC or <addr>
                |execute & wait (ew) [<addr>*]    execute instructions starting from current PC or <addr> and wait to finish
                |help (h)                         print this summary
                |load (l) <file>                  clear ROM, load SREC <file>, and reset CPU
                |memory (m)                       print memory map
                |memory (m) <addr>* <data>*...    write <data> (space separated bytes) to memory at <addr>
                |quit (q)                         exit the REPL
                |registers (r)                    print CPU registers
                |registers (r) <reg> <val>*       set CPU <reg>ister to <val>ue
                |reload (rl)                      redo last 'load' or 'assemble' command
                |reset (re)                       reset CPU registers setting PC from reset vector
                |setup                            setup toolchain scripts, startup code, etc.
                |step (s) [<addr>*]               execute only next instruction at current PC or <addr>
                |stop (st)                        stop code execution
                |save (sa) <file>                 save all ROM contents to SREC file
                |symbols (sy)                     print symbol table
                |symbols (sy) <symbol> <val>*     add <symbol> with associated <val>ue to symbol table
                |trace (t) on/off                 turn CPU trace on or off
                |* can either be a hexadecimal value or label (optionally followed by a colon)
              """.trim.stripMargin )
          case List( "load"|"l", file ) =>
            reload = command
            load( file )
          case ("memory"|"m") :: addr :: data =>
            val addr1 = emu.target( addr )

            for ((d, i) <- data map emu.target zipWithIndex)
              emu.program( addr1 + i, d )

            dump( addr1, (data.length + addr1%16)/16 + 1 )
          case List( "memory"|"m" ) =>
            out.println( emu.mem )
          case List( "quit"|"q" ) =>
            emu.stop
            emu.mem.removeDevices
            sys.exit
          case List( "register"|"r", reg, value ) =>
            val n = emu.target( value )

            reg.toLowerCase match {
              case aRegRegex( r ) => emu.cpu.writeA( n, r.toInt )
              case dRegRegex( r ) => emu.cpu.D(r.toInt) = n
              case "sr" => emu.cpu.toSR( n )
              case "ccr" => emu.cpu.toCCR( n )
              case "pc" => emu.cpu.jumpTo( n )
            }

            registers
          case List( "registers"|"r" ) =>
            registers
          case List( "reload"|"rl" ) =>
            interp( reload )
          case List( "reset"|"re" ) =>
            emu.reset
            registers
          case List( "setup" ) =>
            def copy( file: String ): Unit = {
              val res = getClass.getResourceAsStream( s"setup/$file" )

              Files.copy( res, Paths get file )
              res.close
            }

            List( "asm", "gcc", "ld", "ldscript", "main.c", "services.h",
              "services.s", "startup.s", "syscalls.c"
            ) foreach copy
          case List( "step"|"s", addr ) =>
            emu.cpu.jumpTo( emu.target(addr) )
            emu.step
            registers
          case List( "step"|"s" ) =>
            emu.step
            registers
          case List( "step over"|"so" ) =>
            emu.stepOver( out )
            registers
          case List( "stop"|"st" ) =>
            emu.stop
            waitWhileRunning
            registers
          case List( "symbols"|"sy", symbol, value ) =>
            emu.symbols += (symbol -> emu.target( value ))
          case List( "symbols"|"sy" ) =>
            out.println( "name            value" )
            out.println( "----            -----" )

            for ((s, v) <- emu.symbols.toList sortBy (_._1))
              out.println( f"$s%-15s $v%6x" )
          case List( "trace"|"t", "on" ) =>
            emu.cpu.trace = true
            emu.cpu.traceout = out
          case List( "trace"|"t", "off" ) =>
            emu.cpu.trace = false
            emu.cpu.traceout = Console.out
          case Nil|List( "" ) =>
          case _ => out.println( "error interpreting command" )
        }
      } catch {
        case e: Exception =>
          //					out.println( e )
          e.printStackTrace( out )
      }
    }

    while ({line = reader.readLine; line != null}) {
      interp( line )
      out.println
    }
  }
}