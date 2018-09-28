package xyz.hyperreal.m68k

import org.scalatest._
import prop.PropertyChecks


class BitShiftTests extends FreeSpec with PropertyChecks with Matchers with Testing {
	
	"roxl" in {
    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(0, 1, ByteSize), ByteSize )
    } shouldBe (1, "")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(0, 1, ByteSize), ByteSize )
    } shouldBe (1, "xc")

    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(1, 1, ByteSize), ByteSize )
    } shouldBe (2, "")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(1, 1, ByteSize), ByteSize )
    } shouldBe (3, "")
    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(1, 0x81, ByteSize), ByteSize )
    } shouldBe (2, "xc")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(1, 0x81, ByteSize), ByteSize )
    } shouldBe (3, "xc")

    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(2, 1, ByteSize), ByteSize )
    } shouldBe (4, "")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(2, 1, ByteSize), ByteSize )
    } shouldBe (6, "")
    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(2, 0x81, ByteSize), ByteSize )
    } shouldBe (5, "")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(2, 0x81, ByteSize), ByteSize )
    } shouldBe (7, "")

    test { cpu =>
      cpu.X = false
      cast( cpu.roxl(3, 0xC1, ByteSize), ByteSize )
    } shouldBe (0x0B, "")
    test { cpu =>
      cpu.X = true
      cast( cpu.roxl(3, 0xC1, ByteSize), ByteSize )
    } shouldBe (0x0F, "")
	}

  "roxr" in {
    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(0, 0x80, ByteSize), ByteSize )
    } shouldBe (0x80, "")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(0, 0x80, ByteSize), ByteSize )
    } shouldBe (0x80, "xc")

    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(1, 0x80, ByteSize), ByteSize )
    } shouldBe (0x40, "")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(1, 0x80, ByteSize), ByteSize )
    } shouldBe (0xC0, "")
    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(1, 0x81, ByteSize), ByteSize )
    } shouldBe (0x40, "xc")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(1, 0x81, ByteSize), ByteSize )
    } shouldBe (0xC0, "xc")

    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(2, 0x80, ByteSize), ByteSize )
    } shouldBe (0x20, "")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(2, 0x80, ByteSize), ByteSize )
    } shouldBe (0x60, "")
    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(2, 0x81, ByteSize), ByteSize )
    } shouldBe (0xA0, "")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(2, 0x81, ByteSize), ByteSize )
    } shouldBe (0xE0, "")

    test { cpu =>
      cpu.X = false
      ucast( cpu.roxr(3, 0x83, ByteSize), ByteSize )
    } shouldBe (0xD0, "")
    test { cpu =>
      cpu.X = true
      ucast( cpu.roxr(3, 0x83, ByteSize), ByteSize )
    } shouldBe (0xF0, "")
  }

  "lsr" in {
    test { cpu =>
      ucast( cpu.lsr(0, 0x80, ByteSize), ByteSize )
    } shouldBe (0x80, "")

    test { cpu =>
      ucast( cpu.lsr(1, 0x80, ByteSize), ByteSize )
    } shouldBe (0x40, "")

    test { cpu =>
      ucast( cpu.lsr(1, 0x81, ByteSize), ByteSize )
    } shouldBe (0x40, "xc")

    test { cpu =>
      ucast( cpu.lsr(2, 0x80, ByteSize), ByteSize )
    } shouldBe (0x20, "")

    test { cpu =>
      ucast( cpu.lsr(2, 0x81, ByteSize), ByteSize )
    } shouldBe (0x20, "")

    test { cpu =>
      ucast( cpu.lsr(3, 0x83, ByteSize), ByteSize )
    } shouldBe (0x10, "")
  }

}