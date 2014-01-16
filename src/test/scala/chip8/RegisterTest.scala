package chip8

import org.scalatest.FlatSpec

class RegisterTest extends FlatSpec {

   "Adding two Registers" should "return a new Register with both added" in {
     val reg1 = register(1)
     val reg2 = register(2)
     assert(reg1 + reg2 === register(3))
   }

  "Subtracting two Registers" should "return a new Register with both subtracted" in {
    val reg1 = register(2)
    val reg2 = register(1)
    assert(reg1 - reg2 === register(1))
  }

  "Shifting left two Registers" should "return a new Register with both shifted left" in {
    val reg1 = register(2)
    val reg2 = register(1)
    assert((reg1 << reg2) === register(4))
  }

  "Shifting right two Registers" should "return a new Register with both shifted right" in {
    val reg1 = register(2)
    val reg2 = register(1)
    assert((reg1 >> reg2) === register(1))
  }

  "And'ing two Registers" should "return a new Register with both and'ed" in {
    val reg1 = register(5)
    val reg2 = register(6)
    assert((reg1 & reg2) === register(4))
  }

  "Or'ing two Registers" should "return a new Register with both or'ed" in {
    val reg1 = register(2)
    val reg2 = register(1)
    assert((reg1 | reg2) === register(3))
  }

  "Inverting'ing two Registers" should "return a new Register with both inverted'ed" in {
    val reg1 = register(5)
    val reg2 = register(1)
    assert((reg1 ^ reg2) === register(4))
  }
 }
