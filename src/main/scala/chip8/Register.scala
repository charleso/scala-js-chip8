package chip8

case class Registers(val registers: List[Register]) {

  type AddressR[T] = Address => T

  def X2: AddressR[Register] = address => X(address)

  def X(implicit address: Address) = registers((address & 0x0F00) >> 8)
  def Y(implicit address: Address) = registers((address & 0x00F0) >> 4)
  def CARRY = registers(0xF)

  def X2_(value: Register): AddressR[Registers] = a => X_(value)(a)

  def X_(value: Register)(implicit address: Address): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value))
  }

  def Y_(value: Register)(implicit address: Address): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value))
  }

  def CARRY_(value: Register): Registers = {
    Registers(registers.updated(0xF, value))
  }
}
