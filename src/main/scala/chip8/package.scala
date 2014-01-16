package object chip8 {

  type Opcode = Int
  type Register = Int
  type Address = Int

  // To be implemented with real Reader... maybe
  type CpuReader = Cpu => Cpu

  // TODO Technically this isn't used any more - but you might want to pimp Int
  implicit class RegisterRich(val value: Register) extends AnyVal {
    def ==(i: Register) = value == i
    def +(i: Register) = value + i
    def *(i: Register) = value * i
    def -(i: Register) = value - i
    def <<(i: Register) = value << i
    def >>(i: Register) = value >> i
    def &(i: Register) = value & i
    def |(i: Register) = value | i
    def ^(i: Register) = value ^ i
  }

  // Just in case you want to change the implementation
  // Ideally you could use Tagged Types, but these aren't unboxed for primitive types :(
  // (May still be worth it in a larger system though)
  @inline
  def register(i: Register): Register = i
}
