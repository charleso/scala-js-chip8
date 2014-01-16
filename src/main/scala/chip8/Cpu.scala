package chip8
import scala.collection.immutable.Stack

case class Cpu(
  val pc: Int = 0x200,
  val memory: Memory,
  val stack: Stack[Int] = Stack(),
  val delayTimer: Int = 0,
  val soundTimer: Int = 0,
  val screen: Screen = Screen(65, 33),
  val registers: Registers = Registers(List.fill[Register](16)(0)),
  val registerI: Register = 0) {

  def handleOpcode(beforeExecution: Cpu): Cpu = {
    val opcodeFunction = Opcodes.fetch(beforeExecution.nextOpcode)
    opcodeFunction(beforeExecution.copy(beforeExecution.pc + 2))
  }

  def handleTimers: CpuReader = cpu => cpu.copy(
    delayTimer = if (cpu.delayTimer > 0) cpu.delayTimer - 1 else cpu.delayTimer,
    soundTimer = if (cpu.soundTimer > 0) cpu.soundTimer - 1 else cpu.soundTimer
  )

  // TODO
  def handleInput: CpuReader = identity

  // TODO Should this really be val? Seems like something you want to calculate on the fly?
  val nextOpcode = memory.data(pc) << 8 | memory.data(pc + 1)

  def debug(cpu: Cpu) = {
    println("PC before = " + cpu.pc.toHexString +
      " opcode before = " + cpu.nextOpcode.toHexString +
      " instruction before = " + (cpu.nextOpcode & 0xF000).toHexString)
  }

  def emulate(drawScreen: CpuReader) = {
    handleOpcode _ andThen handleTimers andThen handleInput andThen drawScreen
  }

  def update(pc: Int = pc, stack: Stack[Int] = stack) = {
    // Fast version, one version
    // Could also return an ADT that is then interpreted
//    this.pc = pc
//    this.stack = stack
//    this
    copy(pc = pc, stack = stack)
  }
}

object Cpu {

  def flatMap(f: Cpu => CpuReader): CpuReader = cpu => f(cpu)(cpu)

  def pc(value: Int): CpuReader = cpu => {
    cpu.copy(pc = value)
  }
}

case class Screen(x: Int, y: Int) {
  val s = Array.ofDim[Int](x, y)

  def apply(x: Int) = s(x)

  def update(x: Int, y: Int, value: Int) = s(x)(y) = value
}

