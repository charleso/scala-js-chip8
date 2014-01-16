package chip8

import scala.util.Random
import scala.NotImplementedError

object Opcodes {

  // Technically this is ReaderT[State, Cpu, OpCode], or something like that
  type CpuOp = Opcode => Cpu => Cpu

  def fetch(opcode: Opcode): CpuReader = (opcode & 0xF000 match {
    case 0x0000 => opcode & 0xF match {
      case 0x0000 => clearScreen
      case 0x000E => op00EE
    }
    case 0x1000 => op1NNN
    case 0x2000 => op2NNN
  })(opcode)

//
//  def fetch: CpuState = opcode & 0xF000 match {
//    case 0x0000 => opcode & 0xF match {
//      case 0x0000 => clearScreen
//      case 0x000E => op00EE
//    }
//    case 0x1000 => op1NNN
//    case 0x2000 => op2NNN
//    case 0x3000 => op3XNN
//    case 0x4000 => op4XNN
//    case 0x5000 => op5XY0
//    case 0x6000 => op6XNN
//    case 0x7000 => op7XNN
//    case 0x8000 => opcode & 0xF match {
//      case 0x0 => op8XY0
//      case 0x1 => op8XY1
//      case 0x2 => op8XY2
//      case 0x3 => op8XY3
//      case 0x4 => op8XY4
//      case 0x5 => op8XY5
//      case 0x6 => op8XY6
//      case 0x7 => op8XY7
//      case 0xE => op8XYE
//    }
//    case 0x9000 => op9XY0
//    case 0xA000 => opANNN
//    case 0xB000 => opBNNN
//    case 0xC000 => opCXNN
//    case 0xD000 => opDXYN
//    case 0xE000 => opcode & 0xF match {
//      case 0xE => opEX9E
//      case 0x1 => opEXA1
//    }
//    case 0xF000 => opcode & 0xFF match {
//      case 0x07 => opFX07
//      case 0x0A => opFX0A
//      case 0x15 => opFX15
//      case 0x18 => opFX18
//      case 0x1E => opFX1E
//      case 0x29 => opFX29
//      case 0x33 => opFX33
//      case 0x55 => opFX55
//      case 0x65 => opFX65
//    }
//    case _ => throw new NotImplementedError
//  }



  // Example of raw StateMonad
  /*
  type CpuState = Opcode => State[Cpu, Unit]

  def clearScreen: CpuState = _ => State.unit(())

  def op00EE: CpuState = _ =>
    State.modify(cpu => cpu.copy(pc = cpu.stack.head, stack = cpu.stack.pop))

  */

  val clearScreen: CpuOp = _ => identity

  val op00EE: CpuOp = {
    _ => cpu => cpu.update(pc = cpu.stack.head, stack = cpu.stack.pop)
  }

  val op1NNN: CpuOp = {
    opcode => _.update(pc = opcode & 0x0FFF)
  }

  val op2NNN: CpuOp = {
    opcode => cpu => cpu.update(stack = cpu.stack.push(cpu.pc), pc = opcode & 0x0FFF)
  }

  val op3XNN: CpuOp = {
    opcode => cpu => Cpu.pc(
      // Sucks having to pass in opcode here
      if (cpu.registers.X2(opcode) == (opcode & 0x00FF)) cpu.pc + 2
      else cpu.pc)(cpu)
  }

  /*
  // This _should_ look _something_ like this if we were using monad transformers
  val op3XNN: CpuOp = for {
      cpu <- State.get[Cpu]
      // Lift the reader monad into State, note that we're not passing in the opcode
      x <- cpu.registers.X2.liftU
      _ <- State.set(if (x == (opcode & 0x00FF)) cpu.pc + 2 else cpu.pc)
  } yield ()
   */

  def op4XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc =
      if (cpu.registers.X  != (opcode & 0x00FF)) cpu.pc + 2
      else cpu.pc
  )

  def op5XY0(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc =
      if (cpu.registers.X == cpu.registers.Y) cpu.pc + 2
      else cpu.pc
  )

  def op6XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(opcode & 0x00FF)
  )

  def op7XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X + opcode & 0x00FF)
  )

  def op8XY0(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.Y)
  )

  def op8XY1(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X | cpu.registers.Y)
  )


  def op8XY2(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X & cpu.registers.Y)
  )

  def op8XY3(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X ^ cpu.registers.Y)
  )

  def op8XY4(cpu: Cpu)(implicit opcode: Int) = {
    val result = cpu.registers.X + cpu.registers.Y
    val carryFlag = if (result > 255) 1 else 0
    cpu.copy(
      registers = cpu.registers.X_(register(result & 0xFF))
        .CARRY_(register(carryFlag))
    )
  }

  def op8XY5(cpu: Cpu)(implicit opcode: Int) = {
    val carryFlag = if (cpu.registers.X < cpu.registers.Y) 0 else 1
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.X - cpu.registers.Y)
        .CARRY_(register(carryFlag))
    )
  }

  def op8XY6(cpu: Cpu)(implicit opcode: Int) = {
    val carryFlag = cpu.registers.X & register(0x1)
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.X >> register(1))
        .CARRY_(carryFlag)
    )
  }

  def op8XY7(cpu: Cpu)(implicit opcode: Int) = {
    val carryFlag = if (cpu.registers.Y < cpu.registers.X) 0 else 1
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.Y - cpu.registers.X)
        .CARRY_(register(carryFlag))
    )
  }

  def op8XYE(cpu: Cpu)(implicit opcode: Int) = {
    val carryFlag = cpu.registers.X >> 7
    cpu.copy(
      registers = cpu.registers.X_(register(cpu.registers.X * 2))
        .CARRY_(register(carryFlag))
    )
  }

  def op9XY0(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc = if (cpu.registers.X != cpu.registers.Y) cpu.pc + 2 else cpu.pc
  )

  def opANNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registerI = register(opcode & 0x0FFF)
  )

  def opBNNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc = cpu.registers.registers(0) + opcode & 0x0FFF
  )

  def opCXNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(register(Random.nextInt(255) & (opcode & 0x00FF)))
  )

  def opDXYN(cpu: Cpu)(implicit opcode: Int) = {
    val height = opcode & 0x000F
    val coordx = cpu.registers.X
    val coordy = cpu.registers.Y
    var carryFlag = 0
    var screen = cpu.screen

    for (yline <- 0 until height) {
      val data = cpu.memory.data(cpu.registerI + yline)
      var xpixelinv = 8
      for (xpixel <- 0 until 8) {
        xpixelinv -= 1
        val mask = 1 << xpixelinv
        if ((data & mask) != 0) {
          val x = coordx + xpixel
          val y = coordy + yline
          if ((x < 64) && (y < 32)) {
            if (screen(x)(y) == 1) {
              carryFlag = 1
            }
            screen.update(x, y, screen(x)(y) ^ 1)
          }
        }
      }
    }
    cpu.copy(
      registers = cpu.registers.CARRY_(register(carryFlag)),
      screen = screen
    )
  }

  def opEX9E(cpu: Cpu)(implicit opcode: Int) = cpu

  def opEXA1(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc = cpu.pc + 2
  )

  def opFX07(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(register(cpu.delayTimer))
  )

  def opFX0A(cpu: Cpu)(implicit opcode: Int) = cpu

  def opFX15(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    delayTimer = cpu.registers.X
  )

  def opFX18(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    soundTimer = cpu.registers.X
  )

  def opFX1E(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registerI = cpu.registers.X + cpu.registerI
  )

  def opFX29(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registerI = cpu.registers.X * register(5)
  )

  def opFX33(cpu: Cpu)(implicit opcode: Int) = {
    val updatedMemory = Memory(cpu.memory.data
      .updated(cpu.registerI + 0, cpu.registers.X / 100)
      .updated(cpu.registerI + 1, (cpu.registers.X / 10) % 10)
      .updated(cpu.registerI + 2, cpu.registers.X % 10))
    cpu.copy(memory = updatedMemory)
  }

  def opFX55(cpu: Cpu)(implicit opcode: Int) = {
    val x = (opcode & 0x0F00) >> 8
    val updatedMemory = (0 to x).zipWithIndex.foldLeft(cpu.memory.data){
      case (mem, (value, index)) => mem.updated(cpu.registerI + index, cpu.registers.registers(index))
    }
    cpu.copy(memory = Memory(updatedMemory))
  }

  def opFX65(cpu: Cpu)(implicit opcode: Int) = {
    val x = (opcode & 0x0F00) >> 8
    val updatedRegisters = (0 to x).zipWithIndex.foldLeft(cpu.registers.registers){
      case (regs, (value, index)) => {
        regs.updated(index, register(cpu.memory.data(cpu.registerI + index)))
      }
    }
    cpu.copy(registers = Registers(updatedRegisters))
  }
}