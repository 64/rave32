package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object InstFormat extends ChiselEnum {
  val R = Value
  val I = Value
  val S = Value
  val B = Value
  val U = Value
  val J = Value
}

object Inst {
  def ADD = BitPat("b0000000??????????000?????0110011")
  def ADDI = BitPat("b?????????????????000?????0010011")
  def AND = BitPat("b0000000??????????111?????0110011")
  def ANDI = BitPat("b?????????????????111?????0010011")
  def AUIPC = BitPat("b?????????????????????????0010111")
  def BEQ = BitPat("b?????????????????000?????1100011")
  def BGE = BitPat("b?????????????????101?????1100011")
  def BGEU = BitPat("b?????????????????111?????1100011")
  def BLT = BitPat("b?????????????????100?????1100011")
  def BLTU = BitPat("b?????????????????110?????1100011")
  def BNE = BitPat("b?????????????????001?????1100011")
  def JAL = BitPat("b?????????????????????????1101111")
  def JALR = BitPat("b?????????????????000?????1100111")
  def LB = BitPat("b?????????????????000?????0000011")
  def LBU = BitPat("b?????????????????100?????0000011")
  def LD = BitPat("b?????????????????011?????0000011")
  def LH = BitPat("b?????????????????001?????0000011")
  def LHU = BitPat("b?????????????????101?????0000011")
  def LUI = BitPat("b?????????????????????????0110111")
  def LW = BitPat("b?????????????????010?????0000011")
  def OR = BitPat("b0000000??????????110?????0110011")
  def ORI = BitPat("b?????????????????110?????0010011")
  def SB = BitPat("b?????????????????000?????0100011")
  def SD = BitPat("b?????????????????011?????0100011")
  def SH = BitPat("b?????????????????001?????0100011")
  def SLL = BitPat("b0000000??????????001?????0110011")
  def SLLI = BitPat("b000000???????????001?????0010011")
  def SLT = BitPat("b0000000??????????010?????0110011")
  def SLTI = BitPat("b?????????????????010?????0010011")
  def SLTIU = BitPat("b?????????????????011?????0010011")
  def SLTU = BitPat("b0000000??????????011?????0110011")
  def SRA = BitPat("b0100000??????????101?????0110011")
  def SRAI = BitPat("b010000???????????101?????0010011")
  def SRL = BitPat("b0000000??????????101?????0110011")
  def SRLI = BitPat("b000000???????????101?????0010011")
  def SUB = BitPat("b0100000??????????000?????0110011")
  def SW = BitPat("b?????????????????010?????0100011")
  def XOR = BitPat("b0000000??????????100?????0110011")
  def XORI = BitPat("b?????????????????100?????0010011")
}
