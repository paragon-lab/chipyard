/* petRoCC -- Author: Connor Selna
 * Acknowledgements: Karl Hallsby is cool, Berkeley Architecture Research is also cool
 */
package velma

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import chisel3.experimental.ChiselEnum
import freechips.rocketchip.config.Parameters
//import freechips.rocketchip.tile.{XLen, CoreModule, RoCCCommand}
//import freechips.rocketchip.rocket.constants.MemoryOpConstants
//import freechips.rocketchip.rocket.MStatus
//import freechips.rocketchip.rocket.{HellaCacheReq, HellaCacheResp}
import freechips.rocketchip.util.EnhancedChisel3Assign



class petRoCCControlUnit extends Module
  with HasCoreParameters
  {
  val io = IO(new Bundle{
    val cmd = Input(new RoCCCommand)
    val do_read = Output(Bool())
    val read_addr = Output(UInt(width = coreMaxAddrBits.W)) 
    val rd = Output(UInt(5.W))
  })


////////////////////////////////// Receive and Buffer Command //////////////////////////////////////////////////////
//enable register updating to get cmd
  val cmd_regEn = io.cmd.fire() //also going to want to AND this with inversions of our busy signals for to be safe
  /*store cmd, this basically works as our storage for all the control signals, since the control is determined
    mostly combinationally--will still need to store our read data, though.*/
  val cmd = RegEnable(io.cmd, cmd_regEn)


/////////////////////////////////// Extract Control Signals from cmd //////////////////////////////////////////////
/* inst signals, we likely want all of this stored in a register when cmd.fire goes high.
 * at the moment, the way we handle cmd in IF takes care of this functionality, probably. */

  //instruction signals
  val inst = cmd.bits.inst
  val opcode = inst.opcode 
  val rd = inst.rd //destination register number
  val rs1 = inst.rs1 //source register 1 number
  val rs2 = inst.rs2 //source register 2 number 
  val funct = inst.funct //function code
  val xd = inst.xd //is the destination register being used?
  val xs1 = inst.xs1 // is the first source register being used?
  val xs2 = inst.xs2 // is the second source register being used?

  //rest of cmd
  val rs1d = cmd.bits.rs1 //the stored value in rs1
  val rs2d = cmd.bits.rs2 //the stored value in rs2

  //feelin real dumb here: if we get a valid RoCCCommand, just straight up do the read.
//  when (cmd.valid){
//    io.do_read := true.B}
//  .otherwise{
//    io.do_read := false.B}
  io.do_read := true.B 

  io.read_addr := rs1d
  io.rd := rd

  }





class petRoCCMemRequestMaker extends Module 
      with HasCoreParameters{
  val io = IO(new Bundle {
    //inputs
    val read_addr = Input(UInt(width = coreMaxAddrBits.W))
    val do_read = Input(Bool())
    val memop_size = Input(UInt()) //2^memop_size tells us the number of BYTES in our operation
    //outputs
    val mreq = Output(new HellaCacheReq)
    val req_sent = Output(Bool())
    val req_busy = Output(Bool())
  })

  // ASSEMBLE MEMORY REQUEST 
  val mreq = new HellaCacheReq
  io.mreq := mreq
  mreq.addr := rs1d
  mreq.tag :=  3.U(2.W)//unsigned integer with width of 7 and value 3. If signed, would be -1.
  //mreq.bits.idx : //leaving this guy unused for now, the examples also don't use it and i'll just pray :)
  mreq.cmd := M_XRD //apparently how we issue a read; a store is issued as M_XWR
  mreq.size := log2Ceil(8).U //size of data we want--log_2(8)=3
  mreq.data := 0.U //doing a read for now.
  mreq.phys := false.B
  //mreq.bits.dprv := cmd.bits.status.dprv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
  //mreq.bits.dv := cmd.bits.status.dv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
  mreq.valid := cmd.valid// LATTER PORTIONS COMMENTED FOR DUMB VERSON && (opcode == "b0000001".U) 
  
  //mreq.ready := mreq.valid //not doing anything fancy rn, so as soon as our command is valid...
        // -> so there apparently isn't an mreq.ready -- the readys come from the Decoupled subclasses

  val req_busy = Bool()
  when (mreq.fire){ 
    req_busy := true.B} //is the request in flight? we're busy.
  .otherwise{
    req_busy := false.B
    }
}

class petRoCCMemResponseTaker extends Module 
  with HasCoreParameters
  {
  val io = IO(new Bundle {
    //inputs
    val mresp = Input(new HellaCacheResp)
    //outputs: data, has_data, addr, ready/valid of some kind :)
    val valid_read = Output(Bool())
    val read_data = Output(UInt(xLen.W))
    val mresp_busy = Output(Bool())
  })



  // HANDLE MEMORY RESPONSE 
  val mresp = io.mem.resp
  val valid_read = mresp.valid && mresp.bits.has_data //do we have data?
  val read_data = RegEnable(mresp.bits.data, valid_read)//buffer the data when it comes in!
  val mresp_busy = Bool()

  when (mresp.fire()){
    mresp_busy := true.B}
  .otherwise{
    mresp_busy := false.B}

  io.valid_read := valid_read
  io.read_data := read_data
  io.mresp_busy := mresp_busy
  

}

class petRoCCCoreResponseMaker extends Module
  with HasCoreParameters
  {
  val io = IO(new Bundle {
    //inputs
    val rd = Input(Bits(5.W))
    val data = Input(Bits(xLen.W))
    val valid_read = Input(Bool())
    val mresp_busy = Input(Bool())
    val req_busy = Input(Bool())
    //outputs
    val resp = Output(new RoCCResponse)
    val busy = Output(Bool())

  })

  
///////////////////////////////// WB, As It Were /////////////////////////////////////
  val resp = io.resp 
  resp.valid := io.valid_read
  resp.bits.rd := io.rd
  resp.bits.data := io.data



////////////////  Business ////////////////
  io.busy := io.mresp_busy || io.req_busy 
}

class petRoCCSharpieMouth extends Module 
  with HasCoreParameters
  {
  val io = IO(new Bundle {
    val data = Input(Bits(xLen.W)) //was width = coreDataBits
  //  val doit = Input(Bool())
  })

  printf(cf"Hi! The piece of data you had me read was: $data")
  
}


class petRoCC(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes, nPTWPorts = 1){
  override lazy val module = new petRoCCModule(this)
}



class petRoCCModule(outer: petRoCC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
/*io here is implied to be a RoCCIO (extends RoCCCoreIO) type, maintain for now.
RoCCIO signals: 
val ptw = Vec(nPTWPorts, new TLBPTWIO)
val pfu_req = Decoupled(new FPInput) //FPInput is output from RoCC
val fpu_resp = Flipped(Decoupled(new FPResult)) //FPResult is input to RoCC
  
RoCCCoreIO signals:
val cmd = Flipped(Decoupled(new RoCCCommand)) //input. our incoming command.
val resp = Decoupled(new RoCCResponse) // response. our response to CPU
val mem = new HellaCacheIO //how we talk straight to cache
val busy = Output(Bool())  
val interrupt = Output(Bool())
val exception = Input(Bool()) //input to handle exceptions
*/


////////////////////////////////// Receive and Buffer Command //////////////////////////////////////////////////////
//enable register updating to get cmd
  val cmd_regEn = io.cmd.fire() //also going to want to AND this with inversions of our busy signals for to be safe
  /*store cmd, this basically works as our storage for all the control signals, since the control is determined
    mostly combinationally--will still need to store our read data, though.*/
  val cmd = RegEnable(io.cmd, cmd_regEn)

/////////////////////////////////////////////////////////////////////
/////////////////// Control Handling//////////////////
////////////////////////////////////////
  val ctrl = Module(new petRoCCControlUnit)
  //inputs
  ctrl.io.cmd := cmd
  //outputs
  val do_read = ctrl.io.do_read
  val read_addr = ctrl.io.read_addr
  val memop_size = ctrl.io.memop_size
  

////////////////////////////////////////////////////////////////////
////////////////// Assembling/Dispatching memreq ////////
////////////////////////////////////////////////////

  val mreqer = Module(new petRoCCMemRequestMaker)
  //need to populate inputs and outputs
  //inputs
  mreqer.io.read_addr := read_addr
  mreqer.io.do_read := do_read
  mreqer.io.memop_size := memop_size
  //outputs
  val mreq = new HellaCacheReq
  val req_sent = mreqer.io.req_sent // Bool()
  val req_busy = mreqer.io.req_busy//Bool()
  mreq := mreqer.io.mreq
  //req_sent := mreqer.io.req_sent
  //req_busy := mreqer.io.req_busy

///////////////////////////////////////////////////////////////////
//////////////////// Handling memresp //////////////////
////////////////////////////////////////////////


  val mtaker = Module(new petRoCCMemResponseTaker)
  //inputs
  val mresp = io.mem.resp
  mtaker.io.mresp := mresp
  //outputs
  val valid_read = mtaker.io.valid_read
  val read_data = mtaker.io.read_data
  val mresp_busy = mtaker.io.busy


//////////////////////////////////////////////////////////////////
/////////////////// Responding to CPU /////////////////
///////////////////////////////////////////////

  val coresp = Module(new petRoCCCoreResponseMaker)
  //inputs
  coresp.io.rd := ctrl.io.rd
  coresp.io.data := read_data
  coresp.io.valid_read := valid_read
  coresp.io.mresp_busy := mresp_busy
  coresp.io.req_busy := req_busy
  //outputs  
  io.resp := coresp.io.resp
  io.busy := coresp.io.busy



}



//ye olde mixin
class HasPetRoCC extends Config((site,here,up) => {
  case BuildRoCC => List (
    (p: Parameters) => {
      val petRoCCMod = LazyModule(new Velma(OpcodeSet.custom0)(p))
      petRoCCMod
    })
})
