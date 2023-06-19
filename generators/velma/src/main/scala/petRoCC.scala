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
//import chisel3.experimental.ChiselEnum
import freechips.rocketchip.config.Parameters
//import freechips.rocketchip.tile.{XLen, CoreModule, RoCCCommand}
//import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.util.EnhancedChisel3Assign



class petRoCCControlUnit(implicit val p: Parameters) extends Module
  with HasCoreParameters
  with HasNonDiplomaticTileParameters
  {
  val io = IO(new Bundle{
    val cmd = Input(new RoCCCommand)
    val do_read = Output(Bool())
    val read_addr = Output(UInt(width = coreMaxAddrBits.W)) 
    val rd = Output(UInt(5.W))
  })


/////////////////////////////////// Extract Control Signals from cmd //////////////////////////////////////////////
  val cmd = io.cmd
  //instruction signals
  val inst = cmd.inst
  val opcode = inst.opcode 
  val rd = inst.rd //destination register number
  val rs1 = inst.rs1 //source register 1 number
  val rs2 = inst.rs2 //source register 2 number 
  val funct = inst.funct //function code
  val xd = inst.xd //is the destination register being used?
  val xs1 = inst.xs1 // is the first source register being used?
  val xs2 = inst.xs2 // is the second source register being used?

  //rest of cmd
  val rs1d = cmd.rs1 //the stored value in rs1
  val rs2d = cmd.rs2 //the stored value in rs2

  //feelin real dumb here: if we get a valid RoCCCommand, just straight up do the read.
//  when (cmd.valid){
//    io.do_read := true.B}
//  .otherwise{
//    io.do_read := false.B}
  io.do_read := true.B 

  io.read_addr := rs1d
  io.rd := rd

  }





class petRoCCMemRequestMaker(implicit val p: Parameters) extends Module 
  with HasCoreParameters
  with HasNonDiplomaticTileParameters
  {
    val io = IO(new Bundle {
    //inputs
    val read_addr = Input(UInt(width = coreMaxAddrBits.W))
    val do_read = Input(Bool())
    val cmd_fire_in = Input(Bool())
 //   val memop_size = Input(UInt()) //2^memop_size tells us the number of BYTES in our operation
    //outputs
    val mreq = Output(new HellaCacheReq)
    val mreq_valid = Output(Bool())
 //   val req_sent = Output(Bool())
 //   val req_busy = Output(Bool())
  })

  // ASSEMBLE MEMORY REQUEST 
  io.mreq.addr := io.read_addr
  io.mreq.tag :=  RegInit(3.U(2.W))//unsigned integer with width of 7 and value 3. If signed, would be -1.
  //mreq.bits.idx : //leaving this guy unused for now, the examples also don't use it and i'll just pray :)
  io.mreq.cmd := RegInit(M_XRD) //apparently how we issue a read; a store is issued as M_XWR
  io.mreq.size := RegInit(log2Ceil(8).U(3.W)) //size of data we want--log_2(8)=3
  io.mreq.data := 0.U(coreMaxAddrBits.W) //doing a read for now.
  io.mreq.phys := false.B
  //mreq.bits.dprv := cmd.bits.status.dprv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
  //mreq.bits.dv := cmd.bits.status.dv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
 // mreq.valid := cmd.valid// LATTER PORTIONS COMMENTED FOR DUMB VERSON && (opcode == "b0000001".U) 
  
  //mreq.ready := mreq.valid //not doing anything fancy rn, so as soon as our command is valid...
        // -> so there apparently isn't an mreq.ready -- the readys come from the Decoupled subclasses

}

class petRoCCMemResponseTaker(implicit val p: Parameters) extends Module 
  with HasCoreParameters
  //with HasNonDiplomaticTileParameters
  {
  val io = IO(new Bundle {
    //inputs
    val mresp = Input(new HellaCacheResp)
    val mresp_valid = Input(Bool())
    //outputs: data, has_data, addr, ready/valid of some kind :)
    val valid_read = Output(Bool())
    val read_data = Output(UInt(xLen.W))
  //  val mresp_busy = Output(Bool())
  })



  // HANDLE MEMORY RESPONSE 
  val mresp = io.mresp
  val valid_read = io.mresp_valid && mresp.has_data //do we have data?
  val read_data = RegEnable(mresp.data, valid_read)//buffer the data when it comes in!
  io.valid_read := valid_read
  io.read_data := read_data
  //io.mresp_busy := mresp_busy
  

}

class petRoCCCoreResponseMaker(implicit val p: Parameters) extends Module
  with HasCoreParameters
//  with HasNonDiplomaticTileParameters
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
  resp.rd := io.rd
  resp.data := io.data



////////////////  Business ////////////////
  io.busy := io.mresp_busy || io.req_busy 
}

class petRoCCSharpieMouth(implicit val p: Parameters) extends Module 
  with HasCoreParameters
//  with HasNonDiplomaticTileParameters
  {
  val io = IO(new Bundle {
    val data = Input(Bits(xLen.W)) //was width = coreDataBits
  //  val doit = Input(Bool())
  })
  val data = io.data
  //TODO: FIX THE PRINT STATEMENT 
 // printf(cf"Hi! The piece of data you had me read was: $data")
  
}


class petRoCC(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes, nPTWPorts = 1){
  override lazy val module = new petRoCCModule(this)
}



class petRoCCModule(outer: petRoCC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters  
{
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
  val cmd_regEn = io.cmd.fire() 
  val cmd = RegEnable(io.cmd, cmd_regEn)


/////////////////////////////////////////////////////////////////////
/////////////////// Control Handling//////////////////
////////////////////////////////////////
  val ctrl = Module(new petRoCCControlUnit)
  //inputs
 // ctrl.io.cmd.status  io.cmd.bits.status
 ctrl.io.cmd := io.cmd.bits //this is one of the problem lines--cmd apparently missing status? 
  //outputs
  val do_read = ctrl.io.do_read
  val read_addr = ctrl.io.read_addr
 // val memop_size = ctrl.io.memop_size
  

////////////////////////////////////////////////////////////////////
////////////////// Assembling/Dispatching memreq ////////
////////////////////////////////////////////////////


  val mreqer = Module(new petRoCCMemRequestMaker)
  //need to populate inputs and outputs
  //inputs
  val cmd_fire_in = io.cmd.fire()
  mreqer.io.read_addr := read_addr
  mreqer.io.do_read := do_read
  mreqer.io.cmd_fire_in := cmd_fire_in
  
//  mreqer.io.memop_size := memop_size
  //outputs
  val mreq = mreqer.io.mreq
  //val req_sent = mreqer.io.req_sent // Bool()
  //val req_busy = mreqer.io.req_busy//Bool()
  val mreq_valid = mreqer.io.mreq_valid //this signal may well genuinely exist. however, 
  //it is NOT a signal in HellaCacheReqs as far as I can tell. IF it exists, it comes from
  //the RoCC's decoupledIO (RoCCIO extends Decoupled)
  io.mem.req.bits := mreq
  io.mem.req.valid := mreq_valid 
  //req_sent := mreqer.io.req_sent
  //req_busy := mreqer.io.req_busy

  val req_busy = RegInit(false.B)
  when (io.mem.req.fire()){ 
    req_busy := true.B} //is the request in flight? we're busy.
  .otherwise{
    req_busy := false.B
    }
///////////////////////////////////////////////////////////////////
//////////////////// Handling memresp //////////////////
////////////////////////////////////////////////


  val mtaker = Module(new petRoCCMemResponseTaker)
  //inputs
  val mresp = io.mem.resp.bits
  val mresp_valid = io.mem.resp.valid
  mtaker.io.mresp := mresp
  //val  mtaker.io.mresp
  //outputs
  val valid_read = mtaker.io.valid_read
  val read_data = mtaker.io.read_data
 // val mresp_busy = mtaker.io.busy


  val mresp_busy = RegInit(false.B)

  when (io.mem.resp.fire()){
    mresp_busy := true.B}
  .otherwise{
    mresp_busy := false.B}

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
  io.resp.bits := coresp.io.resp
  io.busy := coresp.io.busy



}



//ye olde mixin
class HasPetRoCC extends Config((site,here,up) => {
  case BuildRoCC => List (
    (p: Parameters) => {
      val petRoCCMod = LazyModule(new petRoCC(OpcodeSet.custom0)(p))
      petRoCCMod
    })
})
