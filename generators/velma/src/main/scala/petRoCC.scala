class petRoCCControlUnit extends Module {
  val io = IO(new Bundle{
    val cmd = Input(new RoCCCommand)
    val do_read = Output(Bool())
    val read_addr = 
  })
    //DECIDE ON OUTPUTS AS WE GO 
  
////////////////////////////////////Super basic IF/////////////////////////////////////////////////////
//enable register updating to get cmd
  val cmd_regEn = io.cmd.valid //also going to want to AND this with inversions of our busy signals for to be safe
  //store cmd 
  val cmd = RegEnable(io.cmd, cmd_regEn)


///////////////////////////////////Super basic ID/////////////////////////////////////////////////
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

// CHECK OUT OBJECT OPCODESET, LINE 400ISH, LAZYROCC.SCALA FOR FREE ENCODINGS 


  })
}




class petRoCCMemRequestMaker extends Module {
  val io = IO(new Bundle {
    //What inputs do we need to assemble the request?
    //    val mreq = new HellaCacheReq <- don't need to input or output a mem request, saving just in case
    val req_sent = Output(Bool())
    val read_addr = Input(UINT(width = coreMaxAddrBits))
    val do_read = Input(Bool())
    val memop_size = Input(log2Ceil(8).U) //2^memop_size tells us the number of BYTES in our operation
  })

  // ASSEMBLE MEMORY REQUEST 
  val mreq = new HellaCacheReq
  io.mreq := mreq
  mreq.bits.addr := rs1d
  mreq.bits.tag :=  3.U(2.W)//unsigned integer with width of 7 and value 3. If signed, would be -1.
  //mreq.bits.idx : //leaving this guy unused for now, the examples also don't use it and i'll just pray :)
  mreq.bits.cmd := M_XRD //apparently how we issue a read; a store is issued as M_XWR
  mreq.bits.size := log2Ceil(8).U //size of data we want--log_2(8)=3
  mreq.bits.data := 0.U //doing a read for now.
  mreq.bits.phys := false.B
  //mreq.bits.dprv := cmd.bits.status.dprv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
  //mreq.bits.dv := cmd.bits.status.dv //THIS LINE NEEDS FIXING DUE TO MODULARIZATION
  mreq.valid := cmd.valid && (opcode == "b0000001".U) //valid command? read instruction? may need funct here.
  mreq.ready := mreq.valid //not doing anything fancy rn, so as soon as our command is valid...


  val req_busy = Bool()
  when (mreq.fire()){ 
    req_busy := true.B} //is the request in flight? we're busy.
  .otherwise{
    req_busy := false.B
    }
}

class petRoCCMemResponseTaker extends Module {
  val io = IO(new Bundle {
    val mresp = new HellaCacheResp
    //outputs: data, has_data, addr, ready/valid of some kind :)
  })



  // HANDLE MEMORY RESPONSE 
  val mresp = new HellaCacheResp 
  mresp := io.mem.resp //connect to io
  val has_data = mresp.bits.has_data
  val read_data = mresp.bits.data

  val data_read = Bool()
  data_read = mresp.valid && mresp.bits.has_data //do we have data?
  val data = Bits(width = coreDataBits)
  data := mresp.bits.data

  val mresp_busy = Bool()
  when mresp.fire(){
    mresp_busy = true.B}
  .otherwise{
    mresp_busy = false.B}

}

class petRoCCCoreResponseMaker extends Module {
  val io = IO(new Bundle {
    val resp = Output(new RoCCResponse)
  })

  
///////////////////////////////// WB, As It Were /////////////////////////////////////
  io.resp.valid := data_read
  io.resp.bits.rd := rd
  io.resp.bits.data := data

  io.interrupt := false.B

////////////////  Business ////////////////
  io.busy := mresp_busy || req_busy 
}

class petRoCCSharpieMouth extends Module {
  val io = IO(new Bundle {
    val data = Input(Bits(width = coreDataBits))
    val doit = Input(Bool())
  })

}


class petRoCC(opcodes: OpcodeSet)(implicit p: parameters) extends LazyRoCC(opcodes = opcodes, nPTWPorts = 1){
  override lazy val module = new petRoCCModule(this)
}



class petRoCCModule(outer: petRoCC) extends LazyRoCCModuleImp(outer) {
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

  //for our declarations of RoCCResponse and similar bundle-extending data, do we need the IO(new RoCCResponse syntax)

/////////////////////////////////////////////////////////////////////
/////////////////// Control Handling//////////////////
////////////////////////////////////////
  val ctrl = Module(new petRoCCControlUnit)
  //inputs
  ctrl.io.cmd := io.cmd
  //outputs
  val do_read := ctrl.io.do_read
  val read_addr := ctrl.io.read_addr
  val memop_size := ctrl.io.memop_size
  

////////////////////////////////////////////////////////////////////
////////////////// Assembling/Dispatching memreq ////////
////////////////////////////////////////////////////

  val mreqer = Module(new petRoCCMemRequestMaker)
  //need to populate inputs and outputs
  


///////////////////////////////////////////////////////////////////
//////////////////// Handling memresp //////////////////
////////////////////////////////////////////////


  val mtaker = Module(new petRoCCMemResponseTaker)
  //need to populate i/o


//////////////////////////////////////////////////////////////////
/////////////////// Responding to CPU /////////////////
///////////////////////////////////////////////

  val coresp = Module(new petRoCCCoreResponseMaker)
  // INPUTS
  // need to populate inputs
  // OUTPUTS
  io.resp := coresp.io.resp

}



//ye olde mixin
class HasPetRoCC extends Config((site,here,up) => {
  case BuildRoCC => List (
    (p: Parameters) => {
      val petRoCCMod = LazyModule(new Velma(OpcodeSet.custom0)(p))
      petRoCCMod
    })
})
