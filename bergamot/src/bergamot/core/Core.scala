package bergamot.core

import chisel3._
import chisel3.util._

import bergamot.core.fetch.Fetch
import bergamot.core.fetch.BranchPredictorUpdateIO
import bergamot.core.decode.Decode
import bergamot.core.record.TLB
import bergamot.core.record.RegisterMappingTable
import bergamot.core.record.ROBTableWriteIO
import bergamot.core.record.PrivilegeType
import bergamot.core.record.RegisterUpdateIO
import bergamot.core.record.CSRsReadIO
import bergamot.core.record.StoreQueue
import bergamot.core.record.StoreQueueMemoryWriter
import bergamot.core.record.StoreQueueRetireIO
import bergamot.core.record.CSRs
import bergamot.core.record.ROB
import bergamot.core.broadcast.RoundRobinBroadcaster
import bergamot.core.broadcast.DataBroadcastIO
import bergamot.core.execute.ExecuteQueueEnqueueIO
import bergamot.core.execute.InOrderedExecuteQueue
import bergamot.core.execute.ExecuteQueueType
import bergamot.core.execute.Branch
import bergamot.core.execute.Memory
import bergamot.core.execute.ALU
import bergamot.core.execute.FPU
import bergamot.core.execute.ExecuteResultEntry
import bergamot.core.execute.OutOfOrderedExecuteQueue
import bergamot.core.execute.LoadReservationUpdateIO
import bergamot.core.retire.InstructionRetire
import bergamot.core.debug.DebugIO

import bergamot.bus.SMAReaderIO
import bergamot.bus.SMAWriterIO
import bergamot.bus.AXIMaster
import bergamot.bus.AXIMasterIO

import bergamot.interconnect.SMA2ReaderInterconnect
import bergamot.interconnect.SMAWithStoreQueueInterconnect
import bergamot.interconnect.CacheLineRequest2Interconnect
import bergamot.interconnect.SkipCacheSMAReaderInterconnect

import bergamot.cache.FlushCacheIO
import bergamot.cache.Serial2Flusher
import bergamot.cache.Parallel2Flusher
import bergamot.cache.SetCache
import bergamot.cache.SMA2CacheLineRequest
import bergamot.cache.CacheLineRequest2SMA
import bergamot.cache.CacheLineRequestIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * Bergamot core integration
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Core config class
  *
  * @param iTLBDepth
  * @param cacheCellDepth
  * @param fetchQueueDepth
  * @param executeQueueWidth
  * @param executeQueueDepth
  * @param dTLBDepth
  * @param storeQueueDepth
  * @param robDepth
  * @param predictorDepth
  * @param pcInit
  */
case class CoreConfig(iTLBDepth: Int, cacheCellDepth: Int, fetchQueueDepth: Int, executeQueueWidth: Int, executeQueueDepth: Int, dTLBDepth: Int, storeQueueDepth: Int, robDepth: Int, predictorDepth: Int, pcInit: String, l1CacheDepth: Int, l1CacheWay: Int, l2CacheDepth: Int, l2CacheWay: Int, memoryAddress: String)

object CoreConfig {

  /** Default config
    */
  val default = CoreConfig(
    iTLBDepth = 8,
    cacheCellDepth = 8,
    fetchQueueDepth = 8,
    executeQueueWidth = 4,
    executeQueueDepth = 8,
    dTLBDepth = 8,
    storeQueueDepth = 8,
    robDepth = 8,
    predictorDepth = 8,
    pcInit = "h00000000",
    l1CacheDepth = 1024,
    l1CacheWay = 2,
    l2CacheDepth = 4096,
    l2CacheWay = 4,
    memoryAddress = "h80000000"
  )
}

/** Bergamot core
  *
  * @param config
  *   Core config
  */
class BergamotCore(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    // AXI
    val axi = new AXIMasterIO()

    // Timer
    val mtime = Input(UInt(64.W))
    val mtimeIRQ = Input(Bool())

    // Debug
    val debug = new DebugIO()
  })

  private val coreFrontend = Module(new CoreFrontend(config))
  private val coreExecute = Module(new CoreExecute(config))
  private val coreBackend = Module(new CoreBackend(config))

  private val tlbFlusher = Module(new Parallel2Flusher())

  private val cacheLineRequest2Interconnect = Module(new CacheLineRequest2Interconnect(config.cacheCellDepth))

  private val l2Cache = Module(new SetCache(config.l2CacheWay, config.l2CacheDepth, config.cacheCellDepth))

  private val axiMaster = Module(new AXIMaster())
  private val cacheLineRequest2SMA = Module(new CacheLineRequest2SMA(config.l2CacheDepth, config.cacheCellDepth))
  private val sma2ReaderInterconnect = Module(new SMA2ReaderInterconnect())

  // CoreFrontend
  coreFrontend.io.cacheLineRequest <> cacheLineRequest2Interconnect.io.in1
  coreFrontend.io.broadcast <> coreBackend.io.broadcast
  coreFrontend.io.alloc <> coreBackend.io.alloc
  coreFrontend.io.tableWrite <> coreBackend.io.tableWrite
  coreFrontend.io.correctPC := coreBackend.io.correctPC
  coreFrontend.io.recover := coreBackend.io.recover
  coreFrontend.io.privilege := coreBackend.io.privilege
  coreFrontend.io.mstatus := coreBackend.io.mstatus
  coreFrontend.io.satp := coreBackend.io.satp

  // CoreExecute
  coreExecute.io.enqs <> coreFrontend.io.enqs
  coreExecute.io.csr <> coreBackend.io.read
  coreExecute.io.broadcast <> coreBackend.io.broadcast
  coreExecute.io.cacheLineRequest <> cacheLineRequest2Interconnect.io.in2
  coreExecute.io.smaWriter <> l2Cache.io.inWriter
  coreExecute.io.ioSMAReader <> sma2ReaderInterconnect.io.in1 // IO read priority
  coreExecute.io.privilege := coreBackend.io.privilege
  coreExecute.io.mstatus := coreBackend.io.mstatus
  coreExecute.io.satp := coreBackend.io.satp
  coreExecute.io.fcsr := coreBackend.io.fcsr
  coreExecute.io.recover := coreBackend.io.recover

  // CoreBackend
  coreBackend.io.deqs <> coreExecute.io.deqs
  coreBackend.io.dCacheFlush <> coreExecute.io.dCacheFlush
  coreBackend.io.iCacheFlush <> coreFrontend.io.iCacheFlush
  coreBackend.io.tlbFlush <> tlbFlusher.io.in
  coreBackend.io.update <> coreFrontend.io.update
  coreBackend.io.updateLoadReservation <> coreExecute.io.updateLoadReservation
  coreBackend.io.predictorUpdate <> coreFrontend.io.predictorUpdate
  coreBackend.io.store <> coreExecute.io.retire
  coreBackend.io.mtime := io.mtime
  coreBackend.io.mtimeIRQ := io.mtimeIRQ
  coreBackend.io.debug <> io.debug

  // TLBFlusher
  tlbFlusher.io.out1 <> coreFrontend.io.iTLBFlush
  tlbFlusher.io.out2 <> coreExecute.io.dTLBFlush

  // L2 Cache
  l2Cache.io.inReader <> cacheLineRequest2Interconnect.io.out
  l2Cache.io.outReader <> cacheLineRequest2SMA.io.request
  cacheLineRequest2SMA.io.smaReader <> sma2ReaderInterconnect.io.in2
  l2Cache.io.outWriter <> axiMaster.io.smaWriter
  l2Cache.io.flush.req := false.B

  // AXIMaster
  sma2ReaderInterconnect.io.out <> axiMaster.io.smaReader
  axiMaster.io.axi <> io.axi
}

/** Core frontend
  *
  * From instruction fetch to dispatching
  *
  * @param config
  *   Core config
  */
class CoreFrontend(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val cacheLineRequest = new CacheLineRequestIO(config.cacheCellDepth)
    val broadcast = Flipped(new DataBroadcastIO())
    val enqs = Vec(config.executeQueueWidth, new ExecuteQueueEnqueueIO())

    val alloc = Flipped(DecoupledIO(DataType.receipt))
    val tableWrite = new ROBTableWriteIO()
    val update = Flipped(new RegisterUpdateIO())
    val predictorUpdate = Flipped(new BranchPredictorUpdateIO())

    val correctPC = Input(DataType.address)
    val recover = Input(Bool())

    val iTLBFlush = Flipped(new FlushCacheIO())
    val iCacheFlush = Flipped(new FlushCacheIO())

    val privilege = Input(PrivilegeType())
    val satp = Input(DataType.operation)
    val mstatus = Input(DataType.operation)
  })

  private val itlb = Module(new TLB(config.iTLBDepth, false))
  private val iCache = Module(new SetCache(config.l1CacheWay, config.l1CacheDepth, config.cacheCellDepth))
  private val fetch = Module(new Fetch(config.l1CacheDepth, config.cacheCellDepth, config.fetchQueueDepth, config.predictorDepth, config.pcInit))
  private val decode = Module(new Decode(config.executeQueueWidth))
  private val registerMappingTable = Module(new RegisterMappingTable())

  private val sma2CacheLineRequest = Module(new SMA2CacheLineRequest(config.l1CacheDepth, config.cacheCellDepth))
  private val cacheLineRequest2Interconnect = Module(new CacheLineRequest2Interconnect(config.cacheCellDepth))

  // ITLB
  itlb.io.sma <> sma2CacheLineRequest.io.smaReader
  sma2CacheLineRequest.io.request <> cacheLineRequest2Interconnect.io.in1
  itlb.io.privilege := io.privilege
  itlb.io.satp := io.satp
  itlb.io.mstatus := io.mstatus
  itlb.io.flush <> io.iTLBFlush

  // ICache
  // Disable ICache write interface
  iCache.io.inWriter <> new SMAWriterIO().zero
  iCache.io.outWriter <> new SMAWriterIO().zero
  iCache.io.outReader <> cacheLineRequest2Interconnect.io.in2
  iCache.io.flush <> io.iCacheFlush

  // Fetch
  fetch.io.asid := io.satp(30, 22)
  fetch.io.update <> io.predictorUpdate
  fetch.io.itlb <> itlb.io.request
  fetch.io.icache <> iCache.io.inReader
  fetch.io.recover := io.recover
  fetch.io.correctPC := io.correctPC
  fetch.io.out <> decode.io.in

  // Decode
  decode.io.mapping <> registerMappingTable.io.mapping
  decode.io.broadcast <> io.broadcast
  decode.io.robTableWrite <> io.tableWrite
  decode.io.recover := io.recover
  decode.io.enqs <> io.enqs

  // RegisterMappingTable
  registerMappingTable.io.alloc <> io.alloc
  registerMappingTable.io.broadcast <> io.broadcast
  registerMappingTable.io.update <> io.update
  registerMappingTable.io.recover <> io.recover

  // CacheLineRequest2Interconnect
  cacheLineRequest2Interconnect.io.out <> io.cacheLineRequest
}

/** Core execute
  *
  * Including three instruction cores: ALU, Memory, Branch
  *
  * @param config
  *   Core config
  */
class CoreExecute(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val enqs = Flipped(Vec(config.executeQueueWidth, new ExecuteQueueEnqueueIO()))
    val deqs = Vec(config.executeQueueWidth, DecoupledIO(new ExecuteResultEntry()))

    val csr = Flipped(new CSRsReadIO())
    val broadcast = Flipped(new DataBroadcastIO())

    val cacheLineRequest = new CacheLineRequestIO(config.cacheCellDepth)
    val smaWriter = new SMAWriterIO()
    val ioSMAReader = new SMAReaderIO()

    val privilege = Input(PrivilegeType())
    val satp = Input(DataType.operation)
    val mstatus = Input(DataType.operation)
    val fcsr = Input(DataType.operation)

    val dTLBFlush = Flipped(new FlushCacheIO())
    val dCacheFlush = Flipped(new FlushCacheIO())
    val updateLoadReservation = Flipped(new LoadReservationUpdateIO())

    val retire = Flipped(new StoreQueueRetireIO())

    val recover = Input(Bool())
  })

  private val aluExecuteQueue =
    Module(new OutOfOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.alu, false))
  private val alu = Module(new ALU())

  private val branchExecuteQueue =
    Module(new OutOfOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.branch, false))
  private val branch = Module(new Branch())

  private val memoryExecuteQueue =
    Module(new InOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.memory, false))
  private val memory = Module(new Memory())

  private val fpuExecuteQueue =
    Module(new OutOfOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.float, true)) // Enable rs3
  private val fpu = Module(new FPU())

  private val storeQueue = Module(new StoreQueue(config.storeQueueDepth))
  private val storeQueueMemoryWriter = Module(new StoreQueueMemoryWriter())
  private val smaWithStoreQueueInterconnect = Module(new SMAWithStoreQueueInterconnect())

  private val dtlb = Module(new TLB(config.dTLBDepth, true))
  private val dCache = Module(new SetCache(config.l1CacheWay, config.l1CacheDepth, config.cacheCellDepth))

  private val sma2CacheLineRequestTLB = Module(new SMA2CacheLineRequest(config.l1CacheDepth, config.cacheCellDepth))
  private val sma2CacheLineRequestStore = Module(new SMA2CacheLineRequest(config.l1CacheDepth, config.cacheCellDepth))
  private val skipCacheSMAReaderInterconnect = Module(new SkipCacheSMAReaderInterconnect(config.memoryAddress))
  private val cacheLineRequest2Interconnect = Module(new CacheLineRequest2Interconnect(config.cacheCellDepth))
  // Enqueue
  io.enqs(0) <> aluExecuteQueue.io.enqAndType
  io.enqs(1) <> branchExecuteQueue.io.enqAndType
  io.enqs(2) <> memoryExecuteQueue.io.enqAndType
  io.enqs(3) <> fpuExecuteQueue.io.enqAndType

  // DTLB
  dtlb.io.sma <> sma2CacheLineRequestTLB.io.smaReader
  sma2CacheLineRequestTLB.io.request <> cacheLineRequest2Interconnect.io.in1
  dtlb.io.privilege := io.privilege
  dtlb.io.satp := io.satp
  dtlb.io.mstatus := io.mstatus
  dtlb.io.flush <> io.dTLBFlush

  // DCache
  dCache.io.outWriter <> io.smaWriter
  dCache.io.outReader <> cacheLineRequest2Interconnect.io.in2
  dCache.io.flush.req := false.B

  // StoreQueue
  storeQueue.io.deq <> storeQueueMemoryWriter.io.deq
  storeQueue.io.retire <> io.retire
  storeQueue.io.recover := io.recover

  // StoreQueueMemoryWriter
  storeQueueMemoryWriter.io.sma <> dCache.io.inWriter

  // SMAWithStoreQueueInterconnect
  smaWithStoreQueueInterconnect.io.bypass <> storeQueue.io.bypass
  smaWithStoreQueueInterconnect.io.out <> skipCacheSMAReaderInterconnect.io.in
  skipCacheSMAReaderInterconnect.io.out2 <> sma2CacheLineRequestStore.io.smaReader
  sma2CacheLineRequestStore.io.request <> dCache.io.inReader
  skipCacheSMAReaderInterconnect.io.out1 <> io.ioSMAReader

  // Store queue flush
  io.dCacheFlush <> storeQueue.io.flush

  // ALU
  alu.io.in <> aluExecuteQueue.io.deq
  alu.io.csr <> io.csr
  alu.io.mstatus := io.mstatus
  alu.io.privilege := io.privilege
  alu.io.recover := io.recover
  aluExecuteQueue.io.broadcast <> io.broadcast
  aluExecuteQueue.io.recover := io.recover

  // Branch
  branch.io.in <> branchExecuteQueue.io.deq
  branch.io.recover := io.recover
  branchExecuteQueue.io.broadcast <> io.broadcast
  branchExecuteQueue.io.recover := io.recover

  // Memory
  memory.io.in <> memoryExecuteQueue.io.deq
  memory.io.dtlb <> dtlb.io.request
  memory.io.sma <> smaWithStoreQueueInterconnect.io.in
  memory.io.alloc <> storeQueue.io.alloc
  memory.io.loadReservationUpdate <> io.updateLoadReservation
  memory.io.recover := io.recover
  memoryExecuteQueue.io.broadcast <> io.broadcast
  memoryExecuteQueue.io.recover := io.recover

  // FPU
  fpu.io.in <> fpuExecuteQueue.io.deq
  fpu.io.fcsr := io.fcsr
  fpu.io.recover := io.recover
  fpuExecuteQueue.io.broadcast <> io.broadcast
  fpuExecuteQueue.io.recover := io.recover

  // CacheLineRequest2Interconnect
  cacheLineRequest2Interconnect.io.out <> io.cacheLineRequest

  // Dequeue
  io.deqs(0) <> alu.io.out
  io.deqs(1) <> branch.io.out
  io.deqs(2) <> memory.io.out
  io.deqs(3) <> fpu.io.out
}

/** Core backend
  *
  * Responsible for instructing retire and update core status
  *
  * @param config
  *   Core config
  */
class CoreBackend(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val deqs = Vec(config.executeQueueWidth, Flipped(DecoupledIO(new ExecuteResultEntry())))

    val alloc = DecoupledIO(DataType.receipt)
    val tableWrite = Flipped(new ROBTableWriteIO())

    val broadcast = new DataBroadcastIO()
    val update = new RegisterUpdateIO()
    val predictorUpdate = new BranchPredictorUpdateIO()
    val store = new StoreQueueRetireIO()
    val recover = Output(new Bool())
    val correctPC = Output(DataType.address)

    val read = new CSRsReadIO()
    val privilege = Output(PrivilegeType())
    val mstatus = Output(DataType.operation)
    val satp = Output(DataType.operation)
    val fcsr = Output(DataType.operation)

    val dCacheFlush = new FlushCacheIO()
    val iCacheFlush = new FlushCacheIO()
    val tlbFlush = new FlushCacheIO()

    val updateLoadReservation = new LoadReservationUpdateIO()

    val mtime = Input(UInt(64.W))
    val mtimeIRQ = Input(Bool())

    val debug = new DebugIO()
  })
  private val broadcaster = Module(new RoundRobinBroadcaster(config.executeQueueWidth))
  private val instructionRetire = Module(new InstructionRetire(config.robDepth))
  private val rob = Module(new ROB(config.robDepth))
  private val csr = Module(new CSRs())

  // Broadcaster
  broadcaster.io.queues <> io.deqs
  broadcaster.io.broadcast <> io.broadcast
  broadcaster.io.robTableCommit <> rob.io.robTableCommit

  // InstructionRetire
  instructionRetire.io.retired <> rob.io.retired
  instructionRetire.io.robTableRetire <> rob.io.robTableRetire
  instructionRetire.io.registerUpdate <> io.update
  instructionRetire.io.loadReservationUpdate <> io.updateLoadReservation
  instructionRetire.io.predictorUpdate <> io.predictorUpdate
  instructionRetire.io.storeRetire <> io.store
  io.recover := instructionRetire.io.recover
  io.correctPC := instructionRetire.io.correctPC
  instructionRetire.io.csr <> csr.io.write
  instructionRetire.io.trap <> csr.io.trap
  instructionRetire.io.monitor <> csr.io.monitor
  instructionRetire.io.iCacheFlush <> io.iCacheFlush
  instructionRetire.io.dCacheFlush <> io.dCacheFlush
  instructionRetire.io.tlbFlush <> io.tlbFlush
  instructionRetire.io.debug <> io.debug

  instructionRetire.io.l2DCacheFlush.empty := true.B

  // ROB
  rob.io.alloc <> io.alloc
  rob.io.recover := instructionRetire.io.recover
  rob.io.robTableWrite <> io.tableWrite

  // CSR
  csr.io.read <> io.read
  io.privilege := csr.io.privilege
  io.mstatus := csr.io.mstatus
  io.satp := csr.io.satp
  io.fcsr := csr.io.fcsr
  csr.io.mtime := io.mtime
  csr.io.mtimeIRQ := io.mtimeIRQ
}
