package bergamot.core.debug

import chisel3._

/*
 * Core debug
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** RTL debug interface
  */
class DebugIO extends Bundle {
  val hit = Output(Bool()) // Hit breakpoint?
}
