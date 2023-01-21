open! Core

let start () =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  fun `Stop ->
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    Int63.((t1 - t0) / of_int 1_000_000)
