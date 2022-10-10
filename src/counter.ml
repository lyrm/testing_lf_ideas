module Counter = struct
  type counter = { global : int Atomic.t; local : int Domain.DLS.key }

  let init () =
    { global = Atomic.make 0; local = Domain.DLS.new_key (fun () -> 0) }

  let incr t =
    let new_local = Domain.DLS.get t.local + 1 in
    let global = Atomic.get t.global in
    if Atomic.compare_and_set t.global global (global + new_local) then
      Domain.DLS.set t.local 0
    else Domain.DLS.set t.local new_local

  let get t =
    let local = Domain.DLS.get t.local in
    let global = Atomic.get t.global in
    if Atomic.compare_and_set t.global global (global + local) then
      Domain.DLS.set t.local 0
    else ();
    Atomic.get t.global + local

  let rec flush t =
    let local = Domain.DLS.get t.local in
    let global = Atomic.get t.global in
    if Atomic.compare_and_set t.global global (global + local) then
      Domain.DLS.set t.local 0
    else flush t
end

let test1 nit ndomain =
  let counter = Counter.init () in
  let domains =
    Array.init ndomain (fun _ ->
        Domain.spawn (fun () ->
            for _ = 0 to nit - 1 do
              Counter.incr counter
            done;
            Counter.flush counter))
  in
  Array.iter (fun domain -> Domain.join domain) domains;
  Counter.get counter

let test2 nit ndomain =
  let counter = Atomic.make 0 in
  let domains =
    Array.init ndomain (fun _ ->
        Domain.spawn (fun () ->
            for _ = 0 to nit - 1 do
              Atomic.set counter (Atomic.get counter + 1)
            done))
  in
  Array.iter (fun domain -> Domain.join domain) domains;
  Atomic.get counter

let test3 nit ndomain =
  let counter = Atomic.make 0 in
  let rec incr t =
    let value = Atomic.get t in
    if Atomic.compare_and_set t value (value + 1) then () else incr t
  in
  let domains =
    Array.init ndomain (fun _ ->
        Domain.spawn (fun () ->
            for _ = 0 to nit - 1 do
              incr counter
            done))
  in
  Array.iter (fun domain -> Domain.join domain) domains;
  Atomic.get counter

let main () =
  let nincr = 1000000 in
  let ndomain = 4 in
  let t0 = Unix.gettimeofday () in
  let r1 = test1 nincr ndomain in
  let t1 = Unix.gettimeofday () in
  let r2 = test2 nincr ndomain in
  let t2 = Unix.gettimeofday () in
  let r3 = test3 nincr ndomain in
  let t3 = Unix.gettimeofday () in
  Format.printf "Result DLS: %d in %f seconds \n@." r1 (t1 -. t0);
  Format.printf "Result without CAS: %d in %f seconds \n@." r2 (t2 -. t1);
  Format.printf "Result with    CAS: %d in %f seconds \n@." r3 (t3 -. t2)
;;

main ()
