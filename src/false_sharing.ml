let print_pair ?name (r1, r2) =
  match name with
  | None -> ()
  | Some s ->
      Format.printf "test %d\n" s;
      Format.printf "%.4f, %.4f\n" r1 r2

(* launch [test] [nr] times and return median and average time value
   in milliseconds. *)
let rerun nr test =
  let res = Array.map (fun _ -> test () *. 1000.) (Array.make nr 0.) in
  ( res.((nr / 2) - 1),
    Array.fold_left (fun avg elt -> avg +. elt) 0. res /. float_of_int nr )

(* Test 1 : domains increment adjacent cells *)
let test1 nd niter =
  let test1 = Array.make nd 0 in

  let rec loop acc_domains i =
    if i = nd then acc_domains
    else
      let domain =
        Domain.spawn (fun () ->
            let t1 = Unix.gettimeofday () in
            for _ = 0 to niter - 1 do
              test1.(i) <- test1.(i) + 1
            done;
            Unix.gettimeofday () -. t1)
      in
      loop (domain :: acc_domains) (i + 1)
  in
  let domains = loop [] 0 in
  List.map (fun domain -> Domain.join domain) domains
  |> List.fold_left (fun avg time -> (time /. float_of_int nd) +. avg) 0.

(* Test 2 : same but each cell of the array is an Atomic.t *)
let test2 nd niter =
  let test2 = Array.make nd (Atomic.make 0) in
  let domains =
    Array.mapi
      (fun _ elt ->
        Domain.spawn (fun () ->
            let t1 = Unix.gettimeofday () in
            for _ = 0 to niter - 1 do
              Atomic.incr elt
            done;
            Unix.gettimeofday () -. t1))
      test2
  in
  Array.map (fun domain -> Domain.join domain) domains
  |> Array.fold_left (fun avg time -> (time /. float_of_int nd) +. avg) 0.

(* Test 3 : domains increment their own cells. The cells with a
   counter are well spaced in the array. *)
let test3 nd niter =
  let dilution = 40 in
  let test3 = Array.make (dilution * nd) 0 in

  let rec loop acc_domains i =
    if i = nd then acc_domains
    else
      let domain =
        Domain.spawn (fun () ->
            let t1 = Unix.gettimeofday () in
            for _ = 0 to niter - 1 do
              test3.(i * dilution) <- test3.(i * dilution) + 1
            done;
            Unix.gettimeofday () -. t1)
      in
      loop (domain :: acc_domains) (i + 1)
  in
  let domains = loop [] 0 in
  List.map (fun domain -> Domain.join domain) domains
  |> List.fold_left (fun avg time -> (time /. float_of_int nd) +. avg) 0.

(* Test 4 : with an array of reference *)
let test4 nd niter =
  let test3 = Array.make nd (ref 0) in

  let rec loop acc_domains i =
    if i = nd then acc_domains
    else
      let domain =
        Domain.spawn (fun () ->
            let t1 = Unix.gettimeofday () in
            for _ = 0 to niter - 1 do
              incr test3.(i)
            done;
            Unix.gettimeofday () -. t1)
      in
      loop (domain :: acc_domains) (i + 1)
  in
  let domains = loop [] 0 in
  List.map (fun domain -> Domain.join domain) domains
  |> List.fold_left (fun avg time -> (time /. float_of_int nd) +. avg) 0.

let main () =
  let usage_msg = "false_sharing -d [ndomain] -r [niter]" in
  let ndomain = ref 0 in
  let nrun = ref 0 in
  let test = ref 0 in
  let speclist =
    [
      ("-d", Arg.Set_int ndomain, "Number of domains");
      ("-r", Arg.Set_int nrun, "Number of runs");
      ("-t", Arg.Set_int test, "Test number");
    ]
  in
  let () = Arg.parse speclist (fun _ -> ()) usage_msg in
  let nd, nr = (!ndomain, !nrun) in
  let res =
    match !test with
    | 1 -> rerun nr @@ fun () -> test1 nd 10000
    | 2 -> rerun nr @@ fun () -> test2 nd 10000
    | 3 -> rerun nr @@ fun () -> test3 nd 10000
    | 4 -> rerun nr @@ fun () -> test4 nd 10000
    | _ -> failwith "pas de tests"
  in

  print_pair ~name:!test res

let _ = main ()
