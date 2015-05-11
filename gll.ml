(* elementary descriptors, sect 2 *)

type label_t = LS | L0 | LS1 | L1 | L2 | LS2 | L3 | L4 | LS3 | LA | LB

type extlbl_t = Extlbl of (int * label_t)

let extlbl_to_label = fun (Extlbl (i,l)) -> l

type stack_t = extlbl_t list

type descriptor_t = label_t * stack_t * int

type 'a finset = 'a list

type r_t = descriptor_t finset

type state_t = { st_pc: label_t; st_i:int; st_r:r_t; st_stk:stack_t; st_input:string }

let s0 = { st_pc = LS; st_i=0; st_r=[]; st_stk=[Extlbl(0,L0)]; st_input="aad$" }

let result = ref s0

let step s0 = (
  let pop s0 i = 
    { s0 with 
      st_pc=L0; 
      st_r=s0.st_r@[
          match s0.st_stk with
          | [] -> failwith "impossible 04o"
          | x::stk' -> (
              let lbl = extlbl_to_label x in
              (lbl,stk',i))]
    }
  in
  match s0.st_pc with
  | LS -> (
      let append1 r x = r@[x] in
      let r' = 
        s0.st_r 
        |> (fun r -> if (List.mem (s0.st_input.[s0.st_i]) ['a';'c']) then append1 r (LS1,s0.st_stk,s0.st_i) else r)
        |> (fun r -> if (List.mem (s0.st_input.[s0.st_i]) ['a';'b']) then append1 r (LS2,s0.st_stk,s0.st_i) else r)
        |> (fun r -> if (List.mem (s0.st_input.[s0.st_i]) ['d';'$']) then append1 r (LS3,s0.st_stk,s0.st_i) else r)
      in
      { s0 with st_pc=L0; st_r=r'})
  | L0 -> (
      match s0.st_r with
      | [] -> failwith "finished"
      | (lbl,st,i)::r' -> (
          if (lbl=L0) && (st=[]) && (i=(String.length s0.st_input - 1)) then  (* -1 because we parse upto the $ *)
            (result:=s0; failwith "success")
(*          else if st=[] then
            (* failwith "stack empty, but not successful"; we should
               ignore and try to find a successful parse; perhaps the
               next clause does this; probably it does *)
            let dummy=[] in
            { s0 with st_pc=L0; st_r=r'} *)
          else
            { s0 with st_pc=lbl; st_stk=st; st_i=i; st_r=r' } ))
  | LS1 -> (
      { s0 with st_pc=LA; st_stk=(Extlbl(s0.st_i,L1)::s0.st_stk) })
  | L1 ->  (
      { s0 with st_pc=LS; st_stk=(Extlbl(s0.st_i,L2)::s0.st_stk) })
  | L2 -> (
      if (s0.st_input.[s0.st_i]) = 'd' then 
        pop s0 (s0.st_i+1)
      else 
        { s0 with st_pc=L0 })
  | LS2 ->  (
      { s0 with st_pc=LB; st_stk=(Extlbl(s0.st_i,L3)::s0.st_stk) })
  | L3 -> (
      { s0 with st_pc=LS; st_stk=(Extlbl(s0.st_i,L4)::s0.st_stk) })
  | L4 -> (pop s0 s0.st_i)
  | LS3 -> (pop s0 s0.st_i)
  | LA -> (
      if (List.mem (s0.st_input.[s0.st_i]) ['a';'c']) then 
        pop s0 (s0.st_i+1)
      else
        { s0 with st_pc=L0 })
  | LB -> (
      if (List.mem (s0.st_input.[s0.st_i]) ['a';'b']) then 
        pop s0 (s0.st_i+1)
      else
        { s0 with st_pc=L0 })
)

(* stepping manually *)
let tmp = s0
let tmp = step tmp
(* ... *)

(* stepping with a while *)
let t = ref s0
let _ = 
  while true do
    t:=step !t
  done

(* result is stored in t; also in result *)

