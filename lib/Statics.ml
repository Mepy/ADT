
open Syntax
type tbl = (string, Type.t) Hashtbl.t
let decl (name:string) (ty:Type.t) (tbl:tbl) = match ty with 
  | Var(name') -> (
    match Hashtbl.find_opt tbl name' with
    | None -> failwith(" Type " ^ name' ^ " Not Found") 
    | Some(ty) -> Hashtbl.add tbl name ty
  )
  | ADT(_) -> Hashtbl.add tbl name ty

let subtract (pt0:Pattern.t) (pt1:Pattern.t) (types:tbl) : Pattern.t list =
  let open Pattern in
  let rec (--) pt0 pt1 : t list = 
    let auto cons0 args0 cons1 args1 = 
      if cons0=cons1 then 
        let folder ((total, leftover, i)) pt1 = 
          let pt0 = List.nth leftover i in
          let diff = pt0 -- pt1 in (* [i] 的差距 *)
          let tail = diff (* 计算除去 pt1 对应的其余分支  *)
            |> List.map (fun pt' -> leftover |> List.mapi (fun j pt -> 
              if j=i then pt' else pt ) ) 
            in
          let leftover = leftover (* 更新 leftover *)
            |> List.mapi (fun j pt -> 
              if j!=i then pt else (* 仅将[i]更新为 pt1 pt0 中较小者  *)
                ( match diff with
                | [] -> pt0   (* diff   空, 说明 pt0 < pt1 *)
                | _::_ -> pt1 (* diff 非空, 说明 pt0 > pt1 *)
                )
            ) in
          (total @ tail, leftover, i+1) 
        in
        let (argss0, _, _) = args1 
        |> List.fold_left folder (([], args0, 0))
        in
        argss0 |> List.map (fun args0 -> Cons(cons0, args0))
      else [Cons(cons0, args0)]
    in
    match pt0, pt1 with 
    | _, Var(_) -> []
    | Cons(cons0, args0), Cons(cons1, args1) ->
      auto cons0 args0 cons1 args1
    | Var(name), Cons(_, _) -> (
      match Hashtbl.find_opt types name with
      | None -> failwith(" Type " ^ name ^ " Not Found") 
      | Some(Type.Var(_)) -> failwith(" never Some(Var(_)) this case" ) 
      | Some(Type.ADT(branches)) -> (
        let pts0 = branches 
        |> List.map (fun ((cons0, args0)) -> 
          Cons(cons0, args0 |> List.map(fun name->Var(name))))
        in 
        pts0 
        |> List.map (fun pt0 -> pt0 -- pt1)
        |> List.concat
      )
    )
  in pt0 -- pt1

let subtracts (pts0:Pattern.t list) (pts1:Pattern.t list) (tbl:tbl) =
  let folder total pt1 = total 
    |> List.map (fun pt0 -> tbl |> subtract pt0 pt1)
    |> List.concat
  in pts1 |> List.fold_left folder pts0 


let check (tm:Term.t) (tbl:tbl) = 
  let rec recur tm = match tm with
  | Term.Var(_) -> ()
  | Term.Cons(_, tms) -> 
    (* 没有检查 cons, tms 是不是符合类型定义, 只是递归检查 *)
    tms |> List.iter recur 
  | Term.Case(tm, pt, cases) ->
    recur tm; (* 递归检查 *)
    let pts = cases |> List.map (fun ((pt, tm)) -> 
      recur tm; (* 递归检查 *)
      pt) in
    let pts' = tbl |> subtracts [pt] pts in
    Pattern.pps Format.std_formatter pts'
  in recur tm

  let walk progs = 
  let tbl : tbl = Hashtbl.create 10 in
  progs |> List.iter (function 
  | Program.Decl(name, ty) -> tbl |> decl name ty 
  | Program.Eval(tm) -> tbl |> check tm
  )
