module Type = struct
type t = 
  | Var of string
  | ADT of (string * string list) list
end

module Pattern = struct

(**
  Pattern.Var(name) 中的 name 在 Syntax 时是 Term.Var(name);
  在 Statics 检查的时候是 Type.Var(name)
  *)
type t = 
  | Var of string 
  | Cons of string * t list

let rec pp fmt (pt:t) =
  let open Format in 
  match pt with
  | Var(_) -> fprintf fmt "_"
  | Cons(label, pts) -> fprintf fmt "%s%a" label pps pts
and
pps fmt (pts:t list)= 
  let open Format in 
  let length = List.length pts in
  if length = 0 then ()
  else
  fprintf fmt "@[<4>(";
  let final = length - 1 in
  let iteri idx ty = 
      pp fmt ty ;
      fprintf fmt (if idx = final then ")@]" else ",@,@ ")
  in 
  List.iteri iteri pts
end

module Term = struct
(** 简便起见, Case(tm, pt, cases) 中的 pt 是 tm 的模式, 
    其可由上文推导, 即先推导 tm 的类型 ty, 获取对应名称 name,
    然后转为 Pattern.Var(name)   
  *)
type t = 
  | Var of string
  | Cons of string * t list
  | Case of t * Pattern.t * (Pattern.t * t) list

end

module Program = struct
type t = 
  | Decl of string * Type.t
  | Eval of Term.t
end