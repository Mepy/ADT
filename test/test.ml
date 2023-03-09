[@@@warning "-33"]
[@@@warning "-32"]
open ADT.Syntax
open ADT.Statics


module Nat = struct

let nat = Type.ADT 
  [ ("Zero", [])
  ; ("Succ", ["nat"])
  ]

let pt_zero = Pattern.Cons("Zero", [])
let pt_succ n = Pattern.Cons("Succ", [n])

let tm_zero = Term.Cons("Zero", [])
let tm_succ n = Term.Cons("Succ", [n])

let tm0 = Term.Case(Var "n", Pattern.Var("nat"),
  [ (pt_succ(Pattern.Var("_")), tm_succ tm_zero)
  ; (pt_zero, tm_zero)]
  )

let tm1 = Term.Case(Var "n", Pattern.Var("nat"),
  [ (pt_succ(pt_succ(pt_succ(Pattern.Var("_")))), tm_succ tm_zero)
  ; (pt_zero, tm_zero)]
  )
  
let progs = Program.(
  [ Decl("nat", nat)
  (* ; Eval(tm0) *)
  ; Eval(tm1) (* Succ(Zero), Succ(Succ(Zero)) *)
  ])

end

(* 样例来自 https://www.zhihu.com/question/24460419/answer/86158686, 作者：Belleve *)
module Tree = struct

let tree = Type.ADT
  [ ("Leaf", [])
  ; ("Node", ["tree"; "tree"])
  ]

let septuple = Type.ADT
  [ ("Septuple", ["tree"; "tree"; "tree"; "tree"; "tree"; "tree"; "tree"])]

let pt_leaf = Pattern.Cons("Leaf", [])
let pt_node pt1 pt2 = Pattern.Cons("Node", [pt1; pt2])
let pt_sep pt1 pt2 pt3 pt4 pt5 pt6 pt7
  = Pattern.Cons("Septuple", [pt1; pt2; pt3; pt4; pt5; pt6; pt7])
let pt_var name = Pattern.Var name
let pt1 = pt_var "t1"
let pt2 = pt_var "t2"
let pt3 = pt_var "t3"
let pt4 = pt_var "t4"
let pt5 = pt_var "t5"
let pt6 = pt_var "t6"
let pt7 = pt_var "t7"
let pt8 = pt_var "t8"



let tm_leaf = Term.Cons("Leaf", [])
let tm_node tm1 tm2 = Term.Cons("Node", [tm1; tm2])
let tm_sep tm1 tm2 tm3 tm4 tm5 tm6 tm7
= Term.Cons("Septuple", [tm1; tm2; tm3; tm4; tm5; tm6; tm7])

let tm_var name = Term.Var name
let tm1 = tm_var "t1"
let tm2 = tm_var "t2"
let tm3 = tm_var "t3"
let tm4 = tm_var "t4"
let tm5 = tm_var "t5"
let tm6 = tm_var "t6"
let tm7 = tm_var "t7"
let tm8 = tm_var "t8"

(*

f :: (Tree, Tree, Tree, Tree, Tree, Tree, Tree) -> Tree
f (Leaf, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)                  = Leaf
f (t1, Node Leaf Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)          = Node t1  Leaf
f (Node t1 t2, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)            = Node t1 (Node t2 Leaf)
f (t1, Node (Node t2 t3) Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)  = Node t1 (Node t2 (Node t3 Leaf))
f (t1, Node t2 (Node t3 t4), Leaf, Leaf, Leaf, Leaf, Leaf)    = Node t1 (Node t2 (Node t3 (Node t4 Leaf)))
f (t1, t2, Node t3 t4, Leaf, Leaf, Leaf, Leaf)                = Node t1 (Node t2 (Node t3 (Node t4 (Node Leaf Leaf))))
f (t1, t2, t3, Node t4 t5, Leaf, Leaf, Leaf)                  = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 Leaf) Leaf))))
f (t1, t2, t3, t4, Node t5 t6, Leaf, Leaf)                    = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 Leaf)) Leaf))))
f (t1, t2, t3, t4, t5, t6, Node t7 t8)                        = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 (Node t7 t8))) Leaf))))
f (t1, t2, t3, t4, t5, Node t6 t7, Leaf)                      = Node t1 (Node t2 (Node t3 (Node t4 (Node t5 (Node t6 t7)))))

*)
let f = Term.Case(Term.Var("septuple"), Pattern.Var("septuple"),
  [ (pt_sep pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf,
      tm_leaf)
  ; (pt_sep pt1 (pt_node pt_leaf pt_leaf) pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf, 
      tm_node tm1 tm_leaf)
  ; (pt_sep (pt_node pt1 pt2) pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf,
      tm_node tm1 (tm_node tm2 tm_leaf) )
  ; (pt_sep pt1 (pt_node (pt_node pt2 pt3) pt_leaf ) pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf,
      tm_node tm1 (tm_node tm2 (tm_node tm3 tm_leaf))) 
  ; (pt_sep pt1 (pt_node pt2 (pt_node pt3 pt4)) pt_leaf pt_leaf pt_leaf pt_leaf pt_leaf,
      tm_node tm1 (tm_node tm2 (tm_node tm3 (tm_node tm4 tm_leaf))) )
  ; (pt_sep pt1 pt2 (pt_node pt3 pt4) pt_leaf pt_leaf pt_leaf pt_leaf,
      tm_node tm1 
        (tm_node tm2 
          (tm_node tm3 
            (tm_node tm4 (tm_node tm_leaf tm_leaf))))
    )
  ; (pt_sep pt1 pt2 pt3 (pt_node pt4 pt5) pt_leaf pt_leaf pt_leaf,
      tm_node tm1 
        (tm_node tm2 
          (tm_node tm3 
            (tm_node tm4 
              (tm_node (tm_node tm5 tm_leaf) tm_leaf))))
    )
  ; (pt_sep pt1 pt2 pt3 pt4 (pt_node pt5 pt6) pt_leaf pt_leaf,
      tm_node tm1 
        (tm_node tm2 
          (tm_node tm3 
            (tm_node tm4  
              (tm_node (tm_node tm5 (tm_node tm6 tm_leaf)) tm_leaf))))
    )
  ; (pt_sep pt1 pt2 pt3 pt4 pt5 pt6 (pt_node pt7 pt8),
      tm_node tm1 
        (tm_node tm2 
          (tm_node tm3 
            (tm_node tm4  
              (tm_node (tm_node tm5 (tm_node tm6 (tm_node tm7 tm8))) tm_leaf))))
    )
  ; (pt_sep pt1 pt2 pt3 pt4 pt5 (pt_node pt6 pt7) pt_leaf,
      tm_node tm1 
        (tm_node tm2 
          (tm_node tm3 
            (tm_node tm4  
              (tm_node tm5 (tm_node tm6 tm7 )))))
    )

  ])

(*
g :: Tree -> (Tree, Tree, Tree, Tree, Tree, Tree, Tree)
g Leaf                                                                                 = (Leaf, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 Leaf)                                                                       = (t1, Node Leaf Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 Leaf))                                                             = (Node t1 t2, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 Leaf)))                                                   = (t1, Node (Node t2 t3) Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 Leaf))))                                         = (t1, Node t2 (Node t3 t4), Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node Leaf Leaf)))))                             = (t1, t2, Node t3 t4, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 Leaf) Leaf)))))                   = (t1, t2, t3, Node t4 t5, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 Leaf)) Leaf)))))         = (t1, t2, t3, t4, Node t5 t6, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 (Node t7 t8))) Leaf))))) = (t1, t2, t3, t4, t5, t6, Node t7 t8)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node t5 (Node t6 t7))))))                       = (t1, t2, t3, t4, t5, Node t6 t7, Leaf)
*)
let g = Term.Case(Term.Var("tree"), Pattern.Var("tree"), 
  [ (pt_leaf, 
      tm_sep tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 pt_leaf, 
      tm_sep tm1 (tm_node tm_leaf tm_leaf) tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 pt_leaf),
      tm_sep (tm_node tm1 tm2) tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 pt_leaf)),
      tm_sep tm1 (tm_node (tm_node tm2 tm3) tm_leaf) tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 pt_leaf))),
      tm_sep tm1 (tm_node tm2 (tm_node tm3 tm4)) tm_leaf tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 (pt_node pt_leaf pt_leaf)))),
      tm_sep tm1 tm2 (tm_node tm3 tm4) tm_leaf tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 (pt_node (pt_node pt5 pt_leaf) pt_leaf)))),
      tm_sep tm1 tm2 tm3 (tm_node tm4 tm5) tm_leaf tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 (pt_node (pt_node pt5 (pt_node pt6 pt_leaf)) pt_leaf)))),
      tm_sep tm1 tm2 tm3 tm4 (tm_node tm5 tm6) tm_leaf tm_leaf)
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 (pt_node (pt_node pt5 (pt_node pt6 (pt_node pt7 pt8))) pt_leaf)))),
      tm_sep tm1 tm2 tm3 tm4 tm5 tm6 (tm_node tm7 tm8))
  ; (pt_node pt1 (pt_node pt2 (pt_node pt3 (pt_node pt4 (pt_node pt5 (pt_node pt6 pt7))))),
      tm_sep tm1 tm2 tm3 tm4 tm5 tm6 tm7)
  ])


let progs = Program.(
  [ Decl("tree", tree)
  ; Decl("septuple", septuple)
  ; Eval(f)
  ; Eval(g)
  ])
end


let _ = walk Nat.progs
let _ = walk Tree.progs
