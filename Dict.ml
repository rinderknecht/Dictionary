(** This module exports two versions of the same abstract data type: a
    dictionary implemented by means of a ternary search tree.

      In short, a ternary search tree is an optimisation of a {i
    trie}, where the children of a node are stored in a binary search
    tree instead of a list.

      The comparison of a character of the current input key to the
    character in the current node yields three ways: to the left, if
    the input character is strictly lower than the node character; to
    the right if strictly greater, {i and to the middle if equal}.

      This data structure was fist published in {i Fast Algorithms for
    Searching and Sorting Strings}, J.L. Bentley and R. Sedgewick, in
    Proceedings of the 8th Annual CAM-SIAM Symposium on Discrete
    Algorithms, January, 1997.

      We offer here two implementations in OCaml.

     One version is provided by the signature [Std] ("standard"),
    whilst the other by [Opt] ("optimised"), the former enabling
    insertions, whilst the latter offers path compression (hence
    faster searches), but no insertions. Conversions between the two
    representations are provided.
*)

type word = string
type 'def entry = word * 'def

(** {1 Utility functions} *)

(** The call [rev_chars l] evaluates in the string made of the
    characters in the list [l], in reverse order. For example,
    [rev_chars ['a';'b';'c'] = "cba"]. *)
let rev_chars (l: char list) : string =
  let len = List.length l in
  let seq = Bytes.create len in
  let rec make i = function
      [] -> assert (i = -1); Bytes.to_string seq
  | c::l -> Bytes.set seq i c; make (i-1) l
in make (len-1) l

(** The call [rcat s] is the string made of the concatenation of the
    strings in the list [s], in reverse order. For example, [rcat
    ["ab";"c";"";"def"] = "defcab"]. *)
let rcat (stack: string list) : string =

  let len = List.fold_left (fun a l -> a + String.length l) 0 stack in
  let seq = Bytes.create len in

  let copy j s =
    let rec cp i j =
      if i = -1 then j else (Bytes.set seq j s.[i]; cp (i-1) (j-1))
    in cp (String.length s - 1) j in

  let rec make j = function
      [] -> assert (j = -1); Bytes.to_string seq
  | s::l -> make (copy j s) l

  in make (len-1) stack

(** {1 The two implementations of ternary search trees} *)

module rec Std : sig
  type 'def t =
    Nil
  | Pre of 'def * 'def t
  | Leg of char * 'def t * 'def t * 'def t

  type 'def dict = 'def t

  val empty       : 'def t
  val add         : word -> 'def -> 'def t -> 'def t
  val load        : 'def entry list -> 'def t
  val find        : word -> 'def t -> 'def
  val find_opt    : word -> 'def t -> 'def option
  val suffix      : char -> 'def t -> 'def t
  val entries     : 'def t -> 'def entry list
  val definitions : 'def t -> 'def list
  val inorder     : 'def t -> 'def entry list
  val freeze      : 'def t -> 'def Opt.t

  exception Found of word
end
= struct
  (** A dictionary of type {! t} is implemented as a ternary
      search tree. *)
  type 'def t =
    Nil   (** [Nil] denotes an empty dictionary. *)
  | Pre of 'def * 'def t
    (** [Pre (d,s)] ("prefix") denotes the dictionary where the empty
        word is mapped to the definition [d] and other words may be
        found in the dictionary denoted by the subtree [s]. *)
  | Leg of char * 'def t * 'def t * 'def t
    (** [Leg (c,l,m,r)] ("character, lower, equal, greater") is tree
        whose root is the character [c], left subtree is [l], middle
        subtree is [m] and right subtree is [r]:
        {ul
          {li the subtree [l] denotes the dictionary containing words
              starting with a letter lower than [c], in alphabetic
              order;}
          {li the subtree [m] denotes a dictionary containing words such
              that prefixing them with the character [c], they belong to
              the dictionary denoted by [Leg (c,l,m,r)];}
          {li the subtree [r] denotes a dictionary holding words
              starting with a letter greater than [c], in alphabetic
              order.}} *)

  type 'def dict = 'def t

  let empty = Nil

  exception Found of word

  let add word def dict =
    let len = String.length word in

    (** The evaluation of the call [mk_suffix n] is a branch of a
        ternary search tree made of [n] nodes [Leg] with empty
        side-trees, ended by a node [Pre], holding the the definition
        [def] captured as an argument of [add] in the scope. *)
    let rec mk_suffix (n: int) =
      if n = 0 then Pre (def,Nil)
      else Leg (word.[len-n], Nil, mk_suffix(n-1), Nil) in

    (** The call [insert n dict] evaluates in a dictionary containing
        all the entries of dictionary [dict], plus the entry
        [(word.[len-n..len-1],def)], where [len] is the length of
        [word] and [word[s..e]] denotes the substring of [word]
        starting at position [s] and ending at position [e]. Clearly,
        the call [insert len dict] results in the addition of the
        entry [(word,def)] to [dict], except if [word] is already in
        [dict], in which case the exception [Found word] is raised. *)
    let rec insert (n: int) = function
                     Nil -> mk_suffix n
    |          Pre (d,t) -> if n = 0 then
                              if d = def then dict else raise (Found word)
                            else Pre (d, insert n t)
    | Leg (c,l,m,r) as t -> if n = 0 then Pre (def,t)
                            else let c' = word.[len-n]
                                 in if c' < c then Leg (c, insert n l, m, r)
                                    else if c' > c then Leg (c, l, m, insert n r)
                                         else Leg (c, l, insert (n-1) m, r)

    in insert len dict

  let load entries = List.fold_left (fun a (w,d) -> add w d a) Nil entries

  let find word : 'def t -> 'def =
    let len = String.length word in
    let rec search (i: int) = function
                Nil -> raise Not_found
    |     Pre (d,t) -> if i = len then d else search i t
    | Leg (c,l,m,r) -> if i = len then raise Not_found
                       else if word.[i] = c then search (i+1) m
                            else search i (if word.[i] < c then l else r)
    in search 0

  let find_opt word dict : 'def option =
    try Some (find word dict) with Not_found -> None

  (** The entries of a dictionary implemented as a ternary search tree
     are located at the fringe of the tree. The alphabetic order is
     achieved by a leftward post-order traversal, pushing the entries
     in a stack. *)
  let entries (dict: 'def t) : 'def entry list =
    let rec post path entries = function
                Nil -> entries
    |     Pre (d,t) -> (rcat path, d) :: post path entries t
    | Leg (c,l,m,r) -> let path' = String.make 1 c :: path
                       in post path (post path' (post path entries r) m) l
    in post [] [] dict

  let definitions dict : 'def list =
    let rec post defs = function
                Nil -> defs
    |     Pre (d,t) -> d :: post defs t
    | Leg (_,l,m,r) -> post (post (post defs r) m) l
    in post [] dict

  let rec suffix (c: char) = function
              Nil -> Nil
  |     Pre (_,t) -> suffix c t
  | Leg (x,l,m,r) -> if c = x then m else suffix c (if c < x then l else r)

  (** The call [inorder dict] evaluates in the list of entries in the
      dictionary [dict], sorted in order, i.e., infix traversal of the
      underlying ternary search tree. The function [inorder] is useful
      when an optimal tree is designed and the corresponding entries
      are needed in the proper order to reconstruct it by calling
      [load] below. *)
  let inorder dict : 'def entry list =
    let rec post path entries = function
                Nil -> entries
    |     Pre (d,t) -> (rcat path, d) :: post path entries t
    | Leg (c,l,m,r) -> let path' = String.make 1 c :: path
                      in post path' (post path (post path entries r) l) m
  in post [] [] dict

  (** The compression of a standard ternary search tree consists in
      compressing paths made only of [Leg] constructors with empty
      side-trees, and replacing [Pre] nodes with an empty subtree by a
      specific constructor [Def]. *)
  let rec compress (p: char list) : 'def t -> 'def Opt.pre_t = function
    Leg (c,Nil,m,Nil) -> compress (c::p) m
  |                 t -> Opt.Cmp (rev_chars p, opti t)

  and opti : 'def t -> 'def Opt.pre_t = function
                  Nil -> Opt.Nil
  |       Pre (d,Nil) -> Opt.Def d
  |         Pre (d,t) -> Opt.Pre (d, opti t)
  | Leg (c,Nil,m,Nil) -> compress [c] m
  |     Leg (c,l,m,r) -> Opt.Leg (c, opti l, opti m, opti r)

  let freeze : 'def t -> 'def Opt.t = fun t -> 0, opti t
end

and Opt : sig
  type 'def pre_t =
    Nil
  | Pre of 'def * 'def pre_t
  | Leg of char * 'def pre_t * 'def pre_t * 'def pre_t
  | Def of 'def
  | Cmp of string * 'def pre_t

  type 'def t = int * 'def pre_t
  type 'def dict = 'def t

  val empty       : 'def t
  val suffix      : char -> 'def t -> 'def t
  val find        : word -> 'def t -> 'def
  val find_opt    : word -> 'def t -> 'def option
  val entries     : 'def t -> 'def entry list
  val definitions : 'def t -> 'def list
  val inorder     : 'def t -> 'def entry list
  val thaw        : 'def t -> 'def Std.t
end = struct

  (** To save some memory, we can add a couple of value constructors
      [Def] and [Cmp], to the type [t], which we rename [pre_t] in the
      process. The structural constraints are the following:
      {ol
        {li The constructors [Def] and [Pre] can only occur as the
          second argument of [Cmp] or the third of [Leg];}
        {li The unary constructor [Nil] can only occur as the second or
         fourth argument of [Leg]; it is also best, although not
         mandatory, to use [Cmp] instead if [Nil] occurs in both
         positions;}
        {li the first argument of [Cmp] should not be [""];}
        {li the second argument of [Pre] must either be [Leg] or [Cmp].}}
  *)
  type 'def pre_t =
    Nil
  | Pre of 'def * 'def pre_t
  | Leg of char * 'def pre_t * 'def pre_t * 'def pre_t
  | Def of 'def
      (** The value [Def d] ({i definition}) is a tree containing only
          the definition [d], that is, it represents the mapping from the
          empty word to [d]. In fact, [Def d] is equivalent to
          [Pre(d,Nil)].*)
  | Cmp of string * 'def pre_t
      (** The value [Cmp (p,s)] ({i compressed path}) is a tree
          containing words with the maximum prefix [p] in common, and
          whose suffixes are in the subtree [s], that is, it denotes a
          dictionary whose entries start with [p] and whose
          definitions are in [s]. It can be understood as the
          compression of a linear path in the tree, so [Comp(p,s)] is
          the same as [Leg(p.[0], Nil, Leg(p.[1], Nil, ..., Leg(p.[n],
          Nil, s, Nil), Nil)...), Nil)], where [n = String.length p]. *)

  type 'def t = int * 'def pre_t

  type 'def dict = 'def t

  let empty = 0,Nil

  let rec suffix (c: char) : 'def t -> 'def t = function
    _,        Pre (_,t') -> suffix c (0,t')
  | _,     (Nil | Def _) -> empty
  | _,     Leg (x,l,m,r) -> if c = x then 0,m
                            else suffix c (0, if c < x then l else r)
  | j, (Cmp (p,t') as t) -> if p.[j] = c
                            then if String.length p = j+1
                                 then 0,t' else j+1,t
                            else empty

  let find key ((_,dict): 'def t) : 'def =
    let len = String.length key in
    let rec search (i: int) (j: int) = function
                  Nil -> raise Not_found
    |           Def d -> if i = len then d else raise Not_found
    |       Pre (d,t) -> if i = len then d else search i j t
    |   Leg (c,l,m,r) -> if i = len then raise Not_found
                         else if key.[i] = c then search (i+1) j m
                              else search i j (if key.[i] < c then l else r)
    | Cmp (p,t') as t -> if j = String.length p
                         then search i 0 t'
                         else if i = len then raise Not_found
                              else if key.[i] = p.[j]
                                   then search (i+1) (j+1) t
                                   else raise Not_found
    in search 0 0 dict

  let find_opt key dict =
    try Some (find key dict) with Not_found -> None

  let entries (_,dict) =
    let rec post path entries = function
                Nil -> entries
    |         Def d -> (rcat path, d) :: entries
    |     Pre (d,t) -> (rcat path, d) :: post path entries t
    |     Cmp (p,t) -> post (p::path) entries t
    | Leg (c,l,m,r) -> let path' = String.make 1 c :: path
                       in post path (post path' (post path entries r) m) l
    in post [] [] dict

  let definitions (_,dict) =
    let rec post defs = function
                Nil -> defs
    |         Def d -> d::defs
    |     Pre (d,t) -> d :: post defs t
    |     Cmp (_,t) -> post defs t
    | Leg (_,l,m,r) -> post (post (post defs r) m) l
    in post [] dict

  let inorder (_,dict) =
    let rec post path entries = function
                Nil -> entries
    |         Def d -> (rcat path, d) :: entries
    |     Pre (d,t) -> (rcat path, d) :: post path entries t
    |     Cmp (p,t) -> post (p::path) entries t
    | Leg (c,l,m,r) -> let path' = String.make 1 c :: path
                       in post path' (post path (post path entries r) l) m
    in post [] [] dict

  let rec expand (path: string) (t: 'def pre_t) =
    let len = String.length path in
    let rec exp (i: int) : 'def Std.t =
      if i < len then Std.Leg (path.[i],Std.Nil,exp(i+1),Std.Nil) else norm t
    in exp 0

  and norm : 'def pre_t -> 'def Std.t = function
              Nil -> Std.Nil
  |         Def d -> Std.Pre (d, Std.Nil)
  |     Pre (d,t) -> Std.Pre (d, norm t)
  | Leg (c,l,m,r) -> Std.Leg (c, norm l, norm m, norm r)
  |     Cmp (p,t) -> expand p t

  let thaw : 'def t -> 'def Std.t = fun (_,t) -> norm t
end
