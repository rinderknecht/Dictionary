(* The module [Dict] ("dictionary") exports two submodules, [Std] and
   [Opt], whose signatures are those of a dictionary, that is, a
   monotonic mapping from strings to definitions (no deletions).

     The module [Opt] ("optimised") features dictionaries with faster
   searches than those of module [Std] ("standard"), but no
   insertions. An optimised dictionary is obtained from a standard one
   when the vocabulary is fixed, and it is always possible to revert
   to the standard version. In other words, a dictionary from module
   [Opt] can be considered a constant and compact version of [Std],
   with fast searches.  *)

(* The type [word] is used in contexts where strings denote words in
   the dictionary. *)

type word = string

(* The type [entry represents entries in a dictionary of definitions
   of type [def], that is, pairs of a word and a definition. *)

type 'def entry = word * 'def

(* The module [Std] ("standard") provides a simple data type for
   dictionaries. *)

module rec Std :
  sig

    (* The type [t] denotes a dictionary, that is, a mapping from
       words of type [string] to definitions of type [def]. *)

    type 'def t

    (* The type [dict] is an alias to be used when opening the module
       [Std]. *)

    type 'def dict = 'def t

    (* The empty dictionary. *)

    val empty : 'def t

    (* Let [char] be a character and [dict] a dictionary. If [dict]
       contains words starting with [char], then the value of [suffix
       char dict] is the dictionary containing those words {i without
       their first letter}; if there are no such words, the value is
       the empty dictionary. *)

    val suffix : char -> 'def t -> 'def t

    (* If a dictionary [dict] contains a definition [def] of the word
       [word], then the value of [find word dict] is [def], otherwise
       the exception [Not_found] is raised. *)

    val find : word -> 'def t -> 'def

    (* If a dictionary [dict] contains a definition [def] for the word
       [word], then the value of [find_opt word dict] is [Some def],
       otherwise [None]. *)

    val find_opt : word -> 'def t -> 'def option

    (* The value of [entries dict] is the list of the entries in the
       dictionary [dict], sorted in alphabetic order. *)

    val entries : 'def t -> 'def entry list

    (* The value of [definitions dict] is the list of definitions in
       the dictionary [dict], sorted in alphabetic order.  It is
       equivalent to [List.map snd (entries dict)].*)

    val definitions : 'def t -> 'def list

    (* The call [add word def dict] evaluates in a dictionary
       containing all the entries of the dictionary [dict], plus the
       entry [(word, def)]. If [word] was already present in [dict],
       the exception [Found word] ("redefinition") is raised, else, if
       the entry [(word, def)] was already present in [dict], then
       [dict] is returned (implying physical equality: [dict == add
       word def dict]). *)

    val add : word -> 'def -> 'def t -> 'def t

    (* The exception [Found w] is raised when adding the word [w] to a
       dictionary where it is already defined. In other words, there
       is no shadowing of previous entries by default. *)

    exception Found of word

    (* The call [load entries] evaluates in a dictionary containing
       exactly the entries in [entries]. *)

    val load : 'def entry list -> 'def t

    (* Converting to the (space-)optimised representation (see module
       [Opt]). The cross-module invariants are [Std.freeze (Opt.thaw
       d) = d] and [Opt.thaw (Std.freeze d) = d] *)

    val freeze : 'def t -> 'def Opt.t

    (* (Maintainer) The call [inorder dict] evaluates in the list of
       entries in the dictionary [dict], sorted in order, i.e., infix
       traversal of the underlying ternary search tree. The function
       [inorder] is useful when an optimal tree is designed and the
       corresponding entries are needed in the proper order to
       reconstruct it by calling [load] below. *)

    val inorder : 'def t -> 'def entry list
  end

(* The module [Opt] ("optimised") provides compact dictionaries. *)

and Opt :
  sig

    type 'def t
    type 'def dict = 'def t

    val empty       : 'def t
    val suffix      : char -> 'def t -> 'def t
    val find        : word -> 'def t -> 'def
    val find_opt    : word -> 'def t -> 'def option
    val entries     : 'def t -> 'def entry list
    val definitions : 'def t -> 'def list
    val inorder     : 'def t -> 'def entry list

    (* Converting to the standard representation (see module [Std]).
       The cross-module invariants are [Std.freeze (Opt.thaw d) = d]
       and [Opt.thaw (Std.freeze d) = d] *)

    val thaw : 'def t -> 'def Std.t
  end
