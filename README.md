# A minimal dictionary implemented on ternary search trees

The module `Dict` ("dictionary") exports two submodules, `Std` and
`Opt`, whose signatures are those of a dictionary, that is, a
monotonic mapping from strings to definitions (no deletions).

The module `Opt` ("optimised") features dictionaries with faster
searches than those of module `Std` ("standard"), but no
insertions. An optimised dictionary is obtained from a standard one
when the vocabulary is fixed, and it is always possible to revert to
the standard version. In other words, a dictionary from module `Opt`
can be considered a constant and compact version of `Std`, with fast
searches.

These submodules export two versions of the same abstract data type: a
dictionary implemented by means of a ternary search tree.

In short, a *ternary search tree* is an optimisation of a *trie*,
where the children of a node are stored in a binary search tree
instead of a list.

The comparison of a character of the current input key to the
character in the current node yields three ways: to the left, if the
input character is strictly lower than the node character; to the
right if strictly greater, *and to the middle if equal*.

This data structure was fist published in *Fast Algorithms for
Searching and Sorting Strings*, J.L. Bentley and R. Sedgewick, in
Proceedings of the 8th Annual CAM-SIAM Symposium on Discrete
Algorithms, January, 1997.

We offer here two implementations in OCaml.
