(** Tiny unit test for the dictionaries implemented as ternary search trees *)

(** The value [kwd_dict] is a dictionary of ASN.1 keywords. Their order
   in the list [keywords] has been determined to yield the optimal
   ternary search tree after sequential insertions (with
   List.fold_left). *)
type kwd =
  ABSENT | ABSTRACT_SYNTAX | ALL | APPLICATION | AUTOMATIC | BEGIN
| BIT | BMPString | BOOLEAN | BY | CHARACTER | CHOICE | CLASS
| COMPONENT | COMPONENTS | CONSTRAINED | CONTAINING | DATE | DATE_TIME
| DEFAULT | DEFINITIONS | DURATION | EMBEDDED | ENCODED | ENCODING_CONTROL
| END | ENUMERATED | EXCEPT | EXPLICIT | EXPORTS | EXTENSIBILITY
| EXTERNAL | FALSE | FROM | GeneralizedTime | GeneralString | GraphicString
| IA5String | IDENTIFIER | IMPLICIT | IMPLIED | IMPORTS | INCLUDES | INSTANCE
| INSTRUCTIONS | INTEGER | INTERSECTION | ISO646String | MAX | MIN
| MINUS_INFINITY | NOT_A_NUMBER | NULL | NumericString | OBJECT
| ObjectDescriptor | OCTET | OF | OID_IRI | OPTIONAL | PATTERN | PDV
| PLUS_INFINITY | PRESENT | PRIVATE | PrintableString | REAL | RELATIVE_OID
| RELATIVE_OID_IRI | SEQUENCE | SET | SETTINGS | SIZE | STRING | SYNTAX
| T61String | TAGS | TeletexString | TIME | TIME_OF_DAY | TRUE
| TYPE_IDENTIFIER | UNION | UNIQUE | UNIVERSAL | UniversalString | UTCTime
| UTF8String | VideotexString | VisibleString | WITH

let string_of_kwd = function
  ABSENT -> "ABSENT" | ABSTRACT_SYNTAX -> "ABSTRACT-SYNTAX" | ALL -> "ALL"
| APPLICATION -> "APPLICATION" | AUTOMATIC -> "AUTOMATIC" | BEGIN -> "BEGIN"
| BIT -> "BIT" | BMPString -> "BMPString" | BOOLEAN -> "BOOLEAN" | BY -> "BY"
| CHARACTER -> "CHARACTER" | CHOICE -> "CHOICE" | CLASS -> "CLASS"
| COMPONENT -> "COMPONENT" | COMPONENTS -> "COMPONENTS"
| CONSTRAINED -> "CONSTRAINED" | CONTAINING -> "CONTAINING" | DATE -> "DATE"
| DATE_TIME -> "DATE-TIME" | DEFAULT -> "DEFAULT" | DEFINITIONS -> "DEFINITIONS"
| DURATION -> "DURATION" | EMBEDDED -> "EMBEDDED" | ENCODED -> "ENCODED"
| ENCODING_CONTROL -> "ENCODING-CONTROL" | END -> "END"
| ENUMERATED -> "ENUMERATED" | EXCEPT -> "EXCEPT" | EXPLICIT -> "EXPLICIT"
| EXPORTS -> "EXPORTS" | EXTENSIBILITY -> "EXTENSIBILITY"
| EXTERNAL -> "EXTERNAL" | FALSE -> "FALSE" | FROM -> "FROM"
| GeneralizedTime -> "GeneralizedTime" | GeneralString -> "GeneralString"
| GraphicString -> "GraphicString" | IA5String -> "IA5String"
| IDENTIFIER -> "IDENTIFIER" | IMPLICIT -> "IMPLICIT" | IMPLIED -> "IMPLIED"
| IMPORTS -> "IMPORTS" | INCLUDES -> "INCLUDES" | INSTANCE -> "INSTANCE"
| INSTRUCTIONS -> "INSTRUCTIONS" | INTEGER -> "INTEGER"
| INTERSECTION -> "INTERSECTION" | ISO646String -> "ISO646String" | MAX -> "MAX"
| MIN -> "MIN" | MINUS_INFINITY -> "MINUS-INFINITY"
| NOT_A_NUMBER -> "NOT-A-NUMBER" | NULL -> "NULL"
| NumericString -> "NumericString" | OBJECT -> "OBJECT"
| ObjectDescriptor -> "ObjectDescriptor" | OCTET -> "OCTET" | OF -> "OF"
| OID_IRI -> "OID-IRI" | OPTIONAL -> "OPTIONAL" | PATTERN -> "PATTERN"
| PDV -> "PDV" | PLUS_INFINITY -> "PLUS-INFINITY" | PRESENT -> "PRESENT"
| PRIVATE -> "PRIVATE" | PrintableString -> "PrintableString" | REAL -> "REAL"
| RELATIVE_OID -> "RELATIVE-OID" | RELATIVE_OID_IRI -> "RELATIVE-OID-IRI"
| SEQUENCE -> "SEQUENCE" | SET -> "SET" | SETTINGS -> "SETTINGS" | SIZE -> "SIZE"
| STRING -> "STRING" | SYNTAX -> "SYNTAX" | T61String -> "T61String"
| TAGS -> "TAGS" | TeletexString -> "TeletexString" | TIME -> "TIME"
| TIME_OF_DAY -> "TIME-OF-DAY" | TRUE -> "TRUE"
| TYPE_IDENTIFIER -> "TYPE-IDENTIFIER" | UNION -> "UNION" | UNIQUE -> "UNIQUE"
| UNIVERSAL -> "UNIVERSAL" | UniversalString -> "UniversalString"
| UTCTime -> "UTCTime" | UTF8String -> "UTF8String"
| VideotexString -> "VideotexString" | VisibleString -> "VisibleString"
| WITH -> "WITH"

let keywords : kwd list =
  [IMPORTS; IMPLICIT; IMPLIED; IDENTIFIER; IA5String; INSTRUCTIONS;
   INSTANCE; INCLUDES; INTERSECTION; INTEGER; ISO646String; END;
   ENCODING_CONTROL; ENCODED; ENUMERATED; EMBEDDED; EXPLICIT; EXPORTS;
   EXCEPT; EXTENSIBILITY; EXTERNAL; CONSTRAINED; CONTAINING; COMPONENT;
   COMPONENTS; CHARACTER; CHOICE; CLASS; ALL; ABSTRACT_SYNTAX; ABSENT;
   APPLICATION; AUTOMATIC; BMPString; BIT; BEGIN; BOOLEAN; BY; DEFINITIONS;
   DEFAULT; DATE; DATE_TIME; DURATION; GraphicString; GeneralizedTime;
   GeneralString; FALSE; FROM; PRIVATE; PRESENT; PDV; PATTERN; PLUS_INFINITY;
   PrintableString; NULL; NOT_A_NUMBER; NumericString; MAX; MIN;
   MINUS_INFINITY; OF; OBJECT; OCTET; OPTIONAL; OID_IRI; ObjectDescriptor;
   TIME; TIME_OF_DAY; T61String; TAGS; TYPE_IDENTIFIER; TRUE; TeletexString;
   RELATIVE_OID; RELATIVE_OID_IRI; REAL; SET; SETTINGS; SEQUENCE; STRING;
   SIZE; SYNTAX; UTCTime; UTF8String; UNIQUE; UNION; UNIVERSAL;
   UniversalString; VideotexString; VisibleString; WITH]

let kwd_dict : kwd Dict.Opt.t =
  let apply dict kwd = Dict.Std.add (string_of_kwd kwd) kwd dict
  in Dict.Std.freeze (List.fold_left apply Dict.Std.empty keywords)

(** {1 Hits} *)

let hits = ["BIT"; "SYNTAX"; "NumericString"; "WITH"; "IMPORTS"]

let check_hit dict s =
  let s' = Dict.Opt.find s dict in assert (s = string_of_kwd s')

let () = List.iter (check_hit kwd_dict) hits

(** {1 Misses} *)

let misses = ["bit"; ""; "WITHOUT"; "IF"]

let check_miss dict s =
  match Dict.Opt.find_opt s dict with
    None -> ()
  | Some _ -> assert false

let () =  List.iter (check_miss kwd_dict) misses
