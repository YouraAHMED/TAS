module ConcreteAnalysis =
  Interpreter.Interprete(Concrete_domain.Concrete)

module ConstantAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Constant_domain.Constants))

module IntervalAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
      (Interval_domain.IntervalDomain))

module ParityIntervalAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Reduced_product_domain.ParityInterval))


let doit filename =
  let prog = File_parser.parse_file filename in
  Abstract_syntax_printer.print_prog Format.std_formatter prog

let eval_prog prog =
  Abstract_syntax_printer.print_prog Format.std_formatter prog

let main () =
  let action = ref eval_prog in
  let files = ref [] in
  Arg.parse
    ["-trace",
     Arg.Set Interpreter.trace,
     "Show the analyzer state after each statement";

     "-nonreldebug",
     Arg.Set Non_relational_domain.non_relational_debug,
     "Turns on debugging information for the non relational lifter";

     "-concrete",
     Arg.Unit (fun () -> action := ConcreteAnalysis.eval_prog),
     "Use the concrete domain";

     "-constant",
     Arg.Unit (fun () -> action := ConstantAnalysis.eval_prog),
     "Use the constant abstract domain";

     "-interval",
     Arg.Unit (fun () -> action := IntervalAnalysis.eval_prog),
     "Use the interval abstract domain";
    
     "-parity-interval",
      Arg.Unit (fun () -> action := ParityIntervalAnalysis.eval_prog),
      "Use reduced product of parity and interval domains";

     "-delay",  Arg.Int (fun n -> Interpreter.delay := n),  "Delay widening by n iterations";
     "-unroll", Arg.Int (fun m -> Interpreter.unroll := m), "Number of times to unroll loops";

     "-int32", Arg.Set Int32_semantics.enabled, "Use 32-bit signed machine integers (wrap-around)";


    ]
    (fun filename -> files := (!files)@[filename])
    "Usage: ./analyzer [options] source_files";

  List.iter
    (fun filename ->
      let prog = File_parser.parse_file filename in
      !action prog
    )
    !files

let _ = main ()