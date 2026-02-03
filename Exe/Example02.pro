

%-----
% last allways on top 
cmp(":-",[cmp("if_and_if",[
  var("$Cs"),var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("Outvar3")]),
cmp(",",[cmp("and_conditional_probability_consistency_conditional_probability_consistency",[
  var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("$Cs"),var("Outvar1")]),
cmp("if_smaller_than_plus",[var("$Cs"),var("$Bs"),var("$BCs"),var("$ABs"),var("Outvar2")]),
cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])

subclause imp check : cmp(":-",[
  cmp("and_conditional_probability_consistency_conditional_probability_consistency",[
    var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("$Cs"),var("Outvar3")]),
  cmp(",",[cmp("conditional_probability_consistency",[
    var("$ABs"),var("$Bs"),var("$As"),var("Outvar1")]),
  cmp("conditional_probability_consistency",[var("$BCs"),var("$Cs"),var("$Bs"),var("Outvar2")]),
  cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])


try find ximp 2 3 smaller_than
subclause imp check : cmp(":-",[cmp("if_smaller_than_plus",[
  var("$Cs"),var("$Bs"),var("$BCs"),var("$ABs"),var("Outvar3")]),
cmp(",",[cmp("smaller_than",[
  var("$Bs"),var("realvar#0.99"),var("Outvar1")]),
cmp("plus_multiplication_division",[
  var("$BCs"),var("$ABs"),var("$Bs"),var("$Cs"),var("Outvar2")]),
cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 4 4 multiplication
subclause imp check : cmp(":-",[cmp("plus_multiplication_division",[
  var("$BCs"),var("$ABs"),var("$Bs"),var("$Cs"),var("Outvar3")]),
cmp(",",[cmp("multiplication",[
  var("$BCs"),var("$ABs"),var("Outvar1")]),
cmp("division_multiplication_minus",[
  var("$Bs"),var("$ABs"),var("$Cs"),var("$BCs"),var("Outvar2")]),
cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 6 5 minus
subclause imp check : cmp(":-",[cmp("division_multiplication_minus",[
  var("$Bs"),var("$ABs"),var("$Cs"),var("$BCs"),var("Outvar3")]),
cmp(",",[cmp("multiplication_minus_minus",[var("$ABs"),var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar1")]),
  cmp("minus",[var("$Bs"),var("realvar#1"),var("Outvar2")]),
  cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 7 6 minus
subclause imp check : cmp(":-",[cmp("multiplication_minus_minus",[
  var("$ABs"),var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar3")]),
cmp(",",[cmp("minus",[var("$ABs"),var("realvar#1"),var("Outvar1")]),
  cmp("minus_multiplication",[var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),
  cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 9 7 multiplication
subclause imp check : cmp(":-",[cmp("minus_multiplication",[
  var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),
cmp(",",[cmp("multiplication",[
  var("$BCs"),var("$Bs"),var("Outvar1")]),cmp("=",[var("Outvar2"),cmp("+",[var("Outvar1")])])])])
try find ximp 9 7 multiplication
subclause imp check : cmp(":-",[cmp("minus_multiplication",[
  var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),
cmp(",",[cmp("multiplication",[var("$BCs"),var("$Bs"),var("Outvar1")]),
  cmp("=",[var("Outvar2"),cmp("+",[var("Outvar1")])])])])




%----

% wat mogelijk is / zou zijn , voor de getallen die zijn gebruikt, de getal variabelen , is :
% misschien:: je haalt ze eigenlijk overal weg , behalve laat je ze staan  in de clause waar ze voorkomen

subclause imp check : cmp(":-",[cmp("and_conditional_probability_consistency_conditional_probability_consistency",[var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("$Cs"),var("Outvar3")]),cmp(",",[cmp("conditional_probability_consistency",[var("$ABs"),var("$Bs"),var("$As"),var("Outvar1")]),cmp("conditional_probability_consistency",[var("$BCs"),var("$Cs"),var("$Bs"),var("Outvar2")]),cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 2 3 smaller_than
subclause imp check : cmp(":-",[

  cmp("if_smaller_than_plus",[var("$Cs"),var("$Bs"),var("realvar#0.99"),var("$BCs"),var("$ABs"),var("realvar#1"),var("Outvar3")]),
  cmp(",",[cmp("smaller_than",[var("$Bs"),var("realvar#0.99"),var("Outvar1")]),
    cmp("plus_multiplication_division",[
      var("$BCs"),var("$ABs"),var("$Bs"),var("realvar#1"),var("$Cs"),var("Outvar2")]),cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 4 4 multiplication
subclause imp check : cmp(":-",[cmp("plus_multiplication_division",[
  var("$BCs"),var("$ABs"),var("$Bs"),var("realvar#1"),var("$Cs"),var("Outvar3")]),cmp(",",[cmp("multiplication",[var("$BCs"),var("$ABs"),var("Outvar1")]),cmp("division_multiplication_minus",[var("$Bs"),var("realvar#1"),var("$ABs"),var("$Cs"),var("$BCs"),var("Outvar2")]),cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 6 5 minus
subclause imp check : cmp(":-",[
  cmp("division_multiplication_minus",[
    var("$Bs"),var("realvar#1"),var("$ABs"),var("$Cs"),var("$BCs"),var("Outvar3")]),
  cmp(",",[cmp("multiplication_minus_minus",[
    var("$ABs"),var("realvar#1"),var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar1")]),
  cmp("minus",[var("$Bs"),var("realvar#1"),var("Outvar2")]),
  cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 7 6 minus
subclause imp check : cmp(":-",[cmp("multiplication_minus_minus",[
  var("$ABs"),var("realvar#1"),var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar3")]),cmp(",",[
  cmp("minus",[var("$ABs"),var("realvar#1"),var("Outvar1")]),
  cmp("minus_multiplication",[var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 9 7 multiplication
subclause imp check : cmp(":-",[cmp("minus_multiplication",[var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),
   cmp(",",[cmp("multiplication",[
     var("$BCs"),var("$Bs"),var("Outvar1")]),
   cmp("=",[var("Outvar2"),cmp("+",[var("Outvar1")])])])])
try find ximp 9 7 multiplication
subclause imp check : cmp(":-",[cmp("minus_multiplication",[var("$Cs"),var("$BCs"),var("$Bs"),var("Outvar2")]),cmp(",",[
  cmp("multiplication",[var("$BCs"),var("$Bs"),var("Outvar1")]),cmp("=",[var("Outvar2"),cmp("+",[var("Outvar1")])])])])


2de test:

cmp(":-",[cmp("nesmathc_erer_match",[
  var("$jh"),var("$er"),var("$state"),var("atomvar#self"),var("atomvar#&"),var("$ui"),var("$weed"),var("$fd"),var("$quad"),var("$column"),var("$row"),var("Outvar3")]),
    cmp(",",[cmp("erer",[var("$er"),var("Outvar1")]),
      cmp("match_lapred_erer_extra",[var("$state"),var("atomvar#self"),var("atomvar#&"),var("$ui"),var("$weed"),
           var("$er"),var("$fd"),var("$quad"),var("$column"),var("$row"),var("Outvar2")]),
      cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])
try find ximp 2 3 lapred
try find ximp 2 3 erer
subclause imp check : cmp(":-",[cmp("match_lapred_erer_extra",[
  var("$state"),var("atomvar#self"),var("atomvar#&"),var("$ui"),var("$weed"),var("$er"),var("$fd"),var("$quad"),var("$column"),var("$row"),var("Outvar4")]),
cmp(",",[cmp("lapred",[var("$ui"),var("$weed"),var("Outvar1")]),
  cmp("erer",[var("$er"),var("Outvar2")]),
  cmp("extra_sudoku_puzzle_state",[var("$fd"),var("$state"),var("$quad"),var("$column"),var("$row"),var("Outvar3")]),
  cmp("=",[var("Outvar4"),cmp("+",[var("Outvar1"),var("Outvar2"),var("Outvar3")])])])])


try find ximp 5 4 sudoku_puzzle_state
subclause imp check : cmp(":-",[
  cmp("extra_sudoku_puzzle_state",[var("$fd"),var("$state"),var("$quad"),var("$column"),var("$row"),var("Outvar2")]),
  cmp(",",[cmp("sudoku_puzzle_state",[
    var("$state"),var("$quad"),var("$column"),var("$row"),var("Outvar1")]),
  cmp("=",[var("Outvar2"),cmp("+",[var("Outvar1")])])])])






RESULT construc clause  first version


cmp(":-",[cmp("if_and_if",[
  var("realvar#0"),var("$Cs"),var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("realvar#0.99"),var("realvar#1"),var("Outvar3")]),
cmp(",",[
  cmp("and_conditional_probability_consistency_conditional_probability_consistency",[
    var("$ABs"),var("$Bs"),var("$As"),var("$BCs"),var("$Cs"),var("Outvar1")]),
  cmp("if_smaller_than_plus",[var("$Cs"),var("$Bs"),var("realvar#0.99"),var("$BCs"),var("$ABs"),var("realvar#1"),var("Outvar2")]),
  cmp("=",[var("Outvar3"),cmp("+",[var("Outvar1"),var("Outvar2")])])])])


uicici cmp(":-",
  [
  cmp("func3",[var("X"),var("W"),var("Q")]),
   cmp(",",
    [ cmp("funcname",[var("X"),var("W"),var("H")]),
     cmp("=",[var("Q"),cmp("+",[var("H"),int(2)])])
      ])
  ]
  )

 cmp("=",[
  cmp("sub",[cmp("simple_deduction_strength_formula",
   [var("$As"),var("$Bs"),var("$Cs"),var("$ABs"),var("$BCs")])]),

   cmp("sub",[cmp("if",[cmp("sub",[cmp("and",[cmp("sub",[cmp("conditional_probability_consistency",[var("$As"),var("$Bs"),var("$ABs")])]),cmp("sub",[cmp("conditional_probability_consistency",[var("$Bs"),var("$Cs"),var("$BCs")])])])]),cmp("sub",[cmp("if",[cmp("sub",[cmp("smaller_than",[real(0.99),var("$Bs")])]),var("$Cs"),cmp("sub",[cmp("plus",[cmp("sub",[cmp("multiplication",[var("$ABs"),var("$BCs")])]),cmp("sub",[cmp("division",[cmp("sub",[cmp("multiplication",[cmp("sub",[cmp("minus",[real(1),var("$ABs")])]),cmp("sub",[cmp("minus",[var("$Cs"),cmp("sub",[cmp("multiplication",[var("$Bs"),var("$BCs")])])])])])]),cmp("sub",[cmp("minus",[real(1),var("$Bs")])])])])])])])]),real(0)])])])

(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) 
 (if  
   (and   
     (conditional_probability_consistency $As $Bs $ABs)  
      (conditional_probability_consistency $Bs $Cs $BCs))   
  (if (smaller_than 0.99 $Bs)        $Cs    
    (plus (multiplication $ABs $BCs) (division (multiplication 
 (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  
   0)) 



cmp("equal",

  [
    cmp("sub",[
       cmp("simple_deduction_strength_formula",
         [var("$As"),var("$Bs"),var("$Cs"),var("$ABs"),var("$BCs")]
         )
           ]),

     cmp("sub",
      [ cmp("if",[
         cmp("sub",[
           cmp("and",[
             cmp("sub",[
               cmp("conditional_probability_consistency",
                [var("$As"),var("$Bs"),var("$ABs")])
               ]),
             cmp("sub",[
               cmp("conditional_probability_consistency",[
                 var("$Bs"),var("$Cs"),var("$BCs")])
               ])

             ])

           ]),

         cmp("sub",[
          cmp("if",[
             cmp("sub",[
               cmp("smaller_than",[real(0.99),var("$Bs")])
               ]),
             var("$Cs"),cmp("sub",[cmp("plus",[cmp("sub",[cmp("multiplication",
              [var("$ABs"),var("$BCs")])]),cmp("sub",[cmp("division",[
              cmp("sub",[cmp("multiplication",[cmp("sub",[cmp("minus",[real(1),var("$ABs")])]),
                cmp("sub",[cmp("minus",[var("$Cs"),cmp("sub",[cmp("multiplication",[var("$Bs"),var("$BCs")])])])])])]),
              cmp("sub",[cmp("minus",[real(1),var("$Bs")])])])])])])])]),real(0)])])

     ])





cmp(":-",[

   cmp("simple_deduction_strength_formula",[var("$As"),var("$Bs"),var("$Cs"),var("$ABs"),var("$BCs")]),


   cmp("sub",[
    cmp("if",[cmp("sub_trp2",[cmp("and",[cmp("sub_transp0",[cmp("conditional_probability_consistency",[var("$As"),var("$Bs"),var("$ABs")])]),cmp("sub_transp0",[cmp("conditional_probability_consistency",[var("$Bs"),var("$Cs"),var("$BCs")])])])]),cmp("sub_trp2",[cmp("if",[cmp("sub_transp0",[cmp("smaller_than",[real(0.99),var("$Bs")])]),var("$Cs"),cmp("sub_trp2",[cmp("plus",[cmp("sub_transp0",[cmp("multiplication",[var("$ABs"),var("$BCs")])]),cmp("sub_trp2",[cmp("division",[cmp("sub_trp2",[cmp("multiplication",[cmp("sub_transp0",[cmp("minus",[real(1),var("$ABs")])]),cmp("sub_trp2",[cmp("minus",[var("$Cs"),cmp("sub_transp0",[cmp("multiplication",[var("$Bs"),var("$BCs")])])])])])]),cmp("sub_transp0",[cmp("minus",[real(1),var("$Bs")])])])])])])])]),real(0)])
    ])
   ])