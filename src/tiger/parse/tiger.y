%filenames parser
%scanner tiger/lex/scanner.h
%baseclass-preinclude tiger/absyn/absyn.h

 /*
  * Please don't modify the lines above.
  */

%union {
  int ival;
  std::string* sval;
  sym::Symbol *sym;
  absyn::Exp *exp;
  absyn::ExpList *explist;
  absyn::Var *var;
  absyn::DecList *declist;
  absyn::Dec *dec;
  absyn::EFieldList *efieldlist;
  absyn::EField *efield;
  absyn::NameAndTyList *tydeclist;
  absyn::NameAndTy *tydec;
  absyn::FieldList *fieldlist;
  absyn::Field *field;
  absyn::FunDecList *fundeclist;
  absyn::FunDec *fundec;
  absyn::Ty *ty;
  }

%token <sym> ID
%token <sval> STRING
%token <ival> INT

%token
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
  LBRACE RBRACE DOT
  // PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE AND OR
  ASSIGN
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
  BREAK NIL
  FUNCTION VAR TYPE

 /* token priority */
 /* TODO: Put your lab3 code here */

%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left NEGATIVE

%type <exp> exp
%type <exp> nil_exp var_exp str_exp rec_exp if_exp assign_exp while_exp for_exp
%type <exp> seq_exp array_exp let_exp break_exp void_exp call_exp int_exp
%type <exp> op_exp
%type <explist> seq_in_seq_exp seq_in_let_exp params params_tail seq_tail_int_let_exp
%type <var> lvalue
%type <declist> decs
%type <dec> single_dec fun_dec type_dec var_dec
%type <efieldlist> rec rec_tail
%type <efield> rec_field_val
%type <tydeclist> type_dec_list
%type <tydec> single_type_dec
%type <fieldlist> tyfields tyfields_tail
%type <field> tyfield
%type <ty> ty
%type <fundeclist> fun_dec_list
%type <fundec> single_fun_dec

%start program

%%
program:  exp  {absyn_tree_ = std::make_unique<absyn::AbsynTree>($1);};


 /* TODO: Put your lab3 code here */

lvalue:  ID  {$$ = new absyn::SimpleVar(scanner_.GetTokPos(), $1);} // var_name
  |  lvalue DOT ID {$$ = new absyn::FieldVar(scanner_.GetTokPos(),$1,$3);}  // var1_name.var2_name.field_name array[index].field_name
  |  lvalue LBRACK exp RBRACK {$$ = new absyn::SubscriptVar(scanner_.GetTokPos(),$1,$3);} // array[index]
  |  ID LBRACK exp RBRACK {$$ = new absyn::SubscriptVar(scanner_.GetTokPos(),new absyn::SimpleVar(scanner_.GetTokPos(),$1),$3);}
  ;

var_dec: VAR ID ASSIGN exp {$$ = new absyn::VarDec(scanner_.GetTokPos(),$2,nullptr,$4);} // var var_name := expression
  | VAR ID COLON ID ASSIGN exp {$$ = new absyn::VarDec(scanner_.GetTokPos(),$2,$4,$6);} //  var var_name : class_name := expression
  ;

ty: ID {$$ = new absyn::NameTy(scanner_.GetTokPos(),$1);} // class_name
  | LBRACE tyfields RBRACE {$$ = new absyn::RecordTy(scanner_.GetTokPos(),$2);} //  { field_name : class_name , file_name : class_name ,... } 
  | ARRAY OF ID {$$ = new absyn::ArrayTy(scanner_.GetTokPos(),$3);}  // array of class_name
  ;

tyfield: ID COLON ID {$$ = new absyn::Field(scanner_.GetTokPos(),$1,$3);} // field_name : class_name
  ;

tyfields:  {$$ = new absyn::FieldList();} // nothing at all
  | tyfield tyfields_tail {$$ = $2->Prepend($1);} //  field_name : class_name ...
  | tyfield {$$ = new absyn::FieldList($1);}
  ;

tyfields_tail:  COMMA tyfield { $$ = new absyn::FieldList($2);}
  | COMMA tyfield tyfields_tail {$$ = $3->Prepend($2);}
  ;

single_type_dec: TYPE ID EQ ty {$$ = new absyn::NameAndTy($2,$4);} //  type class_name = class
  ;

type_dec_list:  single_type_dec{$$ = new absyn::NameAndTyList($1);}  // nothing at all
  | single_type_dec type_dec_list {$$ = $2->Prepend($1);}  // many type class_name = class 
  ;

type_dec: type_dec_list {$$ = new absyn::TypeDec(scanner_.GetTokPos(),$1);}
  ;

single_fun_dec: FUNCTION ID LPAREN tyfields RPAREN EQ exp {$$ = new absyn::FunDec(scanner_.GetTokPos(),$2,$4,nullptr,$7);}  // function func_name (param:class,param:class,...) = expression
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {$$ = new absyn::FunDec(scanner_.GetTokPos(),$2,$4,$7,$9);} //function func_name (param:class,...) : return-type = expression
  ;

fun_dec_list:  single_fun_dec {$$ = new absyn::FunDecList($1);}
  | single_fun_dec fun_dec_list {$$ = $2->Prepend($1);} // many function def
  ;

fun_dec: fun_dec_list {$$ = new absyn::FunctionDec(scanner_.GetTokPos(),$1);}
  ;

single_dec: fun_dec {$$ = $1;}
  | type_dec {$$ = $1;}
  | var_dec {$$ = $1;}
  ;

decs: single_dec decs {$$ = $2->Prepend($1);}
  |  single_dec {$$ = new absyn::DecList($1);}
  ;

rec_field_val: ID EQ exp {$$ = new absyn::EField($1,$3);}
  ;

rec_tail: COMMA rec_field_val rec_tail {$$ = $3->Prepend($2);}
  | COMMA rec_field_val {$$ = new absyn::EFieldList($2);}
  ;

rec:  {$$ = new absyn::EFieldList();}
  | rec_field_val rec_tail {$$ = $2->Prepend($1);}
  | rec_field_val {$$ = new absyn::EFieldList($1);}
  ;

nil_exp: NIL {$$ = new absyn::NilExp(scanner_.GetTokPos());}
  ;

var_exp: lvalue {$$ = new absyn::VarExp(scanner_.GetTokPos(),$1);}
  ;

str_exp: STRING {$$ = new absyn::StringExp(scanner_.GetTokPos(),$1);}
  ;

rec_exp: ID LBRACE rec RBRACE {$$ = new absyn::RecordExp(scanner_.GetTokPos(),$1,$3);}
  ;

assign_exp: lvalue ASSIGN exp {$$ = new absyn::AssignExp(scanner_.GetTokPos(),$1,$3);}
  ;

if_exp: IF exp THEN exp ELSE exp {$$ = new absyn::IfExp(scanner_.GetTokPos(),$2,$4,$6);}
  | IF exp THEN exp {$$ = new absyn::IfExp(scanner_.GetTokPos(),$2,$4,nullptr);}
  ;

while_exp: WHILE exp DO exp {$$ = new absyn::WhileExp(scanner_.GetTokPos(),$2,$4);}
  ;

for_exp: FOR ID ASSIGN exp TO exp DO exp {$$ = new absyn::ForExp(scanner_.GetTokPos(),$2,$4,$6,$8);}
  ;

seq_in_seq_exp: SEMICOLON exp seq_in_seq_exp{ $$ = $3->Prepend($2);}
  | SEMICOLON exp { $$ = new absyn::ExpList($2);}
  ;

seq_exp: LPAREN exp seq_in_seq_exp RPAREN {$$ = new absyn::SeqExp(scanner_.GetTokPos(),$3->Prepend($2));}
  ;

params_tail:  COMMA exp params_tail {$$ = $3->Prepend($2);}
  | COMMA exp {$$ = new absyn::ExpList($2);}
  ;

params: {$$ = new absyn::ExpList();}
  | exp {$$ = new absyn::ExpList($1);}
  | exp params_tail {$$ = $2->Prepend($1);}
  ;

call_exp: ID LPAREN params RPAREN {$$ = new absyn::CallExp(scanner_.GetTokPos(),$1,$3);}
  ;

int_exp: INT {$$ = new absyn::IntExp(scanner_.GetTokPos(),$1);}
  | MINUS INT %prec NEGATIVE {$$ = new absyn::IntExp(scanner_.GetTokPos(),0 - $2);}
  ;

void_exp: LPAREN RPAREN{$$ = new absyn::VoidExp(scanner_.GetTokPos());}
  ;

break_exp: BREAK {$$ = new absyn::BreakExp(scanner_.GetTokPos());}
  ;

seq_tail_int_let_exp:   {$$ = new absyn::ExpList();}
  | SEMICOLON exp seq_tail_int_let_exp  {$$ = $3->Prepend($2);}
  ;

seq_in_let_exp:   {$$ = new absyn::ExpList();}
  | exp seq_tail_int_let_exp  {$$ = $2->Prepend($1);}
  ;

let_exp:  LET decs IN seq_in_let_exp END {$$ = new absyn::LetExp(scanner_.GetTokPos(),$2,new absyn::SeqExp(scanner_.GetTokPos(),$4));}
  ;

array_exp:  ID  LBRACK exp RBRACK OF exp {$$ = new absyn::ArrayExp(scanner_.GetTokPos(),$1,$3,$6);}
  ;

op_exp: exp AND exp { $$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::AND_OP,$1,$3);}
  | exp OR exp {  $$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::OR_OP,$1,$3);}
  | exp PLUS exp {  $$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::PLUS_OP,$1,$3);}
  | exp MINUS exp { $$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::MINUS_OP,$1,$3);}
  | exp TIMES exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::TIMES_OP,$1,$3);}
  | exp DIVIDE exp { $$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::DIVIDE_OP,$1,$3);}
  | exp EQ exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::EQ_OP,$1,$3);}
  | exp NEQ exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::NEQ_OP,$1,$3);}
  | exp LT exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::LT_OP,$1,$3);}
  | exp LE exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::LE_OP,$1,$3);}
  | exp GT exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::GT_OP,$1,$3);}
  | exp GE exp {$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::GE_OP,$1,$3);}
  | MINUS exp %prec NEGATIVE{$$ = new absyn::OpExp(scanner_.GetTokPos(),absyn::Oper::MINUS_OP,new absyn::IntExp(scanner_.GetTokPos(),0),$2);}
  ;

exp:  op_exp {$$ = $1;}
  | array_exp {$$ = $1;}
  | let_exp {$$ = $1;}
  | break_exp {$$ = $1;}
  | void_exp {$$ = $1;}
  | int_exp {$$ = $1;}
  | call_exp {$$ = $1;}
  | seq_exp {$$ = $1;}
  | for_exp {$$ = $1;}
  | while_exp {$$ = $1;}
  | if_exp {$$ = $1;}
  | assign_exp {$$ = $1;}
  | rec_exp {$$ = $1;}
  | str_exp {$$ = $1;}
  | var_exp {$$ = $1;}
  | nil_exp {$$ = $1;}
  | LPAREN exp RPAREN {$$ = $2;}
  ;
