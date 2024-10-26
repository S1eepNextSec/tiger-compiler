%filenames = "scanner"

 /*
  * Please don't modify the lines above.
  */

 /* You can add lex definitions here. */
 /* TODO: Put your lab2 code here */

%x COMMENT STR IGNORE


digit [0-9]
letter [a-zA-Z]

%%

 /*
  * Below is examples, which you can wipe out
  * and write regular expressions and actions of your own.
  *
  * All the tokens:
  *   Parser::ID            variable(sequence of letters digits and _ ,startin with a letter)
  *   Parser::STRING        string  
  *   Parser::INT           int
  *   Parser::COMMA         ,
  *   Parser::COLON         :
  *   Parser::SEMICOLON     ;
  *   Parser::LPAREN        (
  *   Parser::RPAREN        )
  *   Parser::LBRACK        [
  *   Parser::RBRACK        ]
  *   Parser::LBRACE        {
  *   Parser::RBRACE        }
  *   Parser::DOT           .
  *   Parser::PLUS          +
  *   Parser::MINUS         -
  *   Parser::TIMES         *
  *   Parser::DIVIDE        /
  *   Parser::EQ            =
  *   Parser::NEQ           <>
  *   Parser::LT            <
  *   Parser::LE            <=
  *   Parser::GT            > 
  *   Parser::GE            >=
  *   Parser::AND           &
  *   Parser::OR            |
  *   Parser::ASSIGN        :=
  *   Parser::ARRAY         array
  *   Parser::IF            if
  *   Parser::THEN          then
  *   Parser::ELSE          else
  *   Parser::WHILE         while
  *   Parser::FOR           for
  *   Parser::TO            to
  *   Parser::DO            do
  *   Parser::LET           let
  *   Parser::IN            in
  *   Parser::END           end
  *   Parser::OF            of
  *   Parser::BREAK         break
  *   Parser::NIL           nil
  *   Parser::FUNCTION      function
  *   Parser::VAR           var
  *   Parser::TYPE          type
  */

/* reserved words */
/* TODO: Put your lab2 code here */


","     {adjust(); return Parser::COMMA;}
":"     {adjust(); return Parser::COLON;}
";"     {adjust();return Parser::SEMICOLON;}
"("     {adjust();return Parser::LPAREN;}
")"     {adjust();return Parser::RPAREN;}
"["     {adjust();return Parser::LBRACK;}
"]"    {adjust();return Parser::RBRACK;}
"{"     {adjust();return Parser::LBRACE;}
"}"     {adjust();return Parser::RBRACE;}
"."     {adjust();return Parser::DOT;}
"+"     {adjust();return Parser::PLUS;}
"-"     {adjust();return Parser::MINUS;}
"*"     {adjust();return Parser::TIMES;}
"/"     {adjust();return Parser::DIVIDE;}
"="     {adjust();return Parser::EQ;}
"<>"    {adjust();return Parser::NEQ;}
"<"     {adjust(); return Parser::LT;}
"<="    {adjust();return Parser::LE;}
">"     {adjust();return Parser::GT;}
">="    {adjust();return Parser::GE;}
"&"     {adjust();return Parser::AND;}
"|"     {adjust();return Parser::OR;}
":="    {adjust(); return Parser::ASSIGN;}
"array" {adjust(); return Parser::ARRAY;}
"if"    {adjust(); return Parser::IF;}
"then"  {adjust();return Parser::THEN;}
"else"  {adjust();return Parser::ELSE;}
"while" {adjust();return Parser::WHILE;}
"for"   {adjust();return Parser::FOR;}
"to"    {adjust();return Parser::TO;}
"do"    {adjust();return Parser::DO;}
"let"   {adjust();return Parser::LET;}
"in"    {adjust();return Parser::IN;}
"end"   {adjust();return Parser::END;}
"of"    {adjust();return Parser::OF;}
"break" {adjust();return Parser::BREAK;}
"nil"   {adjust();return Parser::NIL;}
"function" {adjust();return Parser::FUNCTION;}
"var"   {adjust();return Parser::VAR;}
"type"  {adjust(); return Parser::TYPE;}
{digit}+ {adjust(); return Parser::INT;}
{letter}({letter}|{digit}|"_")* {adjust();return Parser::ID;}

"/*" {
  adjust();
  comment_level_++;
  begin(StartCondition_::COMMENT);
}
<COMMENT>"/*" {
  adjustStr();
  comment_level_++;
}
<COMMENT>"*/" {
  adjustStr();
  comment_level_--;
  if (comment_level_ == 1){
    begin(StartCondition_::INITIAL);
  }
}
<COMMENT>\n {adjustStr();}
<COMMENT>. {adjustStr();}
<COMMENT><<EOF>> {
  adjustStr();
  errormsg_->Error(errormsg_->tok_pos_, "illegal comment end");
}

\" {
  adjust();
  begin(StartCondition_::STR);
}

<STR>\" {
  adjustStr();
  begin(StartCondition_::INITIAL);
  setMatched(string_buf_);
  string_buf_.clear();
  return Parser::STRING;
}

<STR>\\n {
  adjustStr();
  string_buf_.push_back('\n');
}

<STR>\\t {
  adjustStr();
  string_buf_.push_back('\t');
}

<STR>\\{digit}{3}  {
  adjustStr();
  std::string s = matched();
  int ascii = (s[1]-'0') * 100 + (s[2]-'0') * 10 + (s[3]-'0') * 1;
  char charac = char(ascii);
  string_buf_.push_back(charac);
}

<STR>\\\" {
  adjustStr();
  string_buf_.push_back('\"');
}
<STR>\\\\ {
  adjustStr();
  string_buf_.push_back('\\');
}

<STR>\\[\t\n ]+\\ {
  adjustStr();
}

<STR>\\\^[@A-Z\[\\\]\^_] {
  adjustStr();
  auto s = matched();
  char charac = ((s[2] - '@') + 0);
  string_buf_.push_back(charac);
}

<STR><<EOF>> {
  adjustStr();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string end");
}

<STR>. {
  adjustStr();
  string_buf_ += matched();
}


 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
