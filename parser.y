%{
#include "nodes.hpp"
#include "output.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

void yyerror(const char*);

// root of the AST, set by the parser and used by other parts of the compiler
std::shared_ptr<ast::Node> program;

using namespace std;
%}

// Tokens
%token ID
%token NUM
%token STRING
%token VOID
%token INT
%token BYTE
%token NUM_B
%token BOOL
%token TRUE
%token FALSE
%token RETURN
%token WHILE
%token BREAK
%token CONTINUE
%token SC
%token COMMA

// Operator precedence and associativity
%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left RELOP
%left BINOP
%right NOT
%left LPAREN
%left RPAREN
%left LBRACE
%left RBRACE

%%

// Grammar Rules

Program:
    Funcs { program = $1; }
;

Funcs:
    FuncDecl { $$ = std::make_shared<ast::Funcs>(); $$->push_back($1); }
    | Funcs FuncDecl { $$ = $1; $$->push_back($2); }
;

FuncDecl:
    Type ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
        $$ = std::make_shared<ast::FuncDecl>($2, $1, $4, $6);
    }
;

Formals:
    /* empty */ { $$ = std::make_shared<ast::Formals>(); }
    | Formal { $$ = std::make_shared<ast::Formals>(); $$->push_back($1); }
    | Formals COMMA Formal { $$ = $1; $$->push_back($3); }
;

Formal:
    Type ID { $$ = std::make_shared<ast::Formal>($1, $2); }
;

Statements:
    /* empty */ { $$ = std::make_shared<ast::Statements>(); }
    | Statements Statement { $$ = $1; $$->push_back($2); }

;
Statement:
    Exp SC { $$ = std::make_shared<ast::ExprStatement>($1); }
    | RETURN Exp SC { $$ = std::make_shared<ast::Return>($2); }
    | RETURN SC { $$ = std::make_shared<ast::Return>(); }
    | BREAK SC { $$ = std::make_shared<ast::Break>(); }
    | CONTINUE SC { $$ = std::make_shared<ast::Continue>(); }
;
// TODO FOR YOU MRS CHULMAN :)

Call :
    ID LPAREN ExpList RPAREN { $$ = std::make_shared<ast::Call>($1, $3);} |
    ID LPAREN RPAREN {  $$ = std::make_shared<ast::Call>($1); }
;
ExpList :
    Exp { $$ = std::make_shared<ast::ExpList>($1);} |
    Exp COMMA ExpList {$$ = std::dynamic_pointer_cast<ast::ExpList>($3); $$->exps.push_front($1);}
;
Type :
    INT  {$$ = std::make_shared<ast::Type>(3); } |
    BYTE {$$ = std::make_shared<ast::Type>(2); } |
    BOOL {$$ = std::make_shared<ast::Type>(1); }
;
Exp :
    LPAREN Exp RPAREN { $$ = std::make_shared<ast::Exp>($2); } |
    Exp BINOP Exp { $$ = std::make_shared<ast::BinOp>($1, $3, convert_binop($2)); }  |
    ID { $$ = std::make_shared<ast::ID>($1);} |
    Call { $$ = std::dynamic_pointer_cast<ast::Call>($1); } |
    NUM { $$ = std::make_shared<ast::Num>($1); } |
    NUM_B {$$ = std::make_shared<ast::NumB>($1);} |
    STRING {$$ = std::make_shared<ast::String>($1);} |
    TRUE {$$ = std::make_shared<ast::Bool>(1);} |
    FALSE {$$ = std::make_shared<ast::Bool>(0);} |
    NOT Exp {$$ = std::make_shared<ast::Not>($2);}|
    Exp RELOP Exp {$$ = std::make_shared<ast::BinOp>($1, $3, convert_relop($2)); } |
    LPAREN Type RPAREN Exp {$$ = std::make_shared<ast::Cast>($4, $2);}

%%


void yyerror(const char * message) {
    errorSyn(yylineno);
    exit(0);
}

int main() {
    return yyparse();
}


// TODO: Place any additional code here