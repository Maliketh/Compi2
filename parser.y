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
%token B
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
%left RELOP_EQ
%left RELOP_REL
%left BINOP_ADD
%left BINOP_MUL
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


Statement:
    Expr SC { $$ = std::make_shared<ast::ExprStatement>($1); }
    | RETURN Expr SC { $$ = std::make_shared<ast::Return>($2); }
    | RETURN SC { $$ = std::make_shared<ast::Return>(); }
    | BREAK SC { $$ = std::make_shared<ast::Break>(); }
    | CONTINUE SC { $$ = std::make_shared<ast::Continue>(); }
// TODO FOR YOU MRS CHULMAN :)
%%

// TODO: Place any additional code here