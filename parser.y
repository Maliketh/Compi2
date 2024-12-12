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
%token ERR_UNCLOSED_STR;
%token ERR_GENERAL;
%token COMMENT;

// Operator precedence and associativity
%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left RELOP
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
    FuncDecl {
        $$ = std::make_shared<ast::Funcs>();
        auto funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($$);
        if (funcs_ptr) {
            funcs_ptr->push_back($1);
        }
    }
    | Funcs FuncDecl {
        $$ = $1;
        auto funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($$);
        if (funcs_ptr) {
            funcs_ptr->push_back($2);
        }
    }
;

FuncDecl:
    Type ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
        $$ = std::make_shared<ast::FuncDecl>($2, $1, $4, $6);
    }
;

Formals:
    /* epsilon */ { $$ = std::make_shared<ast::Formals>(); }
    | Formal { $$ = std::make_shared<ast::Formals>();
              auto formals_ptr = std::dynamic_pointer_cast<ast::Formals>($$);
              if (formals_ptr) {
                  formals_ptr->push_back($1);
              }
            }
    | Formals COMMA Formal {
              $$ = $1;
              auto formals_ptr = std::dynamic_pointer_cast<ast::Formals>($$);
              if (formals_ptr) {
                  formals_ptr->push_back($3);
              }
            }
;

Formal:
    Type ID { $$ = std::make_shared<ast::Formal>($1, $2); }
;

Statements:
    /* epsilon */ { $$ = std::make_shared<ast::Statements>(); }
    | Statements Statement {
              $$ = $1;
              auto statements_ptr = std::dynamic_pointer_cast<ast::Statements>($$);
              if (statements_ptr) {
                  statements_ptr->push_back($2);
              }
            }

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
ExpList:
    Exp {
        $$ = std::make_shared<ast::ExpList>();
        $$->push_back($1);
    }
    | Exp COMMA ExpList {
        $$ = std::dynamic_pointer_cast<ast::ExpList>($3);
        $$->push_front($1);
    };
;
Type :
    INT  {$$ = std::make_shared<ast::Type>(3); } |
    BYTE {$$ = std::make_shared<ast::Type>(2); } |
    BOOL {$$ = std::make_shared<ast::Type>(1); }
;
Exp :
    LPAREN Exp RPAREN { $$ = std::make_shared<ast::Exp>($2); } |
    Exp BINOP_ADD Exp { $$ = std::make_shared<ast::BinOp>($1, $3, convert_binop($2)); }  |
    Exp BINOP_MUL Exp { $$ = std::make_shared<ast::BinOp>($1, $3, convert_binop($2)); }  |
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
    output::errorSyn(yylineno);
    exit(0);
}

int main() {
    return yyparse();
}


// TODO: Place any additional code here