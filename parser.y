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
%left RELOP_EQ;
%left RELOP_NEQ;
%left RELOP_LE;
%left RELOP_GE;
%left RELOP_LEQ;
%left RELOP_GEQ;
%left BINOP_ADD
%left BINOP_SUB
%left BINOP_DIV
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
            funcs_ptr->push_back(std::dynamic_pointer_cast<ast::FuncDecl>($1));
        }
    }
    | Funcs FuncDecl {
        $$ = $1;
        auto funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($$);
        if (funcs_ptr) {
            funcs_ptr->push_back(std::dynamic_pointer_cast<ast::FuncDecl>($2));
        }
    }
;

FuncDecl:
    Type ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
        auto arg2 = std::dynamic_pointer_cast<ast::Type>($1);
        auto arg3 = std::dynamic_pointer_cast<ast::Formals>($4);
        auto arg4 = std::dynamic_pointer_cast<ast::Statements>( $6);
        $$ = std::make_shared<ast::FuncDecl>(arg1, arg2, arg3, arg4);
    }
;

Formals:
    /* epsilon */ { $$ = std::make_shared<ast::Formals>(); }
    | Formal { $$ = std::make_shared<ast::Formals>();
              auto formals_ptr = std::dynamic_pointer_cast<ast::Formals>($$);
              if (formals_ptr) {
                  formals_ptr->push_back(std::dynamic_pointer_cast<ast::Formal>($1));
              }
            }
    | Formals COMMA Formal {
              $$ = $1;
              auto formals_ptr = std::dynamic_pointer_cast<ast::Formals>($$);
              if (formals_ptr) {
                  formals_ptr->push_back(std::dynamic_pointer_cast<ast::Formal>($3));
              }
            }
;

Formal:
    Type ID {
            auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
            auto arg2 = std::dynamic_pointer_cast<ast::Type>($1);
            $$ = std::make_shared<ast::Formal>(arg1, arg2); }
;

Statements:
    /* epsilon */ { $$ = std::make_shared<ast::Statements>(); }
    | Statements Statement {
              $$ = $1;
              auto statements_ptr = std::dynamic_pointer_cast<ast::Statements>($$);
              if (statements_ptr) {
                  statements_ptr->push_back(std::dynamic_pointer_cast<ast::Statement>($2));
              }
            }

;
Statement:
      RETURN Exp SC { $$ = std::make_shared<ast::Return>(std::dynamic_pointer_cast<ast::Exp>($2)); }
    | RETURN SC { $$ = std::make_shared<ast::Return>(); }
    | BREAK SC { $$ = std::make_shared<ast::Break>(); }
    | CONTINUE SC { $$ = std::make_shared<ast::Continue>(); }
;
// TODO FOR YOU MRS CHULMAN :)

Call :
    ID LPAREN ExpList RPAREN {
            auto arg1 = std::dynamic_pointer_cast<ast::ID>($1);
            auto arg2 = std::dynamic_pointer_cast<ast::ExpList>($3);
            $$ = std::make_shared<ast::Call>(arg1, arg2);} |
    ID LPAREN RPAREN {  $$ = std::make_shared<ast::Call>(std::dynamic_pointer_cast<ast::ID>($1)); }
;
ExpList:
    Exp { $$ = std::make_shared<ast::ExpList>(std::dynamic_pointer_cast<ast::Exp>($1));}
    | Exp COMMA ExpList {
        auto explist_ptr  = std::dynamic_pointer_cast<ast::ExpList>($3);
        explist_ptr->push_front(std::dynamic_pointer_cast<ast::Exp>($1));
        $$ = explist_ptr;
    };
;
Type :
    VOID  {$$ = std::make_shared<ast::Type>(ast::BuiltInType::VOID); } |
    INT  {$$ = std::make_shared<ast::Type>(ast::BuiltInType::INT); } |
    BYTE {$$ = std::make_shared<ast::Type>(ast::BuiltInType::BYTE); } |
    BOOL {$$ = std::make_shared<ast::Type>(ast::BuiltInType::BOOL); }|
    STRING {$$ = std::make_shared<ast::Type>(ast::BuiltInType::STRING); }
;
Exp :
    LPAREN Exp RPAREN { $$ = $2; } |
    Exp BINOP_ADD Exp {
                      auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                      auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                      $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::ADD);
                      }  |
    Exp BINOP_MUL Exp {
                      auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                      auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                      $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::MUL);
                      }  |
    Exp BINOP_SUB Exp {
                      auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                      auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                      $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::SUB);
                      }  |
    Exp BINOP_DIV Exp {
                      auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                      auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                      $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::DIV);
                      }  |
    ID { $$ = $1;} |
    Call { $$ = $1; } |
    NUM { $$ = std::make_shared<ast::Num>(std::dynamic_pointer_cast<std::string>($1)->c_str()); } |
    NUM_B {$$ = std::make_shared<ast::NumB>(std::dynamic_pointer_cast<std::string>($1)->c_str());} |
    STRING {$$ = std::make_shared<ast::String>(std::dynamic_pointer_cast<std::string>($1)->c_str());} |
    TRUE {$$ = std::make_shared<ast::Bool>(1);} |
    FALSE {$$ = std::make_shared<ast::Bool>(0);} |
    NOT Exp {$$ = std::make_shared<ast::Not>(std::dynamic_pointer_cast<ast::Exp>($2));}|
    Exp RELOP_EQ Exp {
                        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::EQ);
                      } |
    Exp RELOP_NEQ Exp {
                       auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                       auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                       $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::NE);
                       } |
    Exp RELOP_LE Exp {
                          auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                          auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                          $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::LT);
                      } |
    Exp RELOP_GE Exp {auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                      auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                      $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::GT);
                      } |
    Exp RELOP_LEQ Exp {
                          auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                          auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                          $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::LE);
                          } |
    Exp RELOP_GEQ Exp {auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
                       auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
                       $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::GE);
                       } |

    LPAREN Type RPAREN Exp {$$ = std::make_shared<ast::Cast>(std::dynamic_pointer_cast<ast::Exp>($2), std::dynamic_pointer_cast<ast::Type>($1));}

%%


void yyerror(const char * message) {
    output::errorSyn(yylineno);
    exit(0);
}

int main() {
    return yyparse();
}


// TODO: Place any additional code here