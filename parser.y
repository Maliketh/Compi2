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
%token ERR_UNCLOSED_STR
%token ERR_GENERAL
%token COMMENT


// Operator precedence and associativity
%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left RELOP_EQ RELOP_NEQ RELOP_LE RELOP_GE RELOP_LEQ RELOP_GEQ
%left BINOP_ADD BINOP_SUB
%left BINOP_MUL BINOP_DIV
%right NOT
%left LPAREN RPAREN
%left LBRACE RBRACE

%%

// Grammar Rules

Program:
    Funcs { program = std::dynamic_pointer_cast<ast::Funcs>($1); }
;

Funcs:
    /* empty */
    {
        $$ = std::make_shared<ast::Funcs>();
    }
    | FuncDecl Funcs
    {
        $$ = std::dynamic_pointer_cast<ast::Funcs>($2);
        auto funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($$);
        if (funcs_ptr) {
            funcs_ptr->push_front(std::dynamic_pointer_cast<ast::FuncDecl>($1));
        }
    }
;

FuncDecl:
    RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
        auto arg2 = std::dynamic_pointer_cast<ast::Type>($1);
        auto arg3 = std::dynamic_pointer_cast<ast::Formals>($4);
        auto arg4 = std::dynamic_pointer_cast<ast::Statements>($7);
        $$ = std::make_shared<ast::FuncDecl>(arg1, arg2, arg3, arg4);
    } |
    VOID ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
            auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
            auto arg2 = std::make_shared<ast::Type>(ast::BuiltInType::VOID);
            auto arg3 = std::dynamic_pointer_cast<ast::Formals>($4);
            auto arg4 = std::dynamic_pointer_cast<ast::Statements>($7);
            $$ = std::make_shared<ast::FuncDecl>(arg1, arg2, arg3, arg4);
    }

;

RetType:
    Type
    {
        $$ = std::dynamic_pointer_cast<ast::Type>($1);
    }
;

Formals:
    /* epsilon */ { $$ = std::make_shared<ast::Formals>(); }
    | FormalsList
    {
        $$ = std::dynamic_pointer_cast<ast::Formals>($1);
    }
;

FormalsList:
      FormalDecl {
          $$ = std::make_shared<ast::Formals>(std::dynamic_pointer_cast<ast::Formal>($1));
      }
    | FormalDecl COMMA FormalsList {
            $$ = std::dynamic_pointer_cast<ast::Formals>($3);
          auto pointer = std::dynamic_pointer_cast<ast::Formal>($1);
          std::dynamic_pointer_cast<ast::Formals>($$)->push_front(pointer);
      }
    ;


FormalDecl:
    Type ID
    {
        auto pointer1 = std::dynamic_pointer_cast<ast::Type>($1);
        auto pointer2 = std::dynamic_pointer_cast<ast::ID>($2);
        $$ = std::make_shared<ast::Formal>(pointer2, pointer1);
    }
    ;


Statements:
      Statement { $$ = std::make_shared<ast::Statements>(std::dynamic_pointer_cast<ast::Statement>($1)); }
    | Statements Statement
    {
        $$ = std::dynamic_pointer_cast<ast::Statements>($1);
        auto statements_ptr = std::dynamic_pointer_cast<ast::Statements>($$);
        if (statements_ptr) {
            statements_ptr->push_back(std::dynamic_pointer_cast<ast::Statement>($2));
        }
    }
;

Statement:
    LBRACE Statements RBRACE { $$ = std::dynamic_pointer_cast<ast::Statements>($2); }
    | Type ID SC
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
        auto arg2 = std::dynamic_pointer_cast<ast::Type>($1);
        $$ = std::make_shared<ast::VarDecl>(arg1, arg2);
    }
    | Type ID ASSIGN Exp SC
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($2);
        auto arg2 = std::dynamic_pointer_cast<ast::Type>($1);
        auto arg3 = std::dynamic_pointer_cast<ast::Exp>($4);
        $$ = std::make_shared<ast::VarDecl>(arg1, arg2, arg3);
    }
    | ID ASSIGN Exp SC
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::Assign>(arg1, arg2);
    }
    | Call SC { $$ = std::dynamic_pointer_cast<ast::Call>($1); }
    | RETURN SC { $$ = std::make_shared<ast::Return>(); }
    | RETURN Exp SC { $$ = std::make_shared<ast::Return>(std::dynamic_pointer_cast<ast::Exp>($2)); }
    | IF LPAREN Exp RPAREN Statement %prec IF
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($3);
        auto arg2 = std::dynamic_pointer_cast<ast::Statement>($5);
        $$ = std::make_shared<ast::If>(arg1, arg2);
    }
    | IF LPAREN Exp RPAREN Statement ELSE Statement
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($3);
        auto arg2 = std::dynamic_pointer_cast<ast::Statement>($5);
        auto arg3 = std::dynamic_pointer_cast<ast::Statement>($7);
        $$ = std::make_shared<ast::If>(arg1, arg2, arg3);
    }
    | WHILE LPAREN Exp RPAREN Statement
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($3);
        auto arg2 = std::dynamic_pointer_cast<ast::Statement>($5);
        $$ = std::make_shared<ast::While>(arg1, arg2);
    }
    | BREAK SC { $$ = std::make_shared<ast::Break>(); }
    | CONTINUE SC { $$ = std::make_shared<ast::Continue>(); }
;

Call:
    ID LPAREN ExpList RPAREN
    {
        auto arg1 = std::dynamic_pointer_cast<ast::ID>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::ExpList>($3);
        $$ = std::make_shared<ast::Call>(arg1, arg2);
    }
    | ID LPAREN RPAREN { $$ = std::make_shared<ast::Call>(std::dynamic_pointer_cast<ast::ID>($1)); }
;

ExpList:
    Exp { $$ = std::make_shared<ast::ExpList>(std::dynamic_pointer_cast<ast::Exp>($1)); }
    | Exp COMMA ExpList
    {
        auto explist_ptr = std::dynamic_pointer_cast<ast::ExpList>($3);
        explist_ptr->push_front(std::dynamic_pointer_cast<ast::Exp>($1));
        $$ = explist_ptr;
    }
;

Type:
      INT { $$ = std::make_shared<ast::Type>(ast::BuiltInType::INT); }
    | BYTE { $$ = std::make_shared<ast::Type>(ast::BuiltInType::BYTE); }
    | BOOL { $$ = std::make_shared<ast::Type>(ast::BuiltInType::BOOL); }
;


Exp_cast :
    LPAREN Type RPAREN Exp {$$ = std::make_shared<ast::Cast>(std::dynamic_pointer_cast<ast::Exp>($2), std::dynamic_pointer_cast<ast::Type>($1));}
    ;

Exp_t :
    LPAREN Exp RPAREN { $$ = $2; } |
    Exp AND Exp
    {
            auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
            auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
            $$ = std::make_shared<ast::And>(arg1, arg2);
    }  |
    Exp OR Exp
    {
            auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
            auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
            $$ = std::make_shared<ast::Or>(arg1, arg2);
    }  |
    Exp BINOP_ADD Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::ADD);
    }  |
    Exp BINOP_MUL Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::MUL);
    }  |
    Exp BINOP_SUB Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::SUB);
    }  |
    Exp BINOP_DIV Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::BinOp>(arg1, arg2, ast::BinOpType::DIV);
    }  |
    ID { $$ = $1;} |
    Call { $$ = std::dynamic_pointer_cast<ast::Call>($1); } |
    NUM { $$ = $1; } |
    NUM_B {$$ = $1;} |
    STRING {$$ = $1;} |
    TRUE {$$ = std::make_shared<ast::Bool>(1);} |
    FALSE {$$ = std::make_shared<ast::Bool>(0);} |
    NOT Exp {$$ = std::make_shared<ast::Not>(std::dynamic_pointer_cast<ast::Exp>($2));}|
    Exp RELOP_EQ Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::EQ);
    } |
    Exp RELOP_NEQ Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::NE);
    } |
    Exp RELOP_LE Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::LT);
    } |
    Exp RELOP_GE Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::GT);
    } |
    Exp RELOP_LEQ Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::LE);
    } |
    Exp RELOP_GEQ Exp
    {
        auto arg1 = std::dynamic_pointer_cast<ast::Exp>($1);
        auto arg2 = std::dynamic_pointer_cast<ast::Exp>($3);
        $$ = std::make_shared<ast::RelOp>(arg1, arg2, ast::RelOpType::GE);
    }
;
Exp : Exp_cast | Exp_t ;



%%


void yyerror(const char * message) {
    output::errorSyn(yylineno);
    exit(0);
}



// TODO:�Place�any�ad