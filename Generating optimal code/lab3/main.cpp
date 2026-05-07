/*
Целые числа: десятичные.
Переменные: автоматически объявляются при первом присваивании (тип i32)
Выражения: +, -, *, /, сравнения (<, >, <=, >=, ==, !=).
Операторы:
    var = expr;
    if (expr) { ... } else { ... } (ветка else опциональна)
    for (init; cond; step) { ... }
    return expr;
Комментарии: поддерживаются
*/

#include <iostream>
#include <string>
#include <cctype>
#include <memory>
#include <map>
#include <vector>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>

using namespace llvm;

enum Token {
    tok_eof = -1,
    tok_identifier = -2,
    tok_number = -3,
    tok_assign = -4,        // '='
    tok_plus = -5,          // '+'
    tok_minus = -6,         // '-'
    tok_mul = -7,           // '*'
    tok_div = -8,           // '/'
    tok_eq = -9,            // '=='
    tok_ne = -10,           // '!='
    tok_lt = -11,           // '<'
    tok_le = -12,           // '<='
    tok_gt = -13,           // '>'
    tok_ge = -14,           // '>='
    tok_lbrace = -15,       // '{'
    tok_rbrace = -16,       // '}'
    tok_lparen = -17,       // '('
    tok_rparen = -18,       // ')'
    tok_semicolon = -19,    // ';'
    tok_if = -20,
    tok_else = -21,
    tok_for = -22,
    tok_return = -23
};

static std::string IdentifierStr; // для tok_identifier
static int NumVal;                // для tok_number

static int gettok() {
    static int LastChar = ' ';
    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;
        if (IdentifierStr == "if") return tok_if;
        if (IdentifierStr == "else") return tok_else;
        if (IdentifierStr == "for") return tok_for;
        if (IdentifierStr == "return") return tok_return;
        return tok_identifier;
    }

    if (isdigit(LastChar)) {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar));
        NumVal = std::stoi(NumStr);
        return tok_number;
    }

    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF)
            return gettok();
    }

    if (LastChar == EOF)
        return tok_eof;

    int ThisChar = LastChar;
    LastChar = getchar();
    if (ThisChar == '=' && LastChar == '=') {
        LastChar = getchar();
        return tok_eq;
    }
    if (ThisChar == '!' && LastChar == '=') {
        LastChar = getchar();
        return tok_ne;
    }
    if (ThisChar == '<' && LastChar == '=') {
        LastChar = getchar();
        return tok_le;
    }
    if (ThisChar == '>' && LastChar == '=') {
        LastChar = getchar();
        return tok_ge;
    }

    if (ThisChar == '=') return tok_assign;
    if (ThisChar == '+') return tok_plus;
    if (ThisChar == '-') return tok_minus;
    if (ThisChar == '*') return tok_mul;
    if (ThisChar == '/') return tok_div;
    if (ThisChar == '<') return tok_lt;
    if (ThisChar == '>') return tok_gt;
    if (ThisChar == '{') return tok_lbrace;
    if (ThisChar == '}') return tok_rbrace;
    if (ThisChar == '(') return tok_lparen;
    if (ThisChar == ')') return tok_rparen;
    if (ThisChar == ';') return tok_semicolon;

    return ThisChar;
}

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues; // таблица символов (переменные)
static Function *MainFunction;

static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;
};

class NumberExprAST : public ExprAST {
    int Val;
public:
    NumberExprAST(int Val) : Val(Val) {}
    Value *codegen() override {
        return ConstantInt::get(*TheContext, APInt(32, Val, true));
    }
};

class VariableExprAST : public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value *codegen() override {
        AllocaInst *A = NamedValues[Name];
        if (!A)
            return nullptr;
        return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
    }
    const std::string &getName() const { return Name; }
};

class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
        : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value *codegen() override {
        Value *L = LHS->codegen();
        Value *R = RHS->codegen();
        if (!L || !R) return nullptr;
        switch (Op) {
        case tok_plus: return Builder->CreateAdd(L, R, "addtmp");
        case tok_minus: return Builder->CreateSub(L, R, "subtmp");
        case tok_mul: return Builder->CreateMul(L, R, "multmp");
        case tok_div: return Builder->CreateSDiv(L, R, "divtmp");
        case tok_lt: return Builder->CreateICmpSLT(L, R, "cmptmp");
        case tok_gt: return Builder->CreateICmpSGT(L, R, "cmptmp");
        case tok_le: return Builder->CreateICmpSLE(L, R, "cmptmp");
        case tok_ge: return Builder->CreateICmpSGE(L, R, "cmptmp");
        case tok_eq: return Builder->CreateICmpEQ(L, R, "cmptmp");
        case tok_ne: return Builder->CreateICmpNE(L, R, "cmptmp");
        default: return nullptr;
        }
    }
};

class AssignmentExprAST : public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> RHS;
public:
    AssignmentExprAST(const std::string &Name, std::unique_ptr<ExprAST> RHS)
        : Name(Name), RHS(std::move(RHS)) {}
    Value *codegen() override {
        Value *Val = RHS->codegen();
        if (!Val) return nullptr;
        AllocaInst *Alloca = NamedValues[Name];
        if (!Alloca) {
            // Создаём alloca в точке входа функции main
            IRBuilder<> TmpBuilder(&MainFunction->getEntryBlock(), MainFunction->getEntryBlock().begin());
            Alloca = TmpBuilder.CreateAlloca(Type::getInt32Ty(*TheContext), nullptr, Name);
            NamedValues[Name] = Alloca;
        }
        Builder->CreateStore(Val, Alloca);
        return Val;
    }
};

class IfStmtAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::vector<std::unique_ptr<ExprAST>> ThenBody, ElseBody;
public:
    IfStmtAST(std::unique_ptr<ExprAST> Cond,
              std::vector<std::unique_ptr<ExprAST>> ThenBody,
              std::vector<std::unique_ptr<ExprAST>> ElseBody)
        : Cond(std::move(Cond)), ThenBody(std::move(ThenBody)), ElseBody(std::move(ElseBody)) {}
    Value *codegen() override {
        Value *CondV = Cond->codegen();
        if (!CondV) return nullptr;

        Function *TheFunction = Builder->GetInsertBlock()->getParent();
        BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
        BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
        BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

        Builder->CreateCondBr(CondV, ThenBB, ElseBB);

        // Then блок
        Builder->SetInsertPoint(ThenBB);
        for (auto &stmt : ThenBody)
            if (!stmt->codegen()) return nullptr;
        Builder->CreateBr(MergeBB);

        // Else блок
        TheFunction->getBasicBlockList().push_back(ElseBB);
        Builder->SetInsertPoint(ElseBB);
        for (auto &stmt : ElseBody)
            if (!stmt->codegen()) return nullptr;
        Builder->CreateBr(MergeBB);

        // Merge блок
        TheFunction->getBasicBlockList().push_back(MergeBB);
        Builder->SetInsertPoint(MergeBB);
        return ConstantInt::get(*TheContext, APInt(32, 0, true));
    }
};

class ForStmtAST : public ExprAST {
    std::unique_ptr<ExprAST> Init;
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Step;
    std::vector<std::unique_ptr<ExprAST>> Body;
public:
    ForStmtAST(std::unique_ptr<ExprAST> Init, std::unique_ptr<ExprAST> Cond,
               std::unique_ptr<ExprAST> Step,
               std::vector<std::unique_ptr<ExprAST>> Body)
        : Init(std::move(Init)), Cond(std::move(Cond)), Step(std::move(Step)),
          Body(std::move(Body)) {}
    Value *codegen() override {
        Function *TheFunction = Builder->GetInsertBlock()->getParent();
        BasicBlock *LoopBB  = BasicBlock::Create(*TheContext, "loop", TheFunction);
        BasicBlock *BodyBB  = BasicBlock::Create(*TheContext, "forbody", TheFunction);
        BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

        if (Init)
            Init->codegen();

        Builder->CreateBr(LoopBB);

        Builder->SetInsertPoint(LoopBB);
        if (!Cond) return nullptr;
        Value *CondV = Cond->codegen();
        if (!CondV) return nullptr;
        Builder->CreateCondBr(CondV, BodyBB, AfterBB);

        Builder->SetInsertPoint(BodyBB);
        for (auto &stmt : Body)
            if (!stmt->codegen()) return nullptr;
        if (Step)
            Step->codegen();
        Builder->CreateBr(LoopBB);

        Builder->SetInsertPoint(AfterBB);

        return ConstantInt::get(*TheContext, APInt(32, 0, true));
    }
};

class ReturnStmtAST : public ExprAST {
    std::unique_ptr<ExprAST> RetVal;
public:
    ReturnStmtAST(std::unique_ptr<ExprAST> RetVal) : RetVal(std::move(RetVal)) {}
    Value *codegen() override {
        Value *Val = RetVal->codegen();
        if (!Val) return nullptr;
        Builder->CreateRet(Val);
        return Val;
    }
};

std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParseStatement();

static std::unique_ptr<ExprAST> ParsePrimary() {
    if (CurTok == tok_identifier) {
        std::string Id = IdentifierStr;
        getNextToken();
        if (CurTok == tok_assign) {
            getNextToken();
            auto RHS = ParseExpression();
            return std::make_unique<AssignmentExprAST>(Id, std::move(RHS));
        }
        return std::make_unique<VariableExprAST>(Id);
    }
    if (CurTok == tok_number) {
        auto Result = std::make_unique<NumberExprAST>(NumVal);
        getNextToken();
        return Result;
    }
    if (CurTok == tok_lparen) { // (
        getNextToken();
        auto E = ParseExpression();
        if (CurTok != tok_rparen) return nullptr; // )
        getNextToken();
        return E;
    }
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = -1;
        if (CurTok == tok_lt || CurTok == tok_gt || CurTok == tok_le || CurTok == tok_ge ||
            CurTok == tok_eq || CurTok == tok_ne)
            TokPrec = 10;
        else if (CurTok == tok_plus || CurTok == tok_minus)
            TokPrec = 20;
        else if (CurTok == tok_mul || CurTok == tok_div)
            TokPrec = 40;

        if (TokPrec < ExprPrec)
            return LHS;

        int BinOp = CurTok;
        getNextToken();
        auto RHS = ParsePrimary();
        if (!RHS) return nullptr;

        int NextPrec = -1;
        if (CurTok == tok_lt || CurTok == tok_gt || CurTok == tok_le || CurTok == tok_ge ||
            CurTok == tok_eq || CurTok == tok_ne)
            NextPrec = 10;
        else if (CurTok == tok_plus || CurTok == tok_minus)
            NextPrec = 20;
        else if (CurTok == tok_mul || CurTok == tok_div)
            NextPrec = 40;

        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) return nullptr;
        }
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST> ParseStatement() {
    if (CurTok == tok_if) {
        getNextToken();
        if (CurTok != tok_lparen) return nullptr; // (
        getNextToken();
        auto Cond = ParseExpression();
        if (!Cond) return nullptr;
        if (CurTok != tok_rparen) return nullptr; // )
        getNextToken();
        if (CurTok != tok_lbrace) return nullptr;  // {
        getNextToken();
        std::vector<std::unique_ptr<ExprAST>> ThenBody;
        while (CurTok != tok_rbrace) { // }
            if (auto S = ParseStatement())
                ThenBody.push_back(std::move(S));
            else return nullptr;
        }
        getNextToken();
        std::vector<std::unique_ptr<ExprAST>> ElseBody;
        if (CurTok == tok_else) {
            getNextToken();
            if (CurTok != tok_lbrace) return nullptr;
            getNextToken();
            while (CurTok != tok_rbrace) {
                if (auto S = ParseStatement())
                    ElseBody.push_back(std::move(S));
                else return nullptr;
            }
            getNextToken();
        }
        return std::make_unique<IfStmtAST>(std::move(Cond), std::move(ThenBody), std::move(ElseBody));
    }
    if (CurTok == tok_for) {
        getNextToken();
        if (CurTok != tok_lparen) return nullptr;
        getNextToken();
        std::unique_ptr<ExprAST> Init;
        if (CurTok != tok_semicolon) {
            Init = ParseExpression();
            if (!Init) return nullptr;
        }
        if (CurTok != tok_semicolon) {
            return nullptr;
        }
        getNextToken();
        std::unique_ptr<ExprAST> Cond;
        if (CurTok != tok_semicolon) {
            Cond = ParseExpression();
            if (!Cond) return nullptr;
        }
        if (CurTok != tok_semicolon) {
            return nullptr;
        }
        getNextToken();
        std::unique_ptr<ExprAST> Step;
        if (CurTok != tok_rparen) {
            Step = ParseExpression();
            if (!Step) return nullptr;
        }
        if (CurTok != tok_rparen) return nullptr;
        getNextToken();
        if (CurTok != tok_lbrace) return nullptr;
        getNextToken();
        std::vector<std::unique_ptr<ExprAST>> Body;
        while (CurTok != tok_rbrace) {
            if (auto S = ParseStatement())
                Body.push_back(std::move(S));
            else return nullptr;
        }
        getNextToken();
        return std::make_unique<ForStmtAST>(std::move(Init), std::move(Cond), std::move(Step), std::move(Body));
    }
    if (CurTok == tok_return) {
        getNextToken();
        auto Expr = ParseExpression();
        if (!Expr) return nullptr;
        if (CurTok != tok_semicolon) return nullptr;
        getNextToken();
        return std::make_unique<ReturnStmtAST>(std::move(Expr));
    }
    // Обычный statement: expression ;
    auto Expr = ParseExpression();
    if (!Expr) return nullptr;
    if (CurTok != tok_semicolon) return nullptr;
    getNextToken();
    return Expr;
}

static void ParseProgram() {
    if (CurTok != tok_lbrace) {
        std::cerr << "Ожидалась '{' в начале программы\n";
        return;
    }
    getNextToken();
    while (CurTok != tok_rbrace) {
        if (auto S = ParseStatement())
            S->codegen();
        else {
            std::cerr << "Ошибка разбора\n";
            return;
        }
    }
    getNextToken();
    // Если не было return, добавляем return 0
    if (MainFunction->back().getTerminator() == nullptr) {
        Builder->SetInsertPoint(&MainFunction->back());
        Builder->CreateRet(ConstantInt::get(*TheContext, APInt(32, 0, true)));
    }
}

int main() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("lab3_module", *TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    FunctionType *FT = FunctionType::get(Type::getInt32Ty(*TheContext), false);
    MainFunction = Function::Create(FT, Function::ExternalLinkage, "main", TheModule.get());
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", MainFunction);
    Builder->SetInsertPoint(BB);

    getNextToken();
    ParseProgram();

    TheModule->print(outs(), nullptr);
    return 0;
}
