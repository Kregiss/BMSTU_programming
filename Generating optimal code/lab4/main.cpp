#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <algorithm>
#include <cctype>
#include <memory>
#include <fstream>
#include <sstream>
#include <cassert>

enum Token {
    tok_eof = -1,
    tok_identifier = -2,
    tok_number = -3,
    tok_assign = -4,
    tok_plus = -5,
    tok_minus = -6,
    tok_mul = -7,
    tok_div = -8,
    tok_eq = -9,
    tok_ne = -10,
    tok_lt = -11,
    tok_le = -12,
    tok_gt = -13,
    tok_ge = -14,
    tok_lbrace = -15,
    tok_rbrace = -16,
    tok_lparen = -17,
    tok_rparen = -18,
    tok_semicolon = -19,
    tok_if = -20,
    tok_else = -21,
    tok_for = -22,
    tok_return = -23
};

static std::string IdentifierStr;
static int NumVal;
static int CurTok;

static int gettok() {
    static int LastChar = ' ';
    while (isspace(LastChar)) LastChar = getchar();
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar()))) IdentifierStr += LastChar;
        if (IdentifierStr == "if") return tok_if;
        if (IdentifierStr == "else") return tok_else;
        if (IdentifierStr == "for") return tok_for;
        if (IdentifierStr == "return") return tok_return;
        return tok_identifier;
    }
    if (isdigit(LastChar)) {
        std::string NumStr;
        do { NumStr += LastChar; LastChar = getchar(); } while (isdigit(LastChar));
        NumVal = std::stoi(NumStr);
        return tok_number;
    }
    if (LastChar == '#') {
        do { LastChar = getchar(); } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF) return gettok();
    }
    if (LastChar == EOF) return tok_eof;
    int ThisChar = LastChar;
    LastChar = getchar();
    if (ThisChar == '=' && LastChar == '=') { LastChar = getchar(); return tok_eq; }
    if (ThisChar == '!' && LastChar == '=') { LastChar = getchar(); return tok_ne; }
    if (ThisChar == '<' && LastChar == '=') { LastChar = getchar(); return tok_le; }
    if (ThisChar == '>' && LastChar == '=') { LastChar = getchar(); return tok_ge; }
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

static int getNextToken() { return CurTok = gettok(); }

struct ExprAST {
    virtual ~ExprAST() = default;
    virtual void print(int indent = 0) const = 0;
};

struct NumberExprAST : ExprAST {
    int Val;
    NumberExprAST(int v) : Val(v) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << Val << "\n";
    }
};

struct VariableExprAST : ExprAST {
    std::string Name;
    VariableExprAST(const std::string &n) : Name(n) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << Name << "\n";
    }
};

struct BinaryExprAST : ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
    BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << Op << "\n";
        LHS->print(indent+2);
        RHS->print(indent+2);
    }
};

struct AssignmentExprAST : ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> RHS;
    AssignmentExprAST(const std::string &n, std::unique_ptr<ExprAST> rhs)
        : Name(n), RHS(std::move(rhs)) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << "= " << Name << "\n";
        RHS->print(indent+2);
    }
};

struct IfStmtAST;
struct ForStmtAST;
struct ReturnStmtAST;

using StmtAST = std::unique_ptr<ExprAST>;

struct BlockAST : ExprAST {
    std::vector<StmtAST> Stmts;
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << "Block\n";
        for (auto &s : Stmts) s->print(indent+2);
    }
};

struct ReturnStmtAST : ExprAST {
    std::unique_ptr<ExprAST> RetVal;
    ReturnStmtAST(std::unique_ptr<ExprAST> val) : RetVal(std::move(val)) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << "return\n";
        RetVal->print(indent+2);
    }
};

struct IfStmtAST : ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<BlockAST> ThenBlock;
    std::unique_ptr<BlockAST> ElseBlock;  // может nullptr
    IfStmtAST(std::unique_ptr<ExprAST> cond,
              std::unique_ptr<BlockAST> thenBlk,
              std::unique_ptr<BlockAST> elseBlk = nullptr)
        : Cond(std::move(cond)), ThenBlock(std::move(thenBlk)), ElseBlock(std::move(elseBlk)) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << "if\n";
        Cond->print(indent+2);
        ThenBlock->print(indent+2);
        if (ElseBlock) {
            std::cout << std::string(indent, ' ') << "else\n";
            ElseBlock->print(indent+2);
        }
    }
};

struct ForStmtAST : ExprAST {
    std::unique_ptr<ExprAST> Init;
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Step;
    std::unique_ptr<BlockAST> Body;
    ForStmtAST(std::unique_ptr<ExprAST> init,
               std::unique_ptr<ExprAST> cond,
               std::unique_ptr<ExprAST> step,
               std::unique_ptr<BlockAST> body)
        : Init(std::move(init)), Cond(std::move(cond)), Step(std::move(step)), Body(std::move(body)) {}
    void print(int indent) const override {
        std::cout << std::string(indent, ' ') << "for\n";
        if (Init) Init->print(indent+2);
        if (Cond) Cond->print(indent+2);
        if (Step) Step->print(indent+2);
        Body->print(indent+2);
    }
};

std::unique_ptr<ExprAST> ParseExpression();

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
    if (CurTok == tok_lparen) {
        getNextToken();
        auto E = ParseExpression();
        if (CurTok != tok_rparen) return nullptr;
        getNextToken();
        return E;
    }
    return nullptr;
}

static int getTokPrecedence() {
    if (CurTok == tok_lt || CurTok == tok_gt || CurTok == tok_le || CurTok == tok_ge ||
        CurTok == tok_eq || CurTok == tok_ne) return 10;
    if (CurTok == tok_plus || CurTok == tok_minus) return 20;
    if (CurTok == tok_mul || CurTok == tok_div) return 40;
    return -1;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = getTokPrecedence();
        if (TokPrec < ExprPrec) return LHS;
        int BinOp = CurTok;
        getNextToken();
        auto RHS = ParsePrimary();
        if (!RHS) return nullptr;
        int NextPrec = getTokPrecedence();
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

std::unique_ptr<ExprAST> ParseStatement();

static std::unique_ptr<BlockAST> ParseBlock() {
    if (CurTok != tok_lbrace) return nullptr;
    getNextToken();
    auto B = std::make_unique<BlockAST>();
    while (CurTok != tok_rbrace) {
        if (auto S = ParseStatement())
            B->Stmts.push_back(std::move(S));
        else
            return nullptr;
    }
    getNextToken();
    return B;
}

std::unique_ptr<ExprAST> ParseStatement() {
    if (CurTok == tok_if) {
        getNextToken();
        if (CurTok != tok_lparen) return nullptr;
        getNextToken();
        auto Cond = ParseExpression();
        if (!Cond) return nullptr;
        if (CurTok != tok_rparen) return nullptr;
        getNextToken();
        auto ThenBlock = ParseBlock();
        if (!ThenBlock) return nullptr;
        std::unique_ptr<BlockAST> ElseBlock = nullptr;
        if (CurTok == tok_else) {
            getNextToken();
            ElseBlock = ParseBlock();
            if (!ElseBlock) return nullptr;
        }
        return std::make_unique<IfStmtAST>(std::move(Cond), std::move(ThenBlock), std::move(ElseBlock));
    }
    if (CurTok == tok_for) {
        getNextToken();
        if (CurTok != tok_lparen) return nullptr;
        getNextToken();
        std::unique_ptr<ExprAST> Init = nullptr;
        if (CurTok != tok_semicolon) {
            Init = ParseExpression();
            if (!Init) return nullptr;
        }
        if (CurTok != tok_semicolon) return nullptr;
        getNextToken();
        std::unique_ptr<ExprAST> Cond = nullptr;
        if (CurTok != tok_semicolon) {
            Cond = ParseExpression();
            if (!Cond) return nullptr;
        }
        if (CurTok != tok_semicolon) return nullptr;
        getNextToken();
        std::unique_ptr<ExprAST> Step = nullptr;
        if (CurTok != tok_rparen) {
            Step = ParseExpression();
            if (!Step) return nullptr;
        }
        if (CurTok != tok_rparen) return nullptr;
        getNextToken();
        auto Body = ParseBlock();
        if (!Body) return nullptr;
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
    // Обычный statement: expression ';'
    auto Expr = ParseExpression();
    if (!Expr) return nullptr;
    if (CurTok != tok_semicolon) return nullptr;
    getNextToken();
    return Expr;
}

// -----------------------------------------------------------------------------
// 4. Генерация трёхадресного кода (TAC) и построение CFG
// -----------------------------------------------------------------------------
struct TAC {
    enum Op {
        ADD, SUB, MUL, DIV,
        LT, GT, LE, GE, EQ, NE,
        ASSIGN,          // x = y
        LOADI,            // x = const
        BR,               // goto label
        BRCOND,           // if cond goto label
        RET,
        PHI
    };
    Op op;
    std::string dest;     // результат (для phi - переменная)
    std::string arg1, arg2;
    std::string label;    // для переходов
    std::vector<std::pair<std::string, std::string>> phi_args; // (value, label)
};

class BasicBlock {
public:
    std::string label;
    std::vector<TAC> instructions;
    std::vector<BasicBlock*> preds;
    std::vector<BasicBlock*> succs;
    // Для SSA
    std::set<std::string> defs;
    std::set<std::string> uses;
    std::vector<TAC> phis; // фи-функции в начале блока

    BasicBlock(const std::string &l) : label(l) {}
    void addInstruction(const TAC &t) { instructions.push_back(t); }
    void addPhi(const TAC &phi) { phis.push_back(phi); }
};

std::vector<std::unique_ptr<BasicBlock>> Blocks;
BasicBlock *EntryBlock = nullptr;
BasicBlock *ExitBlock = nullptr;

BasicBlock* createBlock(const std::string &prefix = "") {
    static int cnt = 0;
    std::string name = prefix + "_" + std::to_string(cnt++);
    Blocks.push_back(std::make_unique<BasicBlock>(name));
    return Blocks.back().get();
}

struct CFGBuilder {
    BasicBlock *currentBlock;
    BasicBlock *exitBlock;
    
    CFGBuilder() {
        exitBlock = createBlock("exit");
        EntryBlock = createBlock("entry");
        currentBlock = EntryBlock;
    }

    // Генерация TAC из AST

    // выражения
    std::string genExpr(ExprAST *E) {
        if (auto *N = dynamic_cast<NumberExprAST*>(E)) {
            std::string tmp = newTemp();
            currentBlock->addInstruction({TAC::LOADI, tmp, std::to_string(N->Val), ""});
            return tmp;
        } else if (auto *V = dynamic_cast<VariableExprAST*>(E)) {
            return V->Name;
        } else if (auto *B = dynamic_cast<BinaryExprAST*>(E)) {
            std::string L = genExpr(B->LHS.get());
            std::string R = genExpr(B->RHS.get());
            std::string tmp = newTemp();
            TAC::Op op;
            switch (B->Op) {
                case tok_plus: op = TAC::ADD; break;
                case tok_minus: op = TAC::SUB; break;
                case tok_mul: op = TAC::MUL; break;
                case tok_div: op = TAC::DIV; break;
                case tok_lt: op = TAC::LT; break;
                case tok_gt: op = TAC::GT; break;
                case tok_le: op = TAC::LE; break;
                case tok_ge: op = TAC::GE; break;
                case tok_eq: op = TAC::EQ; break;
                case tok_ne: op = TAC::NE; break;
                default: assert(false);
            }
            currentBlock->addInstruction({op, tmp, L, R});
            return tmp;
        } else if (auto *A = dynamic_cast<AssignmentExprAST*>(E)) {
            std::string R = genExpr(A->RHS.get());
            currentBlock->addInstruction({TAC::ASSIGN, A->Name, R, ""});
            return A->Name;
        }
        return "";
    }

    // операторы
    void genStmt(ExprAST *S) {
        if (auto *A = dynamic_cast<AssignmentExprAST*>(S)) {
            genExpr(A);
        } else if (auto *Ret = dynamic_cast<ReturnStmtAST*>(S)) {
            std::string val = genExpr(Ret->RetVal.get());
            currentBlock->addInstruction({TAC::RET, "", val, ""});
            currentBlock->succs.push_back(exitBlock);
            exitBlock->preds.push_back(currentBlock);
        } else if (auto *If = dynamic_cast<IfStmtAST*>(S)) {
            std::string cond = genExpr(If->Cond.get());

            BasicBlock *thenBlock = createBlock("then");
            BasicBlock *elseBlock = If->ElseBlock ? createBlock("else") : nullptr;
            BasicBlock *mergeBlock = createBlock("if_merge");

            // Переход по условию
            currentBlock->addInstruction({TAC::BRCOND, "", cond, "", thenBlock->label});
            if (elseBlock) {
                currentBlock->addInstruction({TAC::BR, "", "", "", elseBlock->label});
            } else {
                currentBlock->addInstruction({TAC::BR, "", "", "", mergeBlock->label});
            }
            currentBlock->succs.push_back(thenBlock);
            thenBlock->preds.push_back(currentBlock);
            if (elseBlock) {
                currentBlock->succs.push_back(elseBlock);
                elseBlock->preds.push_back(currentBlock);
            }

            // Генерация then-ветки
            currentBlock = thenBlock;
            for (auto &stmt : If->ThenBlock->Stmts) genStmt(stmt.get());
            if (thenBlock->instructions.empty() || thenBlock->instructions.back().op != TAC::BR) {
                thenBlock->addInstruction({TAC::BR, "", "", "", mergeBlock->label});
            }
            thenBlock->succs.push_back(mergeBlock);
            mergeBlock->preds.push_back(thenBlock);

            // Генерация else-ветки (если есть)
            if (elseBlock) {
                currentBlock = elseBlock;
                for (auto &stmt : If->ElseBlock->Stmts) genStmt(stmt.get());
                if (elseBlock->instructions.empty() || elseBlock->instructions.back().op != TAC::BR) {
                    elseBlock->addInstruction({TAC::BR, "", "", "", mergeBlock->label});
                }
                elseBlock->succs.push_back(mergeBlock);
                mergeBlock->preds.push_back(elseBlock);
            }

            currentBlock = mergeBlock;
        } else if (auto *For = dynamic_cast<ForStmtAST*>(S)) {
            BasicBlock *initBlock = createBlock("for_init");
            BasicBlock *condBlock = createBlock("for_cond");
            BasicBlock *bodyBlock = createBlock("for_body");
            BasicBlock *stepBlock = createBlock("for_step");
            BasicBlock *afterBlock = createBlock("for_after");

            // Переход в init
            currentBlock->addInstruction({TAC::BR, "", "", "", initBlock->label});
            currentBlock->succs.push_back(initBlock);
            initBlock->preds.push_back(currentBlock);

            // Блок инициализации
            currentBlock = initBlock;
            if (For->Init) genExpr(For->Init.get());
            initBlock->addInstruction({TAC::BR, "", "", "", condBlock->label});
            initBlock->succs.push_back(condBlock);
            condBlock->preds.push_back(initBlock);

            // Блок проверки условия
            currentBlock = condBlock;
            std::string cond = genExpr(For->Cond.get());
            condBlock->addInstruction({TAC::BRCOND, "", cond, "", bodyBlock->label});
            condBlock->addInstruction({TAC::BR, "", "", "", afterBlock->label});
            condBlock->succs.push_back(bodyBlock);
            condBlock->succs.push_back(afterBlock);
            bodyBlock->preds.push_back(condBlock);
            afterBlock->preds.push_back(condBlock);

            // Блок тела цикла
            currentBlock = bodyBlock;
            for (auto &stmt : For->Body->Stmts) genStmt(stmt.get());
            bodyBlock->addInstruction({TAC::BR, "", "", "", stepBlock->label});
            bodyBlock->succs.push_back(stepBlock);
            stepBlock->preds.push_back(bodyBlock);

            // Блок шага
            currentBlock = stepBlock;
            if (For->Step) genExpr(For->Step.get());
            stepBlock->addInstruction({TAC::BR, "", "", "", condBlock->label});
            stepBlock->succs.push_back(condBlock);
            condBlock->preds.push_back(stepBlock);

            currentBlock = afterBlock;
        } else if (auto *Block = dynamic_cast<BlockAST*>(S)) {
            for (auto &stmt : Block->Stmts) genStmt(stmt.get());
        }
    }

    std::string newTemp() {
        static int tmpCnt = 0;
        return "t" + std::to_string(tmpCnt++);
    }
};

class SSAConstructor {
public:
    std::vector<BasicBlock*> blocks;
    std::map<BasicBlock*, std::set<BasicBlock*>> dom;     // доминаторы
    std::map<BasicBlock*, BasicBlock*> idom;              // непосредственный доминатор
    std::map<BasicBlock*, std::set<BasicBlock*>> children; // дети в дереве доминаторов
    std::map<BasicBlock*, std::set<BasicBlock*>> DF;      // граница доминирования
    std::set<std::string> allVars;                         // все переменные программы

    SSAConstructor(const std::vector<std::unique_ptr<BasicBlock>> &bs) {
        for (auto &b : bs) blocks.push_back(b.get());
    }

    void computeDominators() {
        // Инициализация
        std::set<BasicBlock*> all(blocks.begin(), blocks.end());
        for (auto *B : blocks) {
            if (B == EntryBlock) dom[B] = {EntryBlock};
            else dom[B] = all;
        }
        bool changed;
        do {
            changed = false;
            for (auto *B : blocks) {
                if (B == EntryBlock) continue;
                std::set<BasicBlock*> new_dom = all;
                for (auto *P : B->preds) {
                    std::set<BasicBlock*> tmp;
                    std::set_intersection(new_dom.begin(), new_dom.end(),
                                          dom[P].begin(), dom[P].end(),
                                          std::inserter(tmp, tmp.begin()));
                    new_dom = tmp;
                }
                new_dom.insert(B);
                if (new_dom != dom[B]) {
                    dom[B] = new_dom;
                    changed = true;
                }
            }
        } while (changed);

        // Вычисление idom
        for (auto *B : blocks) {
            if (B == EntryBlock) continue;
            for (auto *D : dom[B]) {
                if (D == B) continue;
                bool is_idom = true;
                for (auto *Other : dom[B]) {
                    if (Other == B || Other == D) continue;
                    if (dom[Other].count(D)) { is_idom = false; break; }
                }
                if (is_idom) { idom[B] = D; break; }
            }
        }
        // Дети
        for (auto *B : blocks) {
            if (idom.find(B) != idom.end())
                children[idom[B]].insert(B);
        }
    }

    void computeDominanceFrontiers() {
        for (auto *B : blocks) {
            // DF_local
            for (auto *S : B->succs) {
                if (idom[S] != B)
                    DF[B].insert(S);
            }
        }
        // DF_up
        for (auto *B : blocks) {
            for (auto *C : children[B]) {
                for (auto *Y : DF[C]) {
                    if (idom[Y] != B)
                        DF[B].insert(Y);
                }
            }
        }
    }

    void insertPhis() {
        for (auto *B : blocks) {
            for (auto &I : B->instructions) {
                if (!I.dest.empty() && I.op != TAC::BR && I.op != TAC::BRCOND && I.op != TAC::RET)
                    B->defs.insert(I.dest);
                if (!I.arg1.empty() && !isdigit(I.arg1[0]) && I.arg1 != "t")
                    B->uses.insert(I.arg1);
                if (!I.arg2.empty() && !isdigit(I.arg2[0]))
                    B->uses.insert(I.arg2);
            }
        }
        // Для каждой переменной находим DF+
        for (auto &var : allVars) {
            std::set<BasicBlock*> S;
            for (auto *B : blocks) if (B->defs.count(var)) S.insert(B);
            std::set<BasicBlock*> DFplus = S;
            bool changed;
            do {
                changed = false;
                std::set<BasicBlock*> newDF;
                for (auto *B : DFplus) {
                    for (auto *X : DF[B]) {
                        if (!DFplus.count(X)) {
                            newDF.insert(X);
                            changed = true;
                        }
                    }
                }
                DFplus.insert(newDF.begin(), newDF.end());
            } while (changed);
            // Вставляем фи-функции в каждый блок из DF+ (кроме исходных определений)
            for (auto *B : DFplus) {
                if (S.count(B)) continue; // не вставляем в блоки с присваиванием
                // Проверяем, есть ли уже фи для этой переменной
                bool hasPhi = false;
                for (auto &phi : B->phis)
                    if (phi.dest == var) { hasPhi = true; break; }
                if (!hasPhi) {
                    TAC phi;
                    phi.op = TAC::PHI;
                    phi.dest = var;
                    B->addPhi(phi);
                }
            }
        }
    }

    void renameVariables() {
        std::map<std::string, std::stack<std::string>> varStack;
        std::map<std::string, int> counter;
        for (auto &v : allVars) { varStack[v].push(v); counter[v] = 0; }
        rename(EntryBlock, varStack, counter);
    }

    void rename(BasicBlock *B, std::map<std::string, std::stack<std::string>> &varStack,
                std::map<std::string, int> &counter) {

        for (auto &phi : B->phis) {
            std::string var = phi.dest;
            std::string newName = var + "_" + std::to_string(counter[var]++);
            phi.dest = newName;
            varStack[var].push(newName);
        }

        for (auto &I : B->instructions) {
            // Замена использований
            if (!I.arg1.empty() && varStack.count(I.arg1))
                I.arg1 = varStack[I.arg1].top();
            if (!I.arg2.empty() && varStack.count(I.arg2))
                I.arg2 = varStack[I.arg2].top();
            // Определение новой версии
            if (!I.dest.empty() && I.op != TAC::BR && I.op != TAC::BRCOND && I.op != TAC::RET) {
                std::string var = I.dest;
                std::string newName = var + "_" + std::to_string(counter[var]++);
                I.dest = newName;
                varStack[var].push(newName);
            }
        }
        // Заполнение фи-функций в преемниках
        for (auto *S : B->succs) {
            int idx = std::find(S->preds.begin(), S->preds.end(), B) - S->preds.begin();
            for (auto &phi : S->phis) {
                std::string var = phi.dest;
                std::string base = var.substr(0, var.find('_'));
                if (varStack.count(base) && !varStack[base].empty())
                    phi.phi_args.push_back({varStack[base].top(), B->label});
            }
        }
        // Рекурсивный обход детей
        for (auto *C : children[B]) {
            rename(C, varStack, counter);
        }
        // Восстановление стеков
        for (auto &I : B->instructions) {
            if (!I.dest.empty() && I.op != TAC::BR && I.op != TAC::BRCOND && I.op != TAC::RET) {
                std::string var = I.dest.substr(0, I.dest.find('_'));
                varStack[var].pop();
            }
        }
        for (auto &phi : B->phis) {
            std::string var = phi.dest.substr(0, phi.dest.find('_'));
            varStack[var].pop();
        }
    }

    void run() {
        computeDominators();
        computeDominanceFrontiers();
        // Сбор всех переменных
        for (auto *B : blocks) {
            for (auto &I : B->instructions) {
                if (!I.dest.empty() && I.op != TAC::BR && I.op != TAC::BRCOND && I.op != TAC::RET)
                    allVars.insert(I.dest);
                if (!I.arg1.empty() && !isdigit(I.arg1[0]) && I.arg1 != "t") allVars.insert(I.arg1);
                if (!I.arg2.empty() && !isdigit(I.arg2[0])) allVars.insert(I.arg2);
            }
        }
        insertPhis();
        renameVariables();
    }
};

void printCFG() {
    std::cout << "digraph CFG {\n";
    for (auto &B : Blocks) {
        std::cout << "  " << B->label << " [shape=box, label=\"";
        std::cout << B->label << ":\\l";
        for (auto &phi : B->phis) {
            std::cout << phi.dest << " = phi(";
            for (size_t i = 0; i < phi.phi_args.size(); ++i) {
                if (i) std::cout << ", ";
                std::cout << phi.phi_args[i].first << ":" << phi.phi_args[i].second;
            }
            std::cout << ")\\l";
        }
        for (auto &I : B->instructions) {
            switch (I.op) {
                case TAC::ADD: std::cout << I.dest << " = " << I.arg1 << " + " << I.arg2; break;
                case TAC::SUB: std::cout << I.dest << " = " << I.arg1 << " - " << I.arg2; break;
                case TAC::MUL: std::cout << I.dest << " = " << I.arg1 << " * " << I.arg2; break;
                case TAC::DIV: std::cout << I.dest << " = " << I.arg1 << " / " << I.arg2; break;
                case TAC::LT:  std::cout << I.dest << " = " << I.arg1 << " < " << I.arg2; break;
                case TAC::GT:  std::cout << I.dest << " = " << I.arg1 << " > " << I.arg2; break;
                case TAC::LE:  std::cout << I.dest << " = " << I.arg1 << " <= " << I.arg2; break;
                case TAC::GE:  std::cout << I.dest << " = " << I.arg1 << " >= " << I.arg2; break;
                case TAC::EQ:  std::cout << I.dest << " = " << I.arg1 << " == " << I.arg2; break;
                case TAC::NE:  std::cout << I.dest << " = " << I.arg1 << " != " << I.arg2; break;
                case TAC::ASSIGN: std::cout << I.dest << " = " << I.arg1; break;
                case TAC::LOADI: std::cout << I.dest << " = " << I.arg1; break;
                case TAC::BR: std::cout << "goto " << I.label; break;
                case TAC::BRCOND: std::cout << "if " << I.arg1 << " goto " << I.label; break;
                case TAC::RET: std::cout << "ret " << I.arg1; break;
                default: break;
            }
            std::cout << "\\l";
        }
        std::cout << "\"];\n";
        for (auto *S : B->succs) {
            std::cout << "  " << B->label << " -> " << S->label << ";\n";
        }
    }
    std::cout << "}\n";
}

int main() {
    getNextToken();
    if (CurTok != tok_lbrace) {
        std::cerr << "Expected '{'\n";
        return 1;
    }
    getNextToken();
    CFGBuilder builder;
    while (CurTok != tok_rbrace) {
        auto stmt = ParseStatement();
        if (!stmt) break;
        builder.genStmt(stmt.get());
    }
    if (builder.currentBlock->instructions.empty() ||
        builder.currentBlock->instructions.back().op != TAC::RET) {
        builder.currentBlock->addInstruction({TAC::BR, "", "", "", builder.exitBlock->label});
        builder.currentBlock->succs.push_back(builder.exitBlock);
        builder.exitBlock->preds.push_back(builder.currentBlock);
    }

    SSAConstructor ssa(Blocks);
    ssa.run();

    printCFG();

    return 0;
}
