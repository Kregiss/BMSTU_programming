#include <iostream>
#include <sstream>
#include <string>

#include <gcc-plugin.h>
#include <plugin-version.h>

#include <coretypes.h>

#include <tree-pass.h>
#include <context.h>
#include <basic-block.h>

#include <tree.h>
#include <tree-ssa-alias.h>
#include <gimple-expr.h>
#include <gimple.h>
#include <gimple-ssa.h>
#include <tree-phinodes.h>
#include <tree-ssa-operands.h>

#include <ssa-iterators.h>
#include <gimple-iterator.h>

// Этот плагин удовлетворяет GPL-лицензии
int plugin_is_GPL_compatible = 1;

void print_operator_name(enum tree_code code) {
    switch (code) {
        case PLUS_EXPR:       std::cout << "+";   break;
        case MINUS_EXPR:      std::cout << "-";   break;
        case MULT_EXPR:       std::cout << "*";   break;
        case RDIV_EXPR:       std::cout << "/";   break;
        case BIT_IOR_EXPR:    std::cout << "|";   break;
        case BIT_NOT_EXPR:    std::cout << "!";   break;
        case TRUTH_AND_EXPR:  std::cout << "&&";  break;
        case TRUTH_OR_EXPR:   std::cout << "||";  break;
        case BIT_XOR_EXPR:    std::cout << "^";   break;
        case TRUTH_NOT_EXPR:  std::cout << "!";   break;
        case LT_EXPR:         std::cout << "<";   break;
        case LE_EXPR:         std::cout << "<=";  break;
        case GT_EXPR:         std::cout << ">";   break;
        case GE_EXPR:         std::cout << ">=";  break;
        case EQ_EXPR:         std::cout << "==";  break;
        case NE_EXPR:         std::cout << "!=";  break;
        default:
            std::cout << "UNKNOWN_OP(" << code << ")";
            break;
    }
}

void print_tree_node(tree node) {
    switch (TREE_CODE(node)) {
        case INTEGER_CST:
            std::cout << "INTEGER_CST: " << TREE_INT_CST_LOW(node);
            break;
        case STRING_CST:
            std::cout << "STRING_CST: " << TREE_STRING_POINTER(node);
            break;
        case LABEL_DECL:
            // Метка (label)
            std::cout << (DECL_NAME(node) ? IDENTIFIER_POINTER(DECL_NAME(node)) : "LABEL_DECL") << ":";
            break;
        case VAR_DECL:
            // Объявление переменной
            std::cout << (DECL_NAME(node) ? IDENTIFIER_POINTER(DECL_NAME(node)) : "VAR_DECL");
            break;
        case CONST_DECL:
            // Объявление константы
            std::cout << (DECL_NAME(node) ? IDENTIFIER_POINTER(DECL_NAME(node)) : "CONST_DECL");
            break;
        case ARRAY_REF:
            // array[index]
            std::cout << "ARRAY_REF ";
            print_tree_node(TREE_OPERAND(node, 0)); // массив
            std::cout << "[";
            print_tree_node(TREE_OPERAND(node, 1)); // индекс
            std::cout << "]";
            break;
        case MEM_REF:
            // *ptr
            std::cout << "MEM_REF ";
            std::cout << "((typeof(";
            print_tree_node(TREE_OPERAND(node, 1)); // тип
            std::cout << "))";
            print_tree_node(TREE_OPERAND(node, 0)); // адрес
            std::cout << ")";
            break;
        case SSA_NAME: {
            gimple* definition = SSA_NAME_DEF_STMT(node);
            if (gimple_code(definition) == GIMPLE_PHI) {
                std::cout << "("
                          << (SSA_NAME_IDENTIFIER(node) ? IDENTIFIER_POINTER(SSA_NAME_IDENTIFIER(node)) : "SSA_NAME")
                          << "__v" << SSA_NAME_VERSION(node);
                std::cout << " = GIMPLE_PHI(";
                unsigned num_args = gimple_phi_num_args(definition);
                for (unsigned i = 0; i < num_args; ++i) {
                    print_tree_node(gimple_phi_arg(definition, i)->def);
                    if (i != num_args - 1) {
                        std::cout << ", ";
                    }
                }
                std::cout << "))";
            } else {
                // Просто SSA-переменная
                std::cout << (SSA_NAME_IDENTIFIER(node) ? IDENTIFIER_POINTER(SSA_NAME_IDENTIFIER(node)) : "SSA_NAME")
                          << "__v" << SSA_NAME_VERSION(node);
            }
            break;
        }
        default:
            std::cout << "UNKNOWN_TREE_CODE(" << TREE_CODE(node) << ")";
            break;
    }
}

void print_basic_block_info(basic_block block) {
    std::cout << "\t" << "bb:\n";

    std::cout << "\t\tbefore: { ";
    std::stringstream predecessors_stream;
    edge e;
    edge_iterator ei;
    FOR_EACH_EDGE(e, ei, block->preds) {
        predecessors_stream << e->src->index << ", ";
    }
    std::string predecessors_str = predecessors_stream.str();
    if (!predecessors_str.empty()) {
        std::cout << predecessors_str.substr(0, predecessors_str.size() - 2);
    }
    std::cout << " }" << std::endl;

    std::cout << "\t\tcurrent: { " << block->index << " }\n";

    std::cout << "\t\tafter: { ";
    std::stringstream successors_stream;
    FOR_EACH_EDGE(e, ei, block->succs) {
        successors_stream << e->dest->index << ", ";
    }
    std::string successors_str = successors_stream.str();
    if (!successors_str.empty()) {
        std::cout << successors_str.substr(0, successors_str.size() - 2);
    }
    std::cout << " }" << std::endl;
}

// -----------------------------------------------------------------------------
// GIMPLE_ASSIGN: присваивание
void handle_gimple_assign(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_ASSIGN: " << " { ";
    unsigned num_ops = gimple_num_ops(stmt);
    switch (num_ops) {
        case 2:
            print_tree_node(gimple_assign_lhs(stmt));
            std::cout << " = ";
            print_tree_node(gimple_assign_rhs1(stmt));
            break;
        case 3:
            print_tree_node(gimple_assign_lhs(stmt));
            std::cout << " = ";
            print_tree_node(gimple_assign_rhs1(stmt));
            std::cout << " ";
            print_operator_name(gimple_assign_rhs_code(stmt));
            std::cout << " ";
            print_tree_node(gimple_assign_rhs2(stmt));
            break;
        default:
            std::cout << "UNKNOWN_NUM_OPS(" << num_ops << ")";
            break;
    }
    std::cout << " }" << std::endl;
}

// GIMPLE_CALL: вызов функции
void handle_gimple_call(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_CALL: " << " { ";
    tree lhs = gimple_call_lhs(stmt);
    if (lhs) {
        print_tree_node(lhs);
        std::cout << " = ";
    }

    std::cout << fndecl_name(gimple_call_fndecl(stmt)) << "(";
    unsigned num_args = gimple_call_num_args(stmt);
    for (unsigned i = 0; i < num_args; ++i) {
        print_tree_node(gimple_call_arg(stmt, i));
        if (i != num_args - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
    std::cout << " }" << std::endl;
}

// GIMPLE_COND: условиный переход
void handle_gimple_cond(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_COND: " << " { ";
    print_tree_node(gimple_cond_lhs(stmt));
    std::cout << " ";
    print_operator_name(gimple_cond_code(stmt));
    std::cout << " ";
    print_tree_node(gimple_cond_rhs(stmt));
    std::cout << " }" << std::endl;
}

// GIMPLE_LABEL: метка
void handle_gimple_label(gimple* /*stmt*/) {
    std::cout << "\t\t" << "GIMPLE_LABEL: " << " {";
    std::cout << "}" << std::endl;
}

// GIMPLE_RETURN: return
void handle_gimple_return(gimple* /*stmt*/) {
    std::cout << "\t\t" << "GIMPLE_RETURN: " << " {";
    std::cout << "}" << std::endl;
}

// -----------------------------------------------------------------------------
void print_block_statements(basic_block block) {
    std::cout << "\t" << "statements:\n";
    for (gimple_stmt_iterator gsi = gsi_start_bb(block);
         !gsi_end_p(gsi);
         gsi_next(&gsi)) {
        
        gimple* stmt = gsi_stmt(gsi);
        enum gimple_code code = gimple_code(stmt);

        switch (code) {
            case GIMPLE_ASSIGN: handle_gimple_assign(stmt); break;
            case GIMPLE_CALL:   handle_gimple_call(stmt);   break;
            case GIMPLE_COND:   handle_gimple_cond(stmt);   break;
            case GIMPLE_LABEL:  handle_gimple_label(stmt);  break;
            case GIMPLE_RETURN: handle_gimple_return(stmt); break;
            default: break;
        }
    }
}

int inspect_function_gimple(function* fun) {
    std::cout << "\nfunc " << function_name(fun) << ":" << std::endl;
    
    basic_block block;
    FOR_EACH_BB_FN(block, fun) {
        print_basic_block_info(block);
        print_block_statements(block);
    }
    return 0;
}

// -----------------------------------------------------------------------------
struct GimpleInspectorPass : gimple_opt_pass {
    GimpleInspectorPass(gcc::context* ctx)
        : gimple_opt_pass(
            { GIMPLE_PASS,
              "lab1" },
            ctx) {}

    // Метод execute вызывается GCC для каждой функции
    virtual unsigned int execute(function* fun) override {
        return inspect_function_gimple(fun);
    }
};

int plugin_init(struct plugin_name_args*   args,
                struct plugin_gcc_version* version) {
    if (!plugin_default_version_check(version, &gcc_version)) {
        return 1;
    }

    GimpleInspectorPass* my_pass = new GimpleInspectorPass(g);

    struct register_pass_info pass_info = {
        my_pass,                     // указатель на реализацию pass
        "ssa",                       // имя, к какому пассу будеи цепляться
        1,                           // сколько раз запускать при встрече
        PASS_POS_INSERT_AFTER        // позиция вставки
    };

    // Регистрируем callback, который добавит проход в менеджер проходов
    register_callback(args->base_name, // имя разделяемой библиотеки плагина без расширения .so
                      PLUGIN_PASS_MANAGER_SETUP, // псевдособытие
                      NULL,
                      &pass_info);
    return 0;
}
