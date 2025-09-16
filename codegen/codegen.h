#include "../language/language.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <map>
#include <vector>

namespace implang {
namespace codegen {

class CompileException : public std::exception {
public:
  explicit CompileException(const std::string &msg) : msg(msg) {}
  const char *what() const noexcept override { return msg.c_str(); }

private:
  std::string msg;
};

std::set<Id> get_fvars(std::set<Id> &bound, Expr &expr);
std::set<Id> get_fvars(std::set<Id> &bound, Cmd &cmd);

using namespace llvm;
using SymbolTable = std::vector<std::map<Id, Value *>>;

struct CodeGen {
  LLVMContext context;
  Module module;
  IRBuilder<> builder;
  Function *allocator;
  SymbolTable table;
  BasicBlock *global_bb;

  FunctionAnalysisManager FAM;
  LoopAnalysisManager LAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
  ModulePassManager MPM;
  TargetMachine *TargetMachine;

  CodeGen(const std::string &module_name, int opt_level, Prog &prog);

  void emit(const std::string &output);
};

} // namespace codegen
} // namespace implang