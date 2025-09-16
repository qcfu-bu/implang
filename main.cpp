#include "codegen/codegen.h"
#include "language/language.h"
#include "parsing/parsing.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>
#include <iostream>
#include <sstream>

enum OptLevel {
  O0 = 0,
  O1,
  O2,
  O3,
};

int main(int argc, char *argv[]) {
  using namespace llvm;
  cl::OptionCategory options("implang options");
  cl::opt<std::string> input("i", cl::desc("Input file"),
                             cl::value_desc("filename"), cl::cat(options));
  cl::opt<std::string> output("o", cl::desc("Output filename"),
                              cl::value_desc("filename"), cl::cat(options),
                              cl::init("output.o"));
  cl::opt<OptLevel> opt_level(
      cl::desc("Optimization level"),
      cl::values(clEnumVal(O0, "No optimization"),
                 clEnumVal(O1, "Less optimization"),
                 clEnumVal(O2, "Default optimization"),
                 clEnumVal(O3, "Aggressive optimization")),
      cl::init(O2), cl::cat(options));

  cl::HideUnrelatedOptions(options);
  cl::ParseCommandLineOptions(argc, argv);

  // read input file
  const char *filename = input.c_str();
  std::ifstream file(filename);
  if (!file.is_open()) {
    std::cout << "Failed to open file: " << filename << std::endl;
    exit(1);
  }
  std::stringstream buffer;
  buffer << file.rdbuf();

  implang::lexer::Lexer lexer(buffer.str());
  implang::parser::Parser parser(lexer);

  std::cout << "Parsing." << std::endl;
  auto prog = parser.parse_Prog();
  std::cout << implang::to_string(prog) << std::endl;
  std::cout << "-----------------------------------------" << std::endl;

  std::cout << "Inference." << std::endl;
  implang::infer_type(prog);
  std::cout << implang::to_string(prog) << std::endl;
  std::cout << "-----------------------------------------" << std::endl;

  std::cout << "Codegen." << std::endl;
  implang::codegen::CodeGen codegen("my_module", opt_level, prog);
  codegen.emit(output);
  std::cout << "-----------------------------------------" << std::endl;

  return 0;
}