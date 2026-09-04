#include "llvm-c/Core.h"
#include "llvm/Analysis/ModuleSummaryAnalysis.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Module.h"
#include "llvm/LTO/legacy/ThinLTOCodeGenerator.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

extern "C" LLVMMemoryBufferRef k1_thinlto_bitcode(LLVMModuleRef m) {
  llvm::Module &M = *llvm::unwrap(m);
  llvm::ProfileSummaryInfo PSI(M);
  llvm::ModuleSummaryIndex Index = llvm::buildModuleSummaryIndex(M, nullptr, &PSI);
  llvm::SmallVector<char, 0> buf;
  llvm::raw_svector_ostream OS(buf);
  llvm::WriteBitcodeToFile(M, OS, false, &Index, true);
  return llvm::wrap(
      llvm::MemoryBuffer::getMemBufferCopy(llvm::StringRef(buf.data(), buf.size())).release());
}

struct K1ThinLtoUnit {
  const char *data;
  size_t len;
};

extern "C" int k1_thinlto_codegen(const K1ThinLtoUnit *units, size_t unit_count, const char *cpu,
                                  const char *features, int pic, const char *const *preserved,
                                  size_t preserved_count, const char *const *cross_referenced,
                                  size_t cross_referenced_count, const char *cache_dir,
                                  void (*emit)(void *, size_t, const char *, size_t), void *ctx) {
  llvm::ThinLTOCodeGenerator cg;
  std::vector<std::string> ids;
  ids.reserve(unit_count);
  for (size_t i = 0; i < unit_count; i++) {
    ids.push_back("unit" + std::to_string(i));
    cg.addModule(ids.back(), llvm::StringRef(units[i].data, units[i].len));
  }
  for (size_t i = 0; i < preserved_count; i++)
    cg.preserveSymbol(preserved[i]);
  for (size_t i = 0; i < cross_referenced_count; i++)
    cg.crossReferenceSymbol(cross_referenced[i]);
  cg.setCpu(cpu);
  cg.setAttr(features);
  cg.setCodePICModel(pic ? llvm::Reloc::PIC_ : llvm::Reloc::Static);
  cg.setOptLevel(3);
  cg.setCodeGenOptLevel(llvm::CodeGenOptLevel::Aggressive);
  if (cache_dir && *cache_dir)
    cg.setCacheDir(cache_dir);
  cg.run();
  auto &objects = cg.getProducedBinaries();
  if (objects.size() != unit_count)
    return 1;
  for (size_t i = 0; i < unit_count; i++)
    emit(ctx, i, objects[i]->getBufferStart(), objects[i]->getBufferSize());
  return 0;
}
