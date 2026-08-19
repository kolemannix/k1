#include "lld/Common/Driver.h"
#include "llvm/Support/raw_ostream.h"

LLD_HAS_DRIVER(wasm)

// args[0] selects the flavor ("wasm-ld").
// canRunAgain=false means lld hit a fatal and this process must not link
// again; surface it as failure even when retCode is 0
extern "C" int k1_lld_link(const char *const *args, size_t num_args) {
  llvm::ArrayRef<const char *> arr(args, num_args);
  lld::Result r =
      lld::lldMain(arr, llvm::outs(), llvm::errs(), {{lld::Wasm, &lld::wasm::link}});
  if (!r.canRunAgain && r.retCode == 0)
    return 1;
  return r.retCode;
}
