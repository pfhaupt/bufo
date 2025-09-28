
#include "llvm-c/ExternC.h"
#include "llvm-c/Types.h"
#include "llvm-c/DebugInfo.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Constant.h"

#include "llvm/Support/Casting.h"

#include "llvm/ADT/StringRef.h"

using namespace llvm;

template <typename DIT> DIT *unwrapDI(LLVMMetadataRef Ref) {
  return (DIT *)(Ref ? unwrap<MDNode>(Ref) : nullptr);
}
 
static DINode::DIFlags map_from_llvmDIFlags(LLVMDIFlags Flags) {
  return static_cast<DINode::DIFlags>(Flags);
}
 
static LLVMDIFlags map_to_llvmDIFlags(DINode::DIFlags Flags) {
  return static_cast<LLVMDIFlags>(Flags);
}

LLVM_C_EXTERN_C_BEGIN

LLVMMetadataRef LLVMDIBuilderCreateVariantPart(LLVMDIBuilderRef Builder, LLVMMetadataRef Scope,
        const char *Name, size_t NameLen, LLVMMetadataRef File, unsigned LineNumber,
        uint64_t SizeInBits, uint32_t AlignInBits, LLVMDIFlags Flags, LLVMMetadataRef Discriminator,
        LLVMMetadataRef *Elements, unsigned NumElements, const char *Unique, size_t UniqueLen) {
    auto elts = unwrap(Builder)->getOrCreateArray({unwrap(Elements), NumElements});
    return wrap(unwrap(Builder)->createVariantPart(
                unwrapDI<DIScope>(Scope), {Name, NameLen},
                unwrapDI<DIFile>(File), LineNumber, SizeInBits, AlignInBits, map_from_llvmDIFlags(Flags),
                unwrapDI<DIDerivedType>(Discriminator), elts, {Unique, UniqueLen}));
}

LLVMMetadataRef LLVMDIBuilderCreateVariantMemberType(LLVMDIBuilderRef Builder, LLVMMetadataRef Scope,
        const char *Name, size_t NameLen, LLVMMetadataRef File, unsigned LineNumber,
        uint64_t SizeInBits, uint32_t AlignInBits, uint64_t OffsetInBits, LLVMValueRef Discriminant,
        LLVMDIFlags Flags, LLVMMetadataRef Ty) {
    ConstantInt *Disc = nullptr;
    if (Discriminant) {
        Disc = unwrap<ConstantInt>(Discriminant);
    }
    return wrap(unwrap(Builder)->createVariantMemberType(unwrapDI<DIScope>(Scope), {Name, NameLen},
                unwrapDI<DIFile>(File), LineNumber, SizeInBits, AlignInBits, OffsetInBits,
                Disc, map_from_llvmDIFlags(Flags), unwrapDI<DIType>(Ty)));
}

LLVM_C_EXTERN_C_END
