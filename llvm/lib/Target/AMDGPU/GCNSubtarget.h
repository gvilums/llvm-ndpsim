//=====-- GCNSubtarget.h - Define GCN Subtarget for AMDGPU ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//==-----------------------------------------------------------------------===//
//
/// \file
/// AMD GCN specific subclass of TargetSubtarget.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_AMDGPU_GCNSUBTARGET_H
#define LLVM_LIB_TARGET_AMDGPU_GCNSUBTARGET_H

#include "AMDGPUCallLowering.h"
#include "AMDGPUSubtarget.h"
#include "SIFrameLowering.h"
#include "SIISelLowering.h"
#include "SIInstrInfo.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"

#define GET_SUBTARGETINFO_HEADER
#include "AMDGPUGenSubtargetInfo.inc"

namespace llvm {

class GCNTargetMachine;

class GCNSubtarget final : public AMDGPUGenSubtargetInfo,
                           public AMDGPUSubtarget {

  using AMDGPUSubtarget::getMaxWavesPerEU;

public:
  // Following 2 enums are documented at:
  //   - https://llvm.org/docs/AMDGPUUsage.html#trap-handler-abi
  enum class TrapHandlerAbi {
    NONE   = 0x00,
    AMDHSA = 0x01,
  };

  enum class TrapID {
    LLVMAMDHSATrap      = 0x02,
    LLVMAMDHSADebugTrap = 0x03,
  };

private:
  /// GlobalISel related APIs.
  std::unique_ptr<AMDGPUCallLowering> CallLoweringInfo;
  std::unique_ptr<InlineAsmLowering> InlineAsmLoweringInfo;
  std::unique_ptr<InstructionSelector> InstSelector;
  std::unique_ptr<LegalizerInfo> Legalizer;
  std::unique_ptr<RegisterBankInfo> RegBankInfo;

protected:
  // Basic subtarget description.
  Triple TargetTriple;
  AMDGPU::IsaInfo::AMDGPUTargetID TargetID;
  unsigned Gen = INVALID;
  InstrItineraryData InstrItins;
  int LDSBankCount = 0;
  unsigned MaxPrivateElementSize = 0;

  // Possibly statically set by tablegen, but may want to be overridden.
  bool FastFMAF32 = false;
  bool FastDenormalF32 = false;
  bool HalfRate64Ops = false;
  bool FullRate64Ops = false;

  // Dynamically set bits that enable features.
  bool FlatForGlobal = false;
  bool AutoWaitcntBeforeBarrier = false;
  bool UnalignedScratchAccess = false;
  bool UnalignedAccessMode = false;
  bool HasApertureRegs = false;
  bool SupportsXNACK = false;

  // This should not be used directly. 'TargetID' tracks the dynamic settings
  // for XNACK.
  bool EnableXNACK = false;

  bool EnableTgSplit = false;
  bool EnableCuMode = false;
  bool TrapHandler = false;

  // Used as options.
  bool EnableLoadStoreOpt = false;
  bool EnableUnsafeDSOffsetFolding = false;
  bool EnableSIScheduler = false;
  bool EnableDS128 = false;
  bool EnablePRTStrictNull = false;
  bool DumpCode = false;

  // Subtarget statically properties set by tablegen
  bool FP64 = false;
  bool FMA = false;
  bool MIMG_R128 = false;
  bool CIInsts = false;
  bool GFX8Insts = false;
  bool GFX9Insts = false;
  bool GFX90AInsts = false;
  bool GFX940Insts = false;
  bool GFX10Insts = false;
  bool GFX10_3Insts = false;
  bool GFX7GFX8GFX9Insts = false;
  bool SGPRInitBug = false;
  bool NegativeScratchOffsetBug = false;
  bool NegativeUnalignedScratchOffsetBug = false;
  bool HasSMemRealTime = false;
  bool HasIntClamp = false;
  bool HasFmaMixInsts = false;
  bool HasMovrel = false;
  bool HasVGPRIndexMode = false;
  bool HasScalarStores = false;
  bool HasScalarAtomics = false;
  bool HasSDWAOmod = false;
  bool HasSDWAScalar = false;
  bool HasSDWASdst = false;
  bool HasSDWAMac = false;
  bool HasSDWAOutModsVOPC = false;
  bool HasDPP = false;
  bool HasDPP8 = false;
  bool Has64BitDPP = false;
  bool HasPackedFP32Ops = false;
  bool HasImageInsts = false;
  bool HasExtendedImageInsts = false;
  bool HasR128A16 = false;
  bool HasGFX10A16 = false;
  bool HasG16 = false;
  bool HasNSAEncoding = false;
  unsigned NSAMaxSize = 0;
  bool GFX10_AEncoding = false;
  bool GFX10_BEncoding = false;
  bool HasDLInsts = false;
  bool HasDot1Insts = false;
  bool HasDot2Insts = false;
  bool HasDot3Insts = false;
  bool HasDot4Insts = false;
  bool HasDot5Insts = false;
  bool HasDot6Insts = false;
  bool HasDot7Insts = false;
  bool HasMAIInsts = false;
  bool HasPkFmacF16Inst = false;
  bool HasAtomicFaddInsts = false;
  bool SupportsSRAMECC = false;

  // This should not be used directly. 'TargetID' tracks the dynamic settings
  // for SRAMECC.
  bool EnableSRAMECC = false;

  bool HasNoSdstCMPX = false;
  bool HasVscnt = false;
  bool HasGetWaveIdInst = false;
  bool HasSMemTimeInst = false;
  bool HasShaderCyclesRegister = false;
  bool HasVOP3Literal = false;
  bool HasNoDataDepHazard = false;
  bool FlatAddressSpace = false;
  bool FlatInstOffsets = false;
  bool FlatGlobalInsts = false;
  bool FlatScratchInsts = false;
  bool ScalarFlatScratchInsts = false;
  bool HasArchitectedFlatScratch = false;
  bool EnableFlatScratch = false;
  bool AddNoCarryInsts = false;
  bool HasUnpackedD16VMem = false;
  bool LDSMisalignedBug = false;
  bool HasMFMAInlineLiteralBug = false;
  bool UnalignedBufferAccess = false;
  bool UnalignedDSAccess = false;
  bool HasPackedTID = false;
  bool ScalarizeGlobal = false;

  bool HasVcmpxPermlaneHazard = false;
  bool HasVMEMtoScalarWriteHazard = false;
  bool HasSMEMtoVectorWriteHazard = false;
  bool HasInstFwdPrefetchBug = false;
  bool HasVcmpxExecWARHazard = false;
  bool HasLdsBranchVmemWARHazard = false;
  bool HasNSAtoVMEMBug = false;
  bool HasNSAClauseBug = false;
  bool HasOffset3fBug = false;
  bool HasFlatSegmentOffsetBug = false;
  bool HasImageStoreD16Bug = false;
  bool HasImageGather4D16Bug = false;

  // Dummy feature to use for assembler in tablegen.
  bool FeatureDisable = false;

  SelectionDAGTargetInfo TSInfo;
private:
  SIInstrInfo InstrInfo;
  SITargetLowering TLInfo;
  SIFrameLowering FrameLowering;

public:
  // See COMPUTE_TMPRING_SIZE.WAVESIZE, 13-bit field in units of 256-dword.
  static const unsigned MaxWaveScratchSize = (256 * 4) * ((1 << 13) - 1);

  GCNSubtarget(const Triple &TT, StringRef GPU, StringRef FS,
               const GCNTargetMachine &TM);
  ~GCNSubtarget() override;

  GCNSubtarget &initializeSubtargetDependencies(const Triple &TT,
                                                   StringRef GPU, StringRef FS);

  const SIInstrInfo *getInstrInfo() const override {
    return &InstrInfo;
  }

  const SIFrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }

  const SITargetLowering *getTargetLowering() const override {
    return &TLInfo;
  }

  const SIRegisterInfo *getRegisterInfo() const override {
    return &InstrInfo.getRegisterInfo();
  }

  const CallLowering *getCallLowering() const override {
    return CallLoweringInfo.get();
  }

  const InlineAsmLowering *getInlineAsmLowering() const override {
    return InlineAsmLoweringInfo.get();
  }

  InstructionSelector *getInstructionSelector() const override {
    return InstSelector.get();
  }

  const LegalizerInfo *getLegalizerInfo() const override {
    return Legalizer.get();
  }

  const RegisterBankInfo *getRegBankInfo() const override {
    return RegBankInfo.get();
  }

  const AMDGPU::IsaInfo::AMDGPUTargetID &getTargetID() const {
    return TargetID;
  }

  // Nothing implemented, just prevent crashes on use.
  const SelectionDAGTargetInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }

  const InstrItineraryData *getInstrItineraryData() const override {
    return &InstrItins;
  }

  void ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS);

  Generation getGeneration() const {
    return (Generation)Gen;
  }

  /// Return the number of high bits known to be zero for a frame index.
  unsigned getKnownHighZeroBitsForFrameIndex() const {
    return countLeadingZeros(MaxWaveScratchSize) + getWavefrontSizeLog2();
  }

  int getLDSBankCount() const {
    return LDSBankCount;
  }

  unsigned getMaxPrivateElementSize(bool ForBufferRSrc = false) const {
    return (ForBufferRSrc || !enableFlatScratch()) ? MaxPrivateElementSize : 16;
  }

  unsigned getConstantBusLimit(unsigned Opcode) const;

  /// Returns if the result of this instruction with a 16-bit result returned in
  /// a 32-bit register implicitly zeroes the high 16-bits, rather than preserve
  /// the original value.
  bool zeroesHigh16BitsOfDest(unsigned Opcode) const;

  bool hasIntClamp() const {
    return HasIntClamp;
  }

  bool hasFP64() const {
    return FP64;
  }

  bool hasMIMG_R128() const {
    return MIMG_R128;
  }

  bool hasHWFP64() const {
    return FP64;
  }

  bool hasFastFMAF32() const {
    return FastFMAF32;
  }

  bool hasHalfRate64Ops() const {
    return HalfRate64Ops;
  }

  bool hasFullRate64Ops() const {
    return FullRate64Ops;
  }

  bool hasAddr64() const {
    return (getGeneration() < AMDGPUSubtarget::VOLCANIC_ISLANDS);
  }

  bool hasFlat() const {
    return (getGeneration() > AMDGPUSubtarget::SOUTHERN_ISLANDS);
  }

  // Return true if the target only has the reverse operand versions of VALU
  // shift instructions (e.g. v_lshrrev_b32, and no v_lshr_b32).
  bool hasOnlyRevVALUShifts() const {
    return getGeneration() >= VOLCANIC_ISLANDS;
  }

  bool hasFractBug() const {
    return getGeneration() == SOUTHERN_ISLANDS;
  }

  bool hasBFE() const {
    return true;
  }

  bool hasBFI() const {
    return true;
  }

  bool hasBFM() const {
    return hasBFE();
  }

  bool hasBCNT(unsigned Size) const {
    return true;
  }

  bool hasFFBL() const {
    return true;
  }

  bool hasFFBH() const {
    return true;
  }

  bool hasMed3_16() const {
    return getGeneration() >= AMDGPUSubtarget::GFX9;
  }

  bool hasMin3Max3_16() const {
    return getGeneration() >= AMDGPUSubtarget::GFX9;
  }

  bool hasFmaMixInsts() const {
    return HasFmaMixInsts;
  }

  bool hasCARRY() const {
    return true;
  }

  bool hasFMA() const {
    return FMA;
  }

  bool hasSwap() const {
    return GFX9Insts;
  }

  bool hasScalarPackInsts() const {
    return GFX9Insts;
  }

  bool hasScalarMulHiInsts() const {
    return GFX9Insts;
  }

  TrapHandlerAbi getTrapHandlerAbi() const {
    return isAmdHsaOS() ? TrapHandlerAbi::AMDHSA : TrapHandlerAbi::NONE;
  }

  bool supportsGetDoorbellID() const {
    // The S_GETREG DOORBELL_ID is supported by all GFX9 onward targets.
    return getGeneration() >= GFX9;
  }

  /// True if the offset field of DS instructions works as expected. On SI, the
  /// offset uses a 16-bit adder and does not always wrap properly.
  bool hasUsableDSOffset() const {
    return getGeneration() >= SEA_ISLANDS;
  }

  bool unsafeDSOffsetFoldingEnabled() const {
    return EnableUnsafeDSOffsetFolding;
  }

  /// Condition output from div_scale is usable.
  bool hasUsableDivScaleConditionOutput() const {
    return getGeneration() != SOUTHERN_ISLANDS;
  }

  /// Extra wait hazard is needed in some cases before
  /// s_cbranch_vccnz/s_cbranch_vccz.
  bool hasReadVCCZBug() const {
    return getGeneration() <= SEA_ISLANDS;
  }

  /// Writes to VCC_LO/VCC_HI update the VCCZ flag.
  bool partialVCCWritesUpdateVCCZ() const {
    return getGeneration() >= GFX10;
  }

  /// A read of an SGPR by SMRD instruction requires 4 wait states when the SGPR
  /// was written by a VALU instruction.
  bool hasSMRDReadVALUDefHazard() const {
    return getGeneration() == SOUTHERN_ISLANDS;
  }

  /// A read of an SGPR by a VMEM instruction requires 5 wait states when the
  /// SGPR was written by a VALU Instruction.
  bool hasVMEMReadSGPRVALUDefHazard() const {
    return getGeneration() >= VOLCANIC_ISLANDS;
  }

  bool hasRFEHazards() const {
    return getGeneration() >= VOLCANIC_ISLANDS;
  }

  /// Number of hazard wait states for s_setreg_b32/s_setreg_imm32_b32.
  unsigned getSetRegWaitStates() const {
    return getGeneration() <= SEA_ISLANDS ? 1 : 2;
  }

  bool dumpCode() const {
    return DumpCode;
  }

  /// Return the amount of LDS that can be used that will not restrict the
  /// occupancy lower than WaveCount.
  unsigned getMaxLocalMemSizeWithWaveCount(unsigned WaveCount,
                                           const Function &) const;

  bool supportsMinMaxDenormModes() const {
    return getGeneration() >= AMDGPUSubtarget::GFX9;
  }

  /// \returns If target supports S_DENORM_MODE.
  bool hasDenormModeInst() const {
    return getGeneration() >= AMDGPUSubtarget::GFX10;
  }

  bool useFlatForGlobal() const {
    return FlatForGlobal;
  }

  /// \returns If target supports ds_read/write_b128 and user enables generation
  /// of ds_read/write_b128.
  bool useDS128() const {
    return CIInsts && EnableDS128;
  }

  /// \return If target supports ds_read/write_b96/128.
  bool hasDS96AndDS128() const {
    return CIInsts;
  }

  /// Have v_trunc_f64, v_ceil_f64, v_rndne_f64
  bool haveRoundOpsF64() const {
    return CIInsts;
  }

  /// \returns If MUBUF instructions always perform range checking, even for
  /// buffer resources used for private memory access.
  bool privateMemoryResourceIsRangeChecked() const {
    return getGeneration() < AMDGPUSubtarget::GFX9;
  }

  /// \returns If target requires PRT Struct NULL support (zero result registers
  /// for sparse texture support).
  bool usePRTStrictNull() const {
    return EnablePRTStrictNull;
  }

  bool hasAutoWaitcntBeforeBarrier() const {
    return AutoWaitcntBeforeBarrier;
  }

  bool hasUnalignedBufferAccess() const {
    return UnalignedBufferAccess;
  }

  bool hasUnalignedBufferAccessEnabled() const {
    return UnalignedBufferAccess && UnalignedAccessMode;
  }

  bool hasUnalignedDSAccess() const {
    return UnalignedDSAccess;
  }

  bool hasUnalignedDSAccessEnabled() const {
    return UnalignedDSAccess && UnalignedAccessMode;
  }

  bool hasUnalignedScratchAccess() const {
    return UnalignedScratchAccess;
  }

  bool hasUnalignedAccessMode() const {
    return UnalignedAccessMode;
  }

  bool hasApertureRegs() const {
    return HasApertureRegs;
  }

  bool isTrapHandlerEnabled() const {
    return TrapHandler;
  }

  bool isXNACKEnabled() const {
    return TargetID.isXnackOnOrAny();
  }

  bool isTgSplitEnabled() const {
    return EnableTgSplit;
  }

  bool isCuModeEnabled() const {
    return EnableCuMode;
  }

  bool hasFlatAddressSpace() const {
    return FlatAddressSpace;
  }

  bool hasFlatScrRegister() const {
    return hasFlatAddressSpace();
  }

  bool hasFlatInstOffsets() const {
    return FlatInstOffsets;
  }

  bool hasFlatGlobalInsts() const {
    return FlatGlobalInsts;
  }

  bool hasFlatScratchInsts() const {
    return FlatScratchInsts;
  }

  // Check if target supports ST addressing mode with FLAT scratch instructions.
  // The ST addressing mode means no registers are used, either VGPR or SGPR,
  // but only immediate offset is swizzled and added to the FLAT scratch base.
  bool hasFlatScratchSTMode() const {
    return hasFlatScratchInsts() && (hasGFX10_3Insts() || hasGFX940Insts());
  }

  bool hasFlatScratchSVSMode() const { return GFX940Insts; }

  bool hasScalarFlatScratchInsts() const {
    return ScalarFlatScratchInsts;
  }

  bool enableFlatScratch() const {
    return flatScratchIsArchitected() ||
           (EnableFlatScratch && hasFlatScratchInsts());
  }

  bool hasGlobalAddTidInsts() const {
    return GFX10_BEncoding;
  }

  bool hasAtomicCSub() const {
    return GFX10_BEncoding;
  }

  bool hasMultiDwordFlatScratchAddressing() const {
    return getGeneration() >= GFX9;
  }

  bool hasFlatSegmentOffsetBug() const {
    return HasFlatSegmentOffsetBug;
  }

  bool hasFlatLgkmVMemCountInOrder() const {
    return getGeneration() > GFX9;
  }

  bool hasD16LoadStore() const {
    return getGeneration() >= GFX9;
  }

  bool d16PreservesUnusedBits() const {
    return hasD16LoadStore() && !TargetID.isSramEccOnOrAny();
  }

  bool hasD16Images() const {
    return getGeneration() >= VOLCANIC_ISLANDS;
  }

  /// Return if most LDS instructions have an m0 use that require m0 to be
  /// initialized.
  bool ldsRequiresM0Init() const {
    return getGeneration() < GFX9;
  }

  // True if the hardware rewinds and replays GWS operations if a wave is
  // preempted.
  //
  // If this is false, a GWS operation requires testing if a nack set the
  // MEM_VIOL bit, and repeating if so.
  bool hasGWSAutoReplay() const {
    return getGeneration() >= GFX9;
  }

  /// \returns if target has ds_gws_sema_release_all instruction.
  bool hasGWSSemaReleaseAll() const {
    return CIInsts;
  }

  /// \returns true if the target has integer add/sub instructions that do not
  /// produce a carry-out. This includes v_add_[iu]32, v_sub_[iu]32,
  /// v_add_[iu]16, and v_sub_[iu]16, all of which support the clamp modifier
  /// for saturation.
  bool hasAddNoCarry() const {
    return AddNoCarryInsts;
  }

  bool hasUnpackedD16VMem() const {
    return HasUnpackedD16VMem;
  }

  // Covers VS/PS/CS graphics shaders
  bool isMesaGfxShader(const Function &F) const {
    return isMesa3DOS() && AMDGPU::isShader(F.getCallingConv());
  }

  bool hasMad64_32() const {
    return getGeneration() >= SEA_ISLANDS;
  }

  bool hasSDWAOmod() const {
    return HasSDWAOmod;
  }

  bool hasSDWAScalar() const {
    return HasSDWAScalar;
  }

  bool hasSDWASdst() const {
    return HasSDWASdst;
  }

  bool hasSDWAMac() const {
    return HasSDWAMac;
  }

  bool hasSDWAOutModsVOPC() const {
    return HasSDWAOutModsVOPC;
  }

  bool hasDLInsts() const {
    return HasDLInsts;
  }

  bool hasDot1Insts() const {
    return HasDot1Insts;
  }

  bool hasDot2Insts() const {
    return HasDot2Insts;
  }

  bool hasDot3Insts() const {
    return HasDot3Insts;
  }

  bool hasDot4Insts() const {
    return HasDot4Insts;
  }

  bool hasDot5Insts() const {
    return HasDot5Insts;
  }

  bool hasDot6Insts() const {
    return HasDot6Insts;
  }

  bool hasDot7Insts() const {
    return HasDot7Insts;
  }

  bool hasMAIInsts() const {
    return HasMAIInsts;
  }

  bool hasPkFmacF16Inst() const {
    return HasPkFmacF16Inst;
  }

  bool hasAtomicFaddInsts() const {
    return HasAtomicFaddInsts;
  }

  bool hasNoSdstCMPX() const {
    return HasNoSdstCMPX;
  }

  bool hasVscnt() const {
    return HasVscnt;
  }

  bool hasGetWaveIdInst() const {
    return HasGetWaveIdInst;
  }

  bool hasSMemTimeInst() const {
    return HasSMemTimeInst;
  }

  bool hasShaderCyclesRegister() const {
    return HasShaderCyclesRegister;
  }

  bool hasVOP3Literal() const {
    return HasVOP3Literal;
  }

  bool hasNoDataDepHazard() const {
    return HasNoDataDepHazard;
  }

  bool vmemWriteNeedsExpWaitcnt() const {
    return getGeneration() < SEA_ISLANDS;
  }

  // Scratch is allocated in 256 dword per wave blocks for the entire
  // wavefront. When viewed from the perspective of an arbitrary workitem, this
  // is 4-byte aligned.
  //
  // Only 4-byte alignment is really needed to access anything. Transformations
  // on the pointer value itself may rely on the alignment / known low bits of
  // the pointer. Set this to something above the minimum to avoid needing
  // dynamic realignment in common cases.
  Align getStackAlignment() const { return Align(16); }

  bool enableMachineScheduler() const override {
    return true;
  }

  bool useAA() const override;

  bool enableSubRegLiveness() const override {
    return true;
  }

  void setScalarizeGlobalBehavior(bool b) { ScalarizeGlobal = b; }
  bool getScalarizeGlobalBehavior() const { return ScalarizeGlobal; }

  // static wrappers
  static bool hasHalfRate64Ops(const TargetSubtargetInfo &STI);

  // XXX - Why is this here if it isn't in the default pass set?
  bool enableEarlyIfConversion() const override {
    return true;
  }

  void overrideSchedPolicy(MachineSchedPolicy &Policy,
                           unsigned NumRegionInstrs) const override;

  unsigned getMaxNumUserSGPRs() const {
    return 16;
  }

  bool hasSMemRealTime() const {
    return HasSMemRealTime;
  }

  bool hasMovrel() const {
    return HasMovrel;
  }

  bool hasVGPRIndexMode() const {
    return HasVGPRIndexMode;
  }

  bool useVGPRIndexMode() const;

  bool hasScalarCompareEq64() const {
    return getGeneration() >= VOLCANIC_ISLANDS;
  }

  bool hasScalarStores() const {
    return HasScalarStores;
  }

  bool hasScalarAtomics() const {
    return HasScalarAtomics;
  }

  bool hasLDSFPAtomicAdd() const { return GFX8Insts; }

  /// \returns true if the subtarget has the v_permlanex16_b32 instruction.
  bool hasPermLaneX16() const { return getGeneration() >= GFX10; }

  bool hasDPP() const {
    return HasDPP;
  }

  bool hasDPPBroadcasts() const {
    return HasDPP && getGeneration() < GFX10;
  }

  bool hasDPPWavefrontShifts() const {
    return HasDPP && getGeneration() < GFX10;
  }

  bool hasDPP8() const {
    return HasDPP8;
  }

  bool has64BitDPP() const {
    return Has64BitDPP;
  }

  bool hasPackedFP32Ops() const {
    return HasPackedFP32Ops;
  }

  bool hasFmaakFmamkF32Insts() const {
    return getGeneration() >= GFX10 || hasGFX940Insts();
  }

  bool hasImageInsts() const {
    return HasImageInsts;
  }

  bool hasExtendedImageInsts() const {
    return HasExtendedImageInsts;
  }

  bool hasR128A16() const {
    return HasR128A16;
  }

  bool hasGFX10A16() const {
    return HasGFX10A16;
  }

  bool hasA16() const { return hasR128A16() || hasGFX10A16(); }

  bool hasG16() const { return HasG16; }

  bool hasOffset3fBug() const {
    return HasOffset3fBug;
  }

  bool hasImageStoreD16Bug() const { return HasImageStoreD16Bug; }

  bool hasImageGather4D16Bug() const { return HasImageGather4D16Bug; }

  bool hasNSAEncoding() const { return HasNSAEncoding; }

  unsigned getNSAMaxSize() const { return NSAMaxSize; }

  bool hasGFX10_AEncoding() const {
    return GFX10_AEncoding;
  }

  bool hasGFX10_BEncoding() const {
    return GFX10_BEncoding;
  }

  bool hasGFX10_3Insts() const {
    return GFX10_3Insts;
  }

  bool hasMadF16() const;

  bool hasMovB64() const { return GFX940Insts; }

  bool hasLshlAddB64() const { return GFX940Insts; }

  bool enableSIScheduler() const {
    return EnableSIScheduler;
  }

  bool loadStoreOptEnabled() const {
    return EnableLoadStoreOpt;
  }

  bool hasSGPRInitBug() const {
    return SGPRInitBug;
  }

  bool hasNegativeScratchOffsetBug() const { return NegativeScratchOffsetBug; }

  bool hasNegativeUnalignedScratchOffsetBug() const {
    return NegativeUnalignedScratchOffsetBug;
  }

  bool hasMFMAInlineLiteralBug() const {
    return HasMFMAInlineLiteralBug;
  }

  bool has12DWordStoreHazard() const {
    return getGeneration() != AMDGPUSubtarget::SOUTHERN_ISLANDS;
  }

  // \returns true if the subtarget supports DWORDX3 load/store instructions.
  bool hasDwordx3LoadStores() const {
    return CIInsts;
  }

  bool hasReadM0MovRelInterpHazard() const {
    return getGeneration() == AMDGPUSubtarget::GFX9;
  }

  bool hasReadM0SendMsgHazard() const {
    return getGeneration() >= AMDGPUSubtarget::VOLCANIC_ISLANDS &&
           getGeneration() <= AMDGPUSubtarget::GFX9;
  }

  bool hasReadM0LdsDmaHazard() const {
    return getGeneration() == AMDGPUSubtarget::GFX9;
  }

  bool hasReadM0LdsDirectHazard() const {
    return getGeneration() == AMDGPUSubtarget::GFX9;
  }

  bool hasVcmpxPermlaneHazard() const {
    return HasVcmpxPermlaneHazard;
  }

  bool hasVMEMtoScalarWriteHazard() const {
    return HasVMEMtoScalarWriteHazard;
  }

  bool hasSMEMtoVectorWriteHazard() const {
    return HasSMEMtoVectorWriteHazard;
  }

  bool hasLDSMisalignedBug() const {
    return LDSMisalignedBug && !EnableCuMode;
  }

  bool hasInstFwdPrefetchBug() const {
    return HasInstFwdPrefetchBug;
  }

  bool hasVcmpxExecWARHazard() const {
    return HasVcmpxExecWARHazard;
  }

  bool hasLdsBranchVmemWARHazard() const {
    return HasLdsBranchVmemWARHazard;
  }

  // Has one cycle hazard on transcendental instruction feeding a
  // non transcendental VALU.
  bool hasTransForwardingHazard() const { return GFX940Insts; }

  // Has one cycle hazard on a VALU instruction partially writing dst with
  // a shift of result bits feeding another VALU instruction.
  bool hasDstSelForwardingHazard() const { return GFX940Insts; }

  // Cannot use op_sel with v_dot instructions.
  bool hasDOTOpSelHazard() const { return GFX940Insts; }

  // Does not have HW interlocs for VALU writing and then reading SGPRs.
  bool hasVDecCoExecHazard() const {
    return GFX940Insts;
  }

  bool hasNSAtoVMEMBug() const {
    return HasNSAtoVMEMBug;
  }

  bool hasNSAClauseBug() const { return HasNSAClauseBug; }

  bool hasHardClauses() const { return getGeneration() >= GFX10; }

  bool hasGFX90AInsts() const { return GFX90AInsts; }

  /// Return if operations acting on VGPR tuples require even alignment.
  bool needsAlignedVGPRs() const { return GFX90AInsts; }

  bool hasPackedTID() const { return HasPackedTID; }

  // GFX940 is a derivation to GFX90A. hasGFX940Insts() being true implies that
  // hasGFX90AInsts is also true.
  bool hasGFX940Insts() const { return GFX940Insts; }

  /// Return the maximum number of waves per SIMD for kernels using \p SGPRs
  /// SGPRs
  unsigned getOccupancyWithNumSGPRs(unsigned SGPRs) const;

  /// Return the maximum number of waves per SIMD for kernels using \p VGPRs
  /// VGPRs
  unsigned getOccupancyWithNumVGPRs(unsigned VGPRs) const;

  /// Return occupancy for the given function. Used LDS and a number of
  /// registers if provided.
  /// Note, occupancy can be affected by the scratch allocation as well, but
  /// we do not have enough information to compute it.
  unsigned computeOccupancy(const Function &F, unsigned LDSSize = 0,
                            unsigned NumSGPRs = 0, unsigned NumVGPRs = 0) const;

  /// \returns true if the flat_scratch register should be initialized with the
  /// pointer to the wave's scratch memory rather than a size and offset.
  bool flatScratchIsPointer() const {
    return getGeneration() >= AMDGPUSubtarget::GFX9;
  }

  /// \returns true if the flat_scratch register is initialized by the HW.
  /// In this case it is readonly.
  bool flatScratchIsArchitected() const { return HasArchitectedFlatScratch; }

  /// \returns true if the machine has merged shaders in which s0-s7 are
  /// reserved by the hardware and user SGPRs start at s8
  bool hasMergedShaders() const {
    return getGeneration() >= GFX9;
  }

  /// \returns SGPR allocation granularity supported by the subtarget.
  unsigned getSGPRAllocGranule() const {
    return AMDGPU::IsaInfo::getSGPRAllocGranule(this);
  }

  /// \returns SGPR encoding granularity supported by the subtarget.
  unsigned getSGPREncodingGranule() const {
    return AMDGPU::IsaInfo::getSGPREncodingGranule(this);
  }

  /// \returns Total number of SGPRs supported by the subtarget.
  unsigned getTotalNumSGPRs() const {
    return AMDGPU::IsaInfo::getTotalNumSGPRs(this);
  }

  /// \returns Addressable number of SGPRs supported by the subtarget.
  unsigned getAddressableNumSGPRs() const {
    return AMDGPU::IsaInfo::getAddressableNumSGPRs(this);
  }

  /// \returns Minimum number of SGPRs that meets the given number of waves per
  /// execution unit requirement supported by the subtarget.
  unsigned getMinNumSGPRs(unsigned WavesPerEU) const {
    return AMDGPU::IsaInfo::getMinNumSGPRs(this, WavesPerEU);
  }

  /// \returns Maximum number of SGPRs that meets the given number of waves per
  /// execution unit requirement supported by the subtarget.
  unsigned getMaxNumSGPRs(unsigned WavesPerEU, bool Addressable) const {
    return AMDGPU::IsaInfo::getMaxNumSGPRs(this, WavesPerEU, Addressable);
  }

  /// \returns Reserved number of SGPRs. This is common
  /// utility function called by MachineFunction and
  /// Function variants of getReservedNumSGPRs.
  unsigned getBaseReservedNumSGPRs(const bool HasFlatScratch) const;
  /// \returns Reserved number of SGPRs for given machine function \p MF.
  unsigned getReservedNumSGPRs(const MachineFunction &MF) const;

  /// \returns Reserved number of SGPRs for given function \p F.
  unsigned getReservedNumSGPRs(const Function &F) const;

  /// \returns max num SGPRs. This is the common utility
  /// function called by MachineFunction and Function
  /// variants of getMaxNumSGPRs.
  unsigned getBaseMaxNumSGPRs(const Function &F,
                              std::pair<unsigned, unsigned> WavesPerEU,
                              unsigned PreloadedSGPRs,
                              unsigned ReservedNumSGPRs) const;

  /// \returns Maximum number of SGPRs that meets number of waves per execution
  /// unit requirement for function \p MF, or number of SGPRs explicitly
  /// requested using "amdgpu-num-sgpr" attribute attached to function \p MF.
  ///
  /// \returns Value that meets number of waves per execution unit requirement
  /// if explicitly requested value cannot be converted to integer, violates
  /// subtarget's specifications, or does not meet number of waves per execution
  /// unit requirement.
  unsigned getMaxNumSGPRs(const MachineFunction &MF) const;

  /// \returns Maximum number of SGPRs that meets number of waves per execution
  /// unit requirement for function \p F, or number of SGPRs explicitly
  /// requested using "amdgpu-num-sgpr" attribute attached to function \p F.
  ///
  /// \returns Value that meets number of waves per execution unit requirement
  /// if explicitly requested value cannot be converted to integer, violates
  /// subtarget's specifications, or does not meet number of waves per execution
  /// unit requirement.
  unsigned getMaxNumSGPRs(const Function &F) const;

  /// \returns VGPR allocation granularity supported by the subtarget.
  unsigned getVGPRAllocGranule() const {
    return AMDGPU::IsaInfo::getVGPRAllocGranule(this);
  }

  /// \returns VGPR encoding granularity supported by the subtarget.
  unsigned getVGPREncodingGranule() const {
    return AMDGPU::IsaInfo::getVGPREncodingGranule(this);
  }

  /// \returns Total number of VGPRs supported by the subtarget.
  unsigned getTotalNumVGPRs() const {
    return AMDGPU::IsaInfo::getTotalNumVGPRs(this);
  }

  /// \returns Addressable number of VGPRs supported by the subtarget.
  unsigned getAddressableNumVGPRs() const {
    return AMDGPU::IsaInfo::getAddressableNumVGPRs(this);
  }

  /// \returns Minimum number of VGPRs that meets given number of waves per
  /// execution unit requirement supported by the subtarget.
  unsigned getMinNumVGPRs(unsigned WavesPerEU) const {
    return AMDGPU::IsaInfo::getMinNumVGPRs(this, WavesPerEU);
  }

  /// \returns Maximum number of VGPRs that meets given number of waves per
  /// execution unit requirement supported by the subtarget.
  unsigned getMaxNumVGPRs(unsigned WavesPerEU) const {
    return AMDGPU::IsaInfo::getMaxNumVGPRs(this, WavesPerEU);
  }

  /// \returns max num VGPRs. This is the common utility function
  /// called by MachineFunction and Function variants of getMaxNumVGPRs.
  unsigned getBaseMaxNumVGPRs(const Function &F,
                              std::pair<unsigned, unsigned> WavesPerEU) const;
  /// \returns Maximum number of VGPRs that meets number of waves per execution
  /// unit requirement for function \p F, or number of VGPRs explicitly
  /// requested using "amdgpu-num-vgpr" attribute attached to function \p F.
  ///
  /// \returns Value that meets number of waves per execution unit requirement
  /// if explicitly requested value cannot be converted to integer, violates
  /// subtarget's specifications, or does not meet number of waves per execution
  /// unit requirement.
  unsigned getMaxNumVGPRs(const Function &F) const;

  unsigned getMaxNumAGPRs(const Function &F) const {
    return getMaxNumVGPRs(F);
  }

  /// \returns Maximum number of VGPRs that meets number of waves per execution
  /// unit requirement for function \p MF, or number of VGPRs explicitly
  /// requested using "amdgpu-num-vgpr" attribute attached to function \p MF.
  ///
  /// \returns Value that meets number of waves per execution unit requirement
  /// if explicitly requested value cannot be converted to integer, violates
  /// subtarget's specifications, or does not meet number of waves per execution
  /// unit requirement.
  unsigned getMaxNumVGPRs(const MachineFunction &MF) const;

  void getPostRAMutations(
      std::vector<std::unique_ptr<ScheduleDAGMutation>> &Mutations)
      const override;

  std::unique_ptr<ScheduleDAGMutation>
  createFillMFMAShadowMutation(const TargetInstrInfo *TII) const;

  bool isWave32() const {
    return getWavefrontSize() == 32;
  }

  bool isWave64() const {
    return getWavefrontSize() == 64;
  }

  const TargetRegisterClass *getBoolRC() const {
    return getRegisterInfo()->getBoolRC();
  }

  /// \returns Maximum number of work groups per compute unit supported by the
  /// subtarget and limited by given \p FlatWorkGroupSize.
  unsigned getMaxWorkGroupsPerCU(unsigned FlatWorkGroupSize) const override {
    return AMDGPU::IsaInfo::getMaxWorkGroupsPerCU(this, FlatWorkGroupSize);
  }

  /// \returns Minimum flat work group size supported by the subtarget.
  unsigned getMinFlatWorkGroupSize() const override {
    return AMDGPU::IsaInfo::getMinFlatWorkGroupSize(this);
  }

  /// \returns Maximum flat work group size supported by the subtarget.
  unsigned getMaxFlatWorkGroupSize() const override {
    return AMDGPU::IsaInfo::getMaxFlatWorkGroupSize(this);
  }

  /// \returns Number of waves per execution unit required to support the given
  /// \p FlatWorkGroupSize.
  unsigned
  getWavesPerEUForWorkGroup(unsigned FlatWorkGroupSize) const override {
    return AMDGPU::IsaInfo::getWavesPerEUForWorkGroup(this, FlatWorkGroupSize);
  }

  /// \returns Minimum number of waves per execution unit supported by the
  /// subtarget.
  unsigned getMinWavesPerEU() const override {
    return AMDGPU::IsaInfo::getMinWavesPerEU(this);
  }

  void adjustSchedDependency(SUnit *Def, int DefOpIdx, SUnit *Use, int UseOpIdx,
                             SDep &Dep) const override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_AMDGPU_GCNSUBTARGET_H
