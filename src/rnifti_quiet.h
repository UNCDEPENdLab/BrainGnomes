#ifndef BRAINGNOMES_RNIFTI_QUIET_H
#define BRAINGNOMES_RNIFTI_QUIET_H

// RNifti's inline C++ diagnostics are guarded by NDEBUG. pkgbuild debug builds
// append -UNDEBUG after Makevars flags, so compiler flags alone do not keep
// worker logs quiet. Disable those diagnostics while including RNifti, then
// restore the caller's debug setting for the rest of BrainGnomes.
#ifndef NDEBUG
#define BRAINGNOMES_RESTORE_NDEBUG
#define NDEBUG
#endif

#include <RNifti.h>

#ifdef BRAINGNOMES_RESTORE_NDEBUG
#undef NDEBUG
#undef BRAINGNOMES_RESTORE_NDEBUG
#include <cassert>
#endif

#endif
