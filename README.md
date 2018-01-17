# VRAPS
R package for VRAP rewrite

### To install

```
library(devtools)
#tell devtools not to build for both 32 and 64 bit
options(devtools.install.args = "--no-multiarch")
#VRAP 2.0
install_github("eeholmes/VRAPCpp")
#VRAP 1.0
install_github("eeholmes/VRAP")
```

The vignettes in the package are not built by default as that would take a long time.  If you want them built add `build_vignettes = TRUE` to the `install_github` call.

### To install

## Notes

* Parameters that mean the same thing in VRAP 1.0 and VRAP 2.0 have the same name.
* The exception is 'prod' and 'cap' used instead of 'BSRa' and 'BSRb' since 'prod' and 'cap' are easier to understand and used in DM.
* The "Buffer" jargon from VRAP 1.0 is removed since VRAP 2.0 uses ER/Pop start and end with a specified step size.  The VRAP 2.0 terms are StepSize, StepStart, StepEnd and StepNum.  However in rav files, the step size, start and end are defined as a fraction of base ER or Pop capacity to keep the VRAP 2.0 and VRAP 1.0 rav files the same.
* StepNum in VRAP 2.0 and BufMax in VRAP 2.0 are both the total number of target ERs (or Pops) simulations.  It is not "total - 1" (as in Martin's first draft of VRAP 2.0 code).  
