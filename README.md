# VRAPS
R package for VRAP rewrite

### To install

```
library(devtools)
#tell devtools build only for your machine: 32- or 64-bit
#needed because C++ code in the VRAP 2.0 package
options(devtools.install.args = "--no-multiarch")
#VRAP 2.0
install_github("eeholmes/VRAPCpp")
#VRAP 1.0
install_github("eeholmes/VRAP")
```

The vignettes in the package are not built by default as that would take a long time.  If you want them built add `build_vignettes = TRUE` to the `install_github` call.  However, the normal procedure for a package is for the package developer to build the vignettes locally and then put the Rmd and html (from knit) files in the `inst/doc` folder which will then get uploaded to github.

To build all the vignettes in the `vignette` folder in the package and put the files into `inst/doc`.  Use

```
#note "." must be the base level of the package for this to work.
require(devtools)
build_vignettes()
```

Or knit the vignette and manually move the html file and Rmd file into the `inst/doc` folder.

## Vignettes

The package has vignettes that discuss the background of VRAP and show how to use it.  To view the vignettes available use:
```
browseVignettes(package="VRAPS")
```
You can open a vignette rmarkdown file using
```
vignette_rmd_file = "Background.Rmd"
fpath <- system.file("doc", vignette_rmd_file, package="VRAPS")
file.open(fpath)
```

## Notes

* Parameters that mean the same thing in VRAP 1.0 and VRAP 2.0 have the same name.
* The exception is 'prod' and 'cap' used instead of 'BSRa' and 'BSRb' since 'prod' and 'cap' are easier to understand and used in DM.
* The "Buffer" jargon from VRAP 1.0 is removed since VRAP 2.0 uses ER/Pop start and end with a specified step size.  The VRAP 2.0 terms are StepSize, StepStart, StepEnd and StepNum.  However in rav files, the step size, start and end are defined as a fraction of base ER or Pop capacity to keep the VRAP 2.0 and VRAP 1.0 rav files the same.
* StepNum in VRAP 2.0 and BufMax in VRAP 2.0 are both the total number of target ERs (or Pops) simulations.  It is not "total - 1" (as in Martin's first draft of VRAP 2.0 code).  

## to do 

* Martin: Add totAEQmort to RunSims2C().  I have defined at top.  But simFish() needs to be altered to return it.  See changes to RunSims2R().

* I did not do the BYHR SummaryStats.  I wasn't sure anyone used them and didn't want to add a bunch more code.

## Running a demo of the VRAP 1.0 emmulation

```
setwd("vignette_files")
out = Main.VRAP1("tmp.rav")
out = Main.VRAP1("tmp.rav", version="C")
```
That will run `tmp.rav` through Main.VRAP1() and make all the VRAP 1.0 output files.  Note, Martin needs to add totAEQmort to `simFish()` before this will run.


### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### License

This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. See LICENSE file for additional details.

