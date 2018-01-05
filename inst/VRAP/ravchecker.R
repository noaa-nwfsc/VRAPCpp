source("checks.R")
require(VRAP)

RAVINTFIELDS <- c(
  "BaseRegime",
  "BufMax",
  "CohortStart",
  "DL1",
  "DL2",
  "DR",
  "ECrit",
  "ERecovery",
  "EndAv",
  "MatRate",
  "MatU",
  "MaxAge",
  "MinAge",
  "NRuns",
  "NYears",
  "NumBreakPoints",
  "PTU",
  "RanSeed",
  "TCF3",
  "TCM3"
)

RAVNUMFIELDS <- c(
  "AveEnv",
  "BSRa",
  "BSRb",
  "BSRc",
  "BSRd",
  "BufferEnd",
  "BufferStart",
  "BufferStep",
  "ConvergeCrit",
  "FlowAve",
  "FlowCV",
  "FlowSD",
  "GammaFlowA",
  "GammaFlowB",
  "GammaMarA",
  "GammaMarB",
  "GammaMgmtA",
  "GammaMgmtB",
  "MarAve",
  "MarCV",
  "MarSD",
  "NatMort",
  "SRErrorA",
  "SRErrorB",
  "TCF1",
  "TCF2",
  "TCM1",
  "TCM2",
  "TargetU"
)

ravchecker <- function(ravpath=NULL) {

  if (is.null(ravpath) || !file.exists(ravpath)) {
    return(NULL)
  }
  
  inp <- tryCatch.W.E(VRAP:::GetInput(ravpath));
  if (inherits(inp$value, "error")) {
    if (exists("message",where=inp$value)) {
      return(c(inp$value$message))
    }
    else {
      return(c(inp$value));
    }
  }

  resp <- list()

  ## Check that integer-valued params are really integers
  val <- inp$value
  ir <- suppressWarnings(integer.rep(val[names(val) %in% RAVINTFIELDS]));
  irfalse <- val[names(ir[ir==FALSE])]
  if (length(irfalse) > 0) {
    resp <- c(paste("Parameter",names(irfalse),"=",irfalse,
                    "should be an integer."))
  }

  ## Check that non-integer, numeric-valued params are numeric
  nr <- suppressWarnings(numeric.rep(val[names(val) %in% RAVNUMFIELDS]));
  nrfalse <- val[names(nr[nr==FALSE])]
  if (length(nrfalse) > 0) {
    resp <- c(resp, paste("Parameter",names(nrfalse),"=",nrfalse,
                          "should be numeric."))
  }
  
  resp
}
