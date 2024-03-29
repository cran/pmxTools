#' Read PsN SCM output into a format suitable for further use.
#'
#' \code{read_scm} returns a summary of a Perl-speaks-NONMEM (PsN, \url{https://uupharmacometrics.github.io/PsN/}) SCM (stepwise covariate modeling)
#' procedure. It depends on the presence of \code{scmlog.txt} and \code{short_scmlog.txt} files in the
#' specified directory.
#'
#' @param dir A PsN SCM folder (containing \code{scmlog.txt} and \code{short_scmlog.txt}).
#' @param startPhase Where to start collating the output; can be \code{"forward"} (the default) or \code{"backward"}.
#'
#' @return A list of data frames, containing
#'   \item{forward}{all models evaluated during the forward inclusion step of
#'   covariate model building}
#'   \item{forwardSummary}{the covariate relationships selected at each forward
#'   step}
#'   \item{forwardP}{the P-value used for inclusion during the forward inclusion step}
#'   \item{backward}{all models evaluated during the backward elimination step of covariate
#'   model building}
#'   \item{backwardSummary}{the covariate relationships eliminated at each backward step}
#'   \item{backwardP}{the P-value used for exclusion during the backward elimination step}
#' 
#' @seealso NONMEM (\url{https://www.iconplc.com/innovation/nonmem/})
#' 
#' @seealso Lindbom L, Ribbing J & Jonsson EN (2004). Perl-speaks-NONMEM (PsN) - A Perl module for NONMEM related programming. Computer Methods and Programs in Biomedicine, 75(2), 85-94. \doi{10.1016/j.cmpb.2003.11.003}
#' @seealso Lindbom L, Pihlgren P & Jonsson N (2005). PsN-Toolkit - A collection of computer intensive statistical methods for non-linear mixed effect modeling using NONMEM. Computer Methods and Programs in Biomedicine, 79(3), 241-257. \doi{10.1016/j.cmpb.2005.04.005}
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#' @family NONMEM reading
#' @examples
#' \dontrun{
#' scm <- read_scm("E:/DrugX/ModelDevelopment/scm310")
#' }
#'
#' @export
#' @importFrom stringr str_extract str_split
read_scm <- function(dir, startPhase="forward") {
  
  Drop <- PVal <- NULL  # appease CRAN
  
  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
  
  rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
  
  if(!file.exists(file.path(dir, "scmlog.txt"))) {
    stop("File \"scmlog.txt\" does not exist in this folder!")
  }
  
  if(!file.exists(file.path(dir, "short_scmlog.txt"))) {
    stop("File \"short_scmlog.txt\" does not exist in this folder!")
  }
  
  scmlog <- readLines(file.path(dir, "scmlog.txt"))
  scmlogshort <- readLines(file.path(dir, "short_scmlog.txt"))
  
  lf <- vector("list", length(scmlog))
  lb <- vector("list", length(scmlog))
  
  direc <- ""
  phase <- startPhase
  step  <- 0
  
  for (n in 1:length(scmlog)) {
    # detect step number and directory of the current step
    if(length(h <- grep("Model directory ", scmlog[n]))) {
      dir <- stringr::str_extract(scmlog[n], "([A-Z|a:z]:\\\\(.)*$)")
      step <- step + 1
    }
    
    # forward or backward?
    if(length(h <- grep("Forward search done", scmlog[n]))) {
      phase <- "backward"
      step  <- 1
    }
    
    ### add row (forward)
    if((length(i <- grep(">", scmlog[n]))) & phase=="forward") {
      str <- stringr::str_split(scmlog[n], "(\\s)+?")
      str <- str[[1]][str[[1]]!=""]
      model <- str[1]
      test  <- str[2]
      bOFV  <- suppressWarnings(as.numeric(str[3]))
      nOFV  <- as.numeric(str[4])
      drop  <- as.numeric(str[5])
      goal  <- as.numeric(str[7])
      ddf   <- as.numeric(str[8])
      pval  <- as.numeric(str[length(str)])
      if(length(j <- grep("YES!", scmlog[n]))) {
        sign <- 1
      } else {
        sign <- 0
      }
      lf[[n]] <- c(phase, step, dir, model, test, bOFV, nOFV, drop, goal, ddf, sign, pval)
    }
    
    ### add row (backward)
    if((length(i <- grep(">", scmlog[n]))) & phase=="backward") {
      str <- stringr::str_split(scmlog[n], "(\\s)+?")
      str <- str[[1]][str[[1]]!=""]
      model <- str[1]
      test  <- str[2]
      bOFV  <- suppressWarnings(as.numeric(str[3]))
      nOFV  <- as.numeric(str[4])
      drop  <- as.numeric(str[5])
      goal  <- as.numeric(str[7])
      ddf   <- as.numeric(str[8])
      pval  <- as.numeric(str[length(str)])
      if(length(j <- grep("YES!", scmlog[n]))) {
        sign <- 1
      } else {
        sign <- 0
      }
      if(model!="CRITERION") lb[[n]] <- c(phase, step, dir, model, test, bOFV, nOFV, drop, goal, ddf, sign, pval)
    }
  }
  
  # build forward dataset #########################
  
  lf                  <- rmNullObs(lf)
  
  if(length(lf)>0) {
    scmf                <- do.call(rbind, lf)
    dimnames(scmf)[[2]] <- c("Phase","Step","Dir","Model","Test","BaseOFV","NewOFV","Drop","Goal","dDF","Significant","PVal")
    scmf                <- as.data.frame(scmf)
    
    scmf$Step    <- ordered(scmf$Step, as.character(1:max(as.numeric(as.character(scmf$Step)))))
    scmf$Dir     <- as.character(scmf$Dir)
    scmf$Model   <- as.character(scmf$Model)
    scmf$BaseOFV <- as.numeric(as.character(scmf$BaseOFV))
    scmf$NewOFV  <- as.numeric(as.character(scmf$NewOFV))
    scmf$Drop    <- as.numeric(as.character(scmf$Drop))
    scmf$Goal    <- as.numeric(as.character(scmf$Goal))
    scmf$dDF     <- as.numeric(as.character(scmf$dDF))
    scmf$PVal    <- as.numeric(as.character(scmf$PVal))
  }
  
  # build backward dataset #########################
  
  lb                  <- rmNullObs(lb)
  
  if(length(lb)>0) {
    scmb                <- do.call(rbind, lb)
    dimnames(scmb)[[2]] <- c("Phase","Step","Dir","Model","Test","BaseOFV","NewOFV","Drop","Goal","dDF","Insignificant","PVal")
    scmb                <- as.data.frame(scmb)
    
    scmb$Step    <- ordered(scmb$Step, as.character(1:max(as.numeric(as.character(scmb$Step)))))
    scmb$Dir     <- as.character(scmb$Dir)
    scmb$Model   <- as.character(scmb$Model)
    scmb$BaseOFV <- as.numeric(as.character(scmb$BaseOFV))
    scmb$NewOFV  <- as.numeric(as.character(scmb$NewOFV))
    scmb$Drop    <- as.numeric(as.character(scmb$Drop))
    scmb$Goal    <- as.numeric(as.character(scmb$Goal))
    scmb$dDF     <- as.numeric(as.character(scmb$dDF))
    scmb$PVal    <- as.numeric(as.character(scmb$PVal))
  }
  
  if (exists("scmf")) {
    a1 <- split(scmf, scmf$Step)
    a2 <- lapply(a1, function(x) {
      if(nrow(x) > 0) {
        z <- subset(x, PVal == min(PVal))
        if (nrow(z) > 1) {
          z <- subset(z, Drop == max(Drop))
        }
        z
      }
    })
    fwdSummary <- do.call(rbind, a2)[, c("Model", "BaseOFV", "NewOFV", "Drop", "dDF")]
    names(fwdSummary) <- c("BestModel", "BaseOFV", "NewOFV", "dOFV", "DF")
    fwdSummary$dOFV <- -1 * fwdSummary$dOFV
  }
  
  if (exists("scmb")) {
    a1 <- split(scmb, scmb$Step)
    a2 <- lapply(a1, function(x) {
      if(nrow(x) > 0) {
        z <- subset(x, PVal == min(PVal))
        if (nrow(z) > 1) {
          z <- subset(z, Drop == max(Drop))
        }
        z
      }
    })
    bwdSummary <- do.call(rbind, a2)[, c("Model", "BaseOFV", "NewOFV", "Drop", "dDF")]
    names(bwdSummary) <- c("BestModel", "BaseOFV", "NewOFV", "dOFV", "DF")
    #bwdSummary$dOFV <- -1*bwdSummary$dOFV
  }

  out <- list()
  if(exists("scmf")){
    out$forward <- scmf
    out$forwardSummary <- fwdSummary
    out$forwardP <- signif(1-pchisq(unique(abs(scmf$Goal)), df=1),2)
  }
  if(exists("scmb")){
    out$backward <- scmb
    out$backwardSummary <- bwdSummary
    out$backwardP <- signif(1-pchisq(unique(abs(scmb$Goal)), df=1),2)
  }

  out

}

