#' Fork of John C. Nash's function from Nonlinear Parameter Optimization using R Tools
#'
#' @param par required. a vector containing the starting point
#' @param fn required. objective function
#' @param gr required. gradient of objective function.
#' @param control optional. list of control parameters.
#' @param dowarn=TRUE by default. Set FALSE to suppress warnings
#' @return ans  a list of results
#' @export
#' 
#' 


stdesc <- function(par, fn, gr, control = list(), ...) {
  # 
  
  ## An R version of the steepest descent minimization
  # Input:
  #  par  = a vector containing the starting point
  # fn = objective function (assumed to be sufficeintly
  #   differentiable)
  #  gr = gradient of objective function
  #  control = list of control parameters
  #           maxit = a limit on the number of fn evaluations (default 500)
  #           maximize = TRUE to maximize the function (default FALSE)
  #           trace = 0 (default) for no output,
  #                  >0 for output (bigger => more output)
  # eps=1.0e-7 (default) for use in computing numerical
  #   gradient approximations.
  # dowarn=TRUE by default. Set FALSE to suppress warnings.
  #
  # Output:
  #    A list with components:
  #
  #     par: The best set of parameters found.
  #
  #   value: The value of 'fn' corresponding to 'par'.
  #
  # counts: A two-element integer vector giving the number of
  #   calls to
  # 'fn' and 'gr' respectively. This excludes those calls
  #   needed
  # to compute the Hessian, if requested, and any calls to
  #   'fn'
  # to compute a finite-difference approximation to the
  #   gradient.
  #
  # convergence: An integer code. '0' indicates successful
  #   convergence.
  #          Error codes are
  #          '0' converged
  # '1' indicates that the function evaluation count
  #   'maxfeval'
  #               was reached.
  #          '2' indicates initial point is infeasible
  #
  # message: A character string giving any additional
  #   information returned
  #          by the optimizer, or 'NULL'.
  #
  #
  #  Author:  John C Nash
  #  Date:  April 2, 2009; revised July 28, 2009
  #################################################################
  # control defaults -- idea from spg
  ctrl <- list(maxit = 500, trace = 0, eps = 1e-07, 
               dowarn = TRUE, tol=0, step0 = 0.001)
  namc <- names(control)
  if (!all(namc %in% names(ctrl))) 
    stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
  ctrl[namc] <- control
  npar<-length(par)
  if (ctrl$tol == 0) tol <- npar * (npar * .Machine$double.eps)  # for gradient test.  
  # Note -- integer overflow if npar*npar*.Machine$double.eps
  else tol<-ctrl$tol
  maxit <- ctrl$maxit  # limit on function evaluations
  trace <- ctrl$trace  # 0 for no output, >0 for output (bigger => more output)
  if (trace > 2) 
    cat("trace = ", trace, "\n")
  eps <- ctrl$eps
  fargs <- list(...)  # the ... arguments that are extra function / gradient data
  grNULL <- is.null(gr)
  dowarn <- ctrl$dowarn  #
  # gr MUST be provided
  if (grNULL) {
    require(numDeriv)
    if (control$dowarn) 
      warning("A NULL gradient function is being replaced numDeriv 'grad()'for stdesc")
    if (ctrl$trace > 1) {
      cat("Using following function in numDeriv grad()\n")
      print(fn)
    }
    mygr<-function(prm, func=fn, ...){
      gv<-grad(func=func, x=prm, ...)
    }
    #############################################
  } else { mygr <- gr }
  ############# end test gr ####################
  ## Set working parameters (See CNM Alg 22)
  if (trace > 0) {
    cat("stdescu -- J C Nash 2009 - unconstrained version CG min\n")
  }
  bvec <- par  # copy the parameter vector
  n <- length(bvec)  # number of elements in par vector
  maxfeval <- maxit  # change 091219
  ig <- 0  # count gradient evaluations
  ifn <- 1  # count function evaluations (we always make 1 try below)
  stepredn <- 0.15  # Step reduction in line search
  acctol <- 1e-04  # acceptable point tolerance
  reltest <- 100  # relative equality test
  ceps <- .Machine$double.eps * reltest
  setstep <- 1.75  # step increase factor
  accpoint <- as.logical(FALSE)  # so far do not have an acceptable point
  fail <- as.logical(FALSE)  # Method hasn't yet failed on us!
  cyclimit <- min(2.5 * n, 10 + sqrt(n))  #!! upper bound on when we restart CG cycle
  #!! getting rid of limit makes it work on negstart BUT inefficient
  # This does not appear to be in Y H Dai & Y Yuan, Annals of
  #   Operations Research 103, 33â€“47, 2001 aor01.pdf
  # in Alg 22 pascal, we can set this as user. Do we wish to allow that?
  ##    tol <- n * (n * .Machine$double.eps)  # # for gradient test.  
  ## Note -- integer overflow if n*n*d.eps
  fargs <- list(...)  # function arguments
  if (trace > 2) {
    cat("Extra function arguments:")
    print(fargs)
  }
  # Initial function value -- may NOT be at initial point
  #   specified by user.
  if (trace > 2) {
    cat("Try function at initial point:")
    print(bvec)
  }
  f <- try(fn(bvec, ...), silent = TRUE)  # Compute the function at initial point.
  if (trace > 0) {
    cat("Initial function value=", f, "\n")
  }
  if (class(f) == "try-error") {
    msg <- "Initial point is infeasible."
    if (trace > 0) 
      cat(msg, "\n")
    ans <- list(par, NA, c(ifn, 0), 2, msg)
    names(ans) <- c("par", "value", "counts", "convergence", 
                    "message")
    return(ans)
  }
  fmin <- f
  if (trace > 0) 
    cat("Initial fn=", f, "\n")
  if (trace > 2) 
    print(bvec)
  # Start the minimization process
  keepgoing <- TRUE
  notconv <- TRUE
  msg <- "not finished"  # in case we exit somehow
  oldstep <- ctrl$step0  #!! 2/3 #!!?? Why this choice?
  ####################################################################
  fdiff <- NA  # initially no decrease
  cycle <- 0  # !! cycle loop counter
  while (keepgoing && notconv) {
    ## cycle loop
    cycle <- cycle + 1
    if (trace > 0) 
      cat(ifn, " ", ig, " ", cycle, " ", fmin, "  last decrease=", 
          fdiff, "\n")
    if (trace > 2) {
      print(bvec)
      cat("\n")
    }
    if (ifn > maxfeval) {
      msg <- paste("Too many function evaluations (> ", 
                   maxfeval, ") ", sep = "")
      if (trace > 0) 
        cat(msg, "\n")
      ans <- list(par, fmin, c(ifn, ig), 1, msg)  # 1 indicates not converged in function limit
      names(ans) <- c("par", "value", "counts", "convergence", 
                      "message")
      return(ans)
    }
    par <- bvec  # save best parameters
    ig <- ig + 1
    if (ig > maxit) {
      msg <- paste("Too many gradient evaluations (> ", 
                   maxit, ") ", sep = "")
      if (trace > 0) 
        cat(msg, "\n")
      ans <- list(par, fmin, c(ifn, ig), 1, msg)  # 1 indicates not converged in function or gradient limit
      names(ans) <- c("par", "value", "counts", "convergence", 
                      "message")
      return(ans)
    }
    g <- mygr(bvec, ...)
    gradsqr <- sum(g * g)
    if (trace > 1) {
      cat("Gradsqr = ", gradsqr, " fmin=", fmin, "\n")
    }
    c <- g  # save last gradient
    if (gradsqr <= tol * (abs(fmin) + reltest)) {
      msg <- paste("Very small gradient -- gradsqr =", 
                   gradsqr, sep = " ")
      if (trace > 0) 
        cat(msg, "\n")
      keepgoing <- FALSE  # done loops -- should we break ??
      break  # to leave inner loop
    }
    t <- - g  
    gradproj <- -gradsqr  # gradient projection
    OKpoint <- FALSE
    if (trace > 2) 
      cat("Start linesearch with oldstep=", oldstep, "\n")
    steplength <- oldstep * 1.5  #!! try a bit bigger
    f <- fmin
    changed <- TRUE  # Need to set so loop will start
    while ((f >= fmin) && changed) {
      bvec <- par + steplength * t
      changed <- (!identical((bvec + reltest), (par + reltest)))
      if (changed) {
        # compute newstep, if possible
        f <- fn(bvec, ...)  # Because we need the value for linesearch, don't use try()
        # instead preferring to fail out, which will hopefully be
        #   unlikely.
        ifn <- ifn + 1
        if (is.na(f) || (!is.finite(f))) {
          warning("stdesc - undefined function")
          f <- .Machine$double.xmax
        }
        if (f < fmin) {
          f1 <- f  # Hold onto value
        }
        else {
          savestep<-steplength
          steplength <- steplength * stepredn
          if (steplength >=savestep) changed<-FALSE
          if (trace > 0) 
            cat("*")
        }
      }
    }  # end while
    changed1 <- changed  # Change in parameters occured in step reduction
    if (changed1) {
      ## ?? should we check for reduction? or is this done in if
      #   (newstep >0) ?
      newstep <- 2 * (f - fmin - gradproj * steplength)  # JN 081219 change
      if (newstep > 0) {
        newstep = -(gradproj * steplength * steplength/newstep)
      }
      bvec <- par + newstep * t
      changed <- (!identical((bvec + reltest), 
                             (par + reltest)))
      if (changed) {
        f <- fn(bvec, ...)
        ifn <- ifn + 1
      }
      if (trace > 2) 
        cat("fmin, f1, f: ", fmin, f1, f, "\n")
      if (f < min(fmin, f1)) {
        # success
        OKpoint <- TRUE
        accpoint <- (f <= fmin + gradproj * newstep * 
                       acctol)
        fdiff <- (fmin - f)  # check decrease
        fmin <- f
        oldstep <- newstep  # !! save it
      }
      else {
        if (f1 < fmin) {
          bvec <- par + steplength * t  # reset best point
          accpoint <- (f1 <= fmin + gradproj * 
                         steplength * acctol)
          OKpoint <- TRUE  # Because f1 < fmin
          fdiff <- (fmin - f1)  # check decrease
          fmin <- f1
          oldstep <- steplength  #!! save it
        }
        else {
          # no reduction
          fdiff <- NA
          accpoint <- FALSE
        }  # f1<?fmin
      }  # f < min(f1, fmin)
      if (trace > 1) 
        cat("accpoint = ", accpoint, " OKpoint = ", 
            OKpoint, "\n")
      if (!accpoint) {
        msg <- "No acceptable point -- exit loop"
        if (trace > 0) 
          cat("\n", msg, "\n")
        keepgoing <- FALSE
        break  #!!
      }
    }  # changed1
    else {
      # not changed on step redn
      if (cycle == 1) {
        msg <- " Converged -- no progress on new CG cycle"
        if (trace > 0) 
          cat("\n", msg, "\n")
        keekpgoing <- FALSE
        notconv <- FALSE  # mark as converged
        break  #!!
      }
    }  # end else
    #### End line search ####
    if (oldstep < acctol) {
      oldstep <- acctol
    }
    #!!  oldstep = setstep * newstep # change to newstep for
    #   steplength
    if (oldstep > 1) {
      oldstep <- 1
    }
    if (trace > 1) 
      cat("End inner loop, cycle =", cycle, "\n")
  }  # end of outer loop
  msg <- "stdesc seems to have converged"
  if (trace > 0) 
    cat(msg, "\n")
  #  par: The best set of parameters found.
  #  value: The value of 'fn' corresponding to 'par'.
  #  counts: number of calls to 'fn' and 'gr' (2 elements)
  # convergence: An integer code. '0' indicates successful
  #   convergence.
  #  message: A character string or 'NULL'.
  #    if (maximize) 
  #        fmin <- -fmin
  ans <- list(par, fmin, c(ifn, ig), 0, msg)
  names(ans) <- c("par", "value", "counts", "convergence", 
                  "message")
  return(ans)
}  ## end of stdesc

