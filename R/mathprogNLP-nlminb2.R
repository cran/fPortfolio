
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rnlminb2NLP                  Rmetrics Interface for NLMINB2 LP solvers 
#  nlminb2NLP                   Convenience wrapper for NLMINB2 LP solvers
#  nlminb2Control               NLMINB2 LP control parameter list
#  nlminb2                      Code imported from Rnlminb2 on R-Forge
#  rnlminb2                     Synonym for nlminb2 function
################################################################################


rnlminb2NLP <-
    function(start, objective, lower=0, upper=1, linCons, funCons, 
        control=list())
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Function wrapper for solver nlminb2()

    # FUNCTION:

    # Update Control List:
    ctrl <- nlminb2NLPControl()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control <- ctrl
    BIG <- 1e6
    N <- length(start)

    # Box Constraints:
    if(length(lower) == 1) {
        par.lower <- rep(lower, N)
    } else {
        par.lower <- lower
    }
    if(length(upper) == 1) {
        par.upper <- rep(upper, N)
    } else {
        par.upper <- upper
    }
    par.lower[is.infinite(par.lower)] <- BIG*sign(par.lower[is.infinite(par.lower)])
    par.upper[is.infinite(par.upper)] <- BIG*sign(par.upper[is.infinite(par.upper)])
  
    # Linear Constraints:
    if(missing(linCons)) {
        eqA <- ineqA <- NULL
        eqA.bound <- ineqA.lower <- ineqA.upper <- NULL
    } else {
        mat <- linCons[[1]]
        lower <- linCons[[2]]
        upper <- linCons[[3]]
        
        if(length(lower) == 1) {
        lower <- rep(lower, N)
        } else {
            lower <- lower
        }
        if(length(upper) == 1) {
            upper <- rep(upper, N)
        } else {
            upper <- upper
        }
        lower[is.infinite(lower)] <- BIG*sign(lower[is.infinite(lower)])
        upper[is.infinite(upper)] <- BIG*sign(upper[is.infinite(upper)])
        
        eqIndex <- which(lower == upper)
        ineqIndex <- which(lower != upper)
        
        if (length(eqIndex) == 0) {
            eqA <- NULL
            eqA.bound <- NULL
        } else {
            eqA <- mat[eqIndex, ]
            eqA.bound <- lower[eqIndex]
        }
        if (length(ineqIndex) == 0) {
            ineqA <- NULL
            ineqA.lower <- NULL
            ineqA.upper <- NULL
        } else {
            ineqA <- mat[ineqIndex, ]
            ineqA.lower <- lower[ineqIndex]
            ineqA.upper <- upper[ineqIndex]
        }
    }

    # Nonlinear Constraints:
    if(missing(funCons)) {
        eqFun <- ineqFun <- list()
        eqFun.bound <- ineqFun.lower <- ineqFun.upper <- NULL
    } else {
        fun <- funCons[[1]]
        lower <- funCons[[2]]
        upper <- funCons[[3]]
        eqIndex <- which(lower == upper)
        ineqIndex <- which(lower != upper)
        if (length(eqIndex) == 0) {
            eqFun <- list()
            eqFun.boud <- NULL
        } else {
            eqFun <- fun[eqIndex]
            eqFun.bound <- lower[eqIndex]
        }
        if (length(ineqIndex) == 0) {
            ineqFun <- list()
            ineqFun.lower <- NULL
            ineqFun.upper <- NULL
        } else {
            ineqFun <- fun[ineqIndex]
            ineqFun.lower <- lower[ineqIndex]
            ineqFun.upper <- upper[ineqIndex]
        }
    }

    # Optimize Portfolio:
    optim <- nlminb2NLP(
        start = start,
        objective = objective,
        par.lower = par.lower,
        par.upper = par.upper,
        eqA = eqA,
        eqA.bound = eqA.bound,
        ineqA = ineqA,
        ineqA.lower = ineqA.lower,
        ineqA.upper = ineqA.upper,
        eqFun = eqFun,
        eqFun.bound = eqFun.bound,
        ineqFun = ineqFun,
        ineqFun.lower = ineqFun.lower,
        ineqFun.upper = ineqFun.upper,
        control = control)

    # Return Value:
    value <- list(
        opt = optim,
        solution = optim$solution,
        objective = objective(optim$solution),
        status = optim$status,
        message = optim$message,
        solver = "nlminb2NLP")
    class(value) <- c("solver", "list")
    value
}


###############################################################################


nlminb2NLP <-
    function(
        start, objective,  
        par.lower = NULL, par.upper = NULL,
        eqA = NULL, eqA.bound = NULL,
        ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
        eqFun = list(), eqFun.bound = NULL,
        ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
        control = list())
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Function wrapper for solver nlminb2()

    # FUNCTION:

    # Environment:
    env <- .GlobalEnv

    # Update Control List:
    ctrl <- nlminb2NLPControl()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control <- ctrl
    N <- length(start)
    
    # Set Box Constraints:
    if (is.null(par.lower)) par.lower <- -Inf
    if (is.null(par.upper)) par.upper <- +Inf
    if (length(par.lower) == 1) par.lower <- rep(par.lower, N)
    if (length(par.upper) == 1) par.upper <- rep(par.upper, N)

    # Set Linear and Function Equality Constraints:
    if (!is.null(eqA) || length(eqFun) > 0) {
        eqfun <- function(x) {
            ans <- NULL
            if(!is.null(eqA)) {
                ans <- c(ans, eqA %*% x - eqA.bound)
            }
            if (length(eqFun) > 0)
                for (i in 1:length(eqFun))
                    ans <- c(ans, eqFun[[i]](x) - eqFun.bound[i])
            return(as.double(eval(ans, env)))
        }
    } else {
        eqfun <- NULL
    }

    # Set Linear and Function Inequality Constraints:
    if (!is.null(ineqA) || length(ineqFun) > 0) {
        leqfun <- function(x) {
            ans <- NULL
            if(!is.null(ineqA)) 
                ans <- c(ans, +ineqA %*% x - ineqA.upper)
            if(!is.null(ineqA)) 
                ans <- c(ans, -ineqA %*% x + ineqA.lower)
            if (length(ineqFun) > 0)
                for (i in 1:length(ineqFun))
                    ans <- c(ans, +ineqFun[[i]](x) - ineqFun.upper[i])
            if (length(ineqFun) > 0)
                for (i in 1:length(ineqFun))
                    ans <- c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
            return(as.double(eval(ans, env))) }
    } else {
        leqfun <- NULL
    }

    # Optimize Portfolio:
    optim <- rnlminb2(
        start = start,
        objective = objective,
        eqFun = eqfun,
        leqFun = leqfun,
        lower = par.lower,
        upper = par.upper,
        gradient = NULL,
        hessian = NULL,
        control = control,
        env = env)
    names(optim$par) <- names(start)

    # Return Value:
    value <- list(
        opt = optim,
        solution = optim$par,
        objective = objective(optim$par)[[1]],
        convergence = optim$convergence,
        message = optim$message,
        solver = "nlminb2NLP")
    class(value) <- c("solver", "list")
    
    # Return Value:
    value
}


################################################################################

## Description: 
##     NLMINB2 is an interior point nonlinear constrained programming 
##     problem interface written by Dietgelm Wuertz. The version here 
##     calls the R function nlminb from R's base environment. 
## Author: 
##     Douglas Bates and Deepayan Sarkar have written the nlminb R Port.
##     David M. Gay has written the underlying Fortran code.
##     Diethelm Wuertz has added nonlinear constraints functionality nlminb2.
##     To facilitate CRAN checking, Stefan Theussl integrated the code into
##     fPortfolio.


rnlminb2 <- function(...) {
 fPortfolio::nlminb2(...)
}

nlminb2 <- function(
    start, objective, 
    
    eqFun = NULL, 
    leqFun = NULL,
     
    lower = -Inf, 
    upper = Inf,
    
    gradient = NULL, 
    hessian = NULL, 
    
    control = list(),
    
    env = .GlobalEnv)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Nonlinear programming with nonlinear constraints
    
    # Details:
    #                        min f(x)
    #
    #                 lower_i < x_i < upper_i
    #    s/t                h_i(x)  = 0
    #                       g_i(x) <= 0
    
    # Arguments:
    #   start - numeric vector of start values
    #   objective - objective function to be minimized f(x)
    #   eqFun - equal constraint functions h_i(x) = 0
    #   leqFun - less equal constraint functions g_i(x) <= 0
    #   lower, upper - lower and upper bounds
    #   gradient - optional gradient of f(x)
    #   hessian - optional hessian of f(x)
    #   scale - control parameter
    #   control - control list
    #       eval.max - maximum number of evaluations (200)
    #       iter.max - maximum number of iterations (150) 
    #       trace - value of the objective function and the parameters 
    #           is printed every trace'th iteration (0)
    #       abs.tol - absolute tolerance (1e-20)
    #       rel.tol - relative tolerance (1e-10) 
    #       x.tol - X tolerance (1.5e-8)
    #       step.min - minimum step size (2.2e-14)
    
    # Todo:
    #   R, N and alpha should become part of the control list.
   
    # FUNCTION:
    
    # Debug:
    DEBUG = FALSE
    
    # Control List:
    ctrl = nlminb2Control()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control = ctrl
    
    # Arg Functions:
    if (DEBUG) {
        print(eqFun)
        print(eqFun(start))
        print(leqFun)
        print(leqFun(start))
    }
    
    # Composed Objective Function:
    if (is.null(eqFun(start))) {
        type = "leq"
        fun <- function(x, r) { 
            objective(x) - 
                r * sum(.Log(-leqFun(x))) }  
    } else if (is.null(leqFun(start))) {
        type = "eq"
        fun <- function(x, r) { 
            objective(x) +
                sum((eqFun(x))^2 / r) } 
    } else {
        type = "both"
        fun <- function(x, r) { 
            objective(x) +
                sum((eqFun(x))^2 / r) - 
                r * sum(.Log(-leqFun(x))) }  
    }
    
    # Compute in global environment:
    fun2 = function(x, r) {
        return(as.double(eval(fun(x, r), env)))
    }
       
    # Debug: 
    if (DEBUG) {
        print(fun)
        print(fun(start, 1))
    }
       
    # Minimization:
    steps.tol <- control$steps.tol
    R <- control$R
    beta <- control$beta
    scale <- control$scale
    
    trace = control$trace
    if (trace > 0) TRACE = TRUE else TRACE = FALSE
    
    control2 = control
    control2[["R"]] <- NULL
    control2[["beta"]] <- NULL
    control2[["steps.max"]] <- NULL
    control2[["steps.tol"]] <- NULL
    control2[["scale"]] <- NULL
    
    counts <- 0
    test <- 0
    while (counts < control$steps.max && test == 0) {
        counts = counts + 1
        ans = nlminb(
            start = start, objective = fun2, 
            gradient = gradient, hessian = hessian, 
            scale = scale, control = control2, lower = lower, upper = upper,
            r = R)
        start = ans$par
        tol = abs((fun(ans$par, R)-objective(ans$par))/objective(ans$par))
        if (!is.na(tol)) 
            if (tol < steps.tol) test = 1
        if (TRACE) {
            print(paste("counts:", counts, "R:", R))
            print(paste("   ", ans$convergence))
            print(paste("   ", ans$message))
            print(ans$par)
            print(fun(ans$par, R))
            print(objective(ans$par))
            print(tol)
        }
        R = beta * R
    } 
    
    if (TRACE) {
        print(paste("type:", type))
        cat("\n\n") 
    } 
    
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


.Log <-
function(x) 
{
    # Description:
    #   Returns log taking care of negative values
    
    # FUNCTION:
    
    # Check for negative values:
    x[x < 0] <- 0
    
    # Return Value:
    log(x)
}


################################################################################


nlminb2Control <- 
function(
    eval.max = 500, 
    iter.max = 400,
    trace = 0,
    abs.tol = 1e-20, 
    rel.tol = 1e-10,
    x.tol = 1.5e-8, 
    step.min = 2.2e-14,
    scale = 1,
    R = 1,
    beta = 0.01,
    steps.max = 10,
    steps.tol = 1e-6)         
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Control list
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # Control list:
    optim <- list(
        eval.max = eval.max, 
        iter.max = iter.max,
        trace = trace,
        abs.tol = abs.tol, 
        rel.tol = rel.tol,
        x.tol = x.tol, 
        step.min = step.min,
        scale = scale,
        R = R,
        beta = beta,
        steps.max = steps.max,
        steps.tol = steps.tol)
        
   # Return Value:
   optim
}


nlminb2NLPControl <-
    function(eval.max = 500, iter.max = 400, trace = 0, abs.tol = 1e-20,
    rel.tol = 1e-10, x.tol = 1.5e-08, step.min = 2.2e-14, scale = 1,
    R = 1, beta.tol = 1e-20) #, step.beta = 20)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Control Parameters:
    ans <- list(eval.max = eval.max, iter.max = iter.max, trace = trace,
        abs.tol = abs.tol, rel.tol = rel.tol, x.tol = x.tol,
        step.min = step.min, scale = scale, R = R, 
        beta.tol = beta.tol) #, step.beta = step.beta)
        
    # Return Value:
    ans
}


###############################################################################

