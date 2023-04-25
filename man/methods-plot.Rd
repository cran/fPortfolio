\name{methods-plot}


\alias{plot-methods}
\alias{.fportfolio.plot.1}
\alias{.fportfolio.plot.2}
\alias{.fportfolio.plot.3}
\alias{.fportfolio.plot.4}
\alias{.fportfolio.plot.5}
\alias{.fportfolio.plot.6}
\alias{.fportfolio.plot.7}
\alias{.fportfolio.plot.8}

\title{plot-methods}


\description{

    Various plot-methods. In particalur, functions
    \code{.fportfolio.plot.[i]()} will:
    \enumerate{
      \item plot the efficient frontier,
      \item add minimum risk portfolio,
      \item add tangency portfolio,
      \item add risk/return of single assets,
      \item add equal weights portfolio,
      \item add two asset frontiers [0-1 PF only],
      \item add Monte Carlo portfolios, and/or
      \item add Sharpe ratio [MV PF only].
    }
}

\usage{
.fportfolio.plot.1(x)
.fportfolio.plot.2(x)
.fportfolio.plot.3(x)
.fportfolio.plot.4(x)
.fportfolio.plot.5(x)
.fportfolio.plot.6(x)
.fportfolio.plot.7(x)
.fportfolio.plot.8(x)
}



\arguments{

    \item{x}{an object of class \code{fPORTFOLIO}}

}

\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

