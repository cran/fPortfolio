\name{portfolio-portfolioFrontier}


\alias{portfolioFrontier}


\title{Efficient Portfolio Frontier}


\description{
  
    Compoutes the efficient portfolio frontier.
    
}
    

\usage{
portfolioFrontier(data, spec = portfolioSpec(), constraints = "LongOnly",
    include.mvl = TRUE, title = NULL, description = NULL)
}



\arguments{

    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{description}{ 
        a character string which allows for a brief description. 
        }
    \item{include.mvl}{ 
        a logical flag, should the minimum variance locus be added to the 
        plot? 
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{title}{
        a character string which allows for a project title.
        } 
}


\details{
    
    \bold{Portfolio Frontier:}
    \cr\cr   
    

    The function \code{portfolioFrontier} calculates the whole efficient 
    frontier. The portfolio information consists of five arguments: data, 
    specifications, constraints, title and description. 
    
    The range of the frontier is determined from the range of the asset
    returns, and the number of equidistant points in the returns, is
    calculated from the number of frontier points hold in the specifrication 
    structure. To extract or to modify the number of frontier points
    use the functions \code{getNFrontierPoints} and \code{setNFrontierPoints}.
   
    
    The \code{frontierPortfolio} function returns the properties of 
    the the efficient frontier as an S4 object of class \code{fPORTFOLIO}.

}


\value{
    
    \code{portfolioFrontier} function returns an S4 object of class 
    \code{"fPORTFOLIO"}.
    
}



\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

