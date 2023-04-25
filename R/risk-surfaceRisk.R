
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


###############################################################################
# FUNCTION:                DESCRIPTION:
#  markowitzHull            Hull for a long-only Markowitz portfolio
#  feasibleGrid             Square grid on top of the feasible set
# FUNCTION:                DESCRIPTION:
#  .scaledColors            Quantile color scaling 
################################################################################


markowitzHull <-
    function (data, nFrontierPoints=50)
{
    # Description:
    #    Returns the Hull for a long-only Markowitz portfolio
    
    # Arguments:
    #    data - an object of class 'timeSeries'
   
    # Example:
    #    hull <- markowitzHull(100*LPP2005.RET[, 1:6], nFrontierPoints=11) 
    #    plot(hull[[1]], type="n"); polygon(hull[[1]], col="grey")
    
    # FUNCTION:
    
    # Check:
    stopifnot(is.timeSeries(data))   
    
    # Compute Frontier and Minimum Variance Locus:
    Spec <- portfolioSpec()
    setNFrontierPoints(Spec) <- nFrontierPoints
    frontier <- portfolioFrontier(data, spec=Spec)
    Risks <- risks <- frontierPoints(frontier)[, 1]
    Returns <- frontierPoints(frontier)[, 2]
    
    # Compute Maximum Variance Locus - Pairwise Assets Approach:
    N <- ncol(data)
    for (i in 1:(N - 1)) for (j in (i + 1):N) {
        Data <- data[, c(i, j)]
        ans <- portfolioFrontier(Data, spec=Spec)
        coord <- frontierPoints(ans)
        nextFrontier <- approx(coord[, 2], coord[, 1], xout = Returns)$y
        naIndex <- which(is.na(nextFrontier))
        nextFrontier[naIndex] <- Risks[naIndex]
        risks <- rbind(risks, nextFrontier)
    }
    
    # Hull:
    targetReturn <- Returns
    minTargetRisk <- Risks
    maxTargetRisk <- colMaxs(risks)
    hull <- cbind(
        targetReturn = Returns, 
        minTargetRisk = Risks,
        maxTargetRisk = colMaxs(risks))
    
    # Polygon:
    polygon <- cbind(
        c(minTargetRisk, rev(maxTargetRisk)[-1]), 
        c(targetReturn, rev(targetReturn)[-1]) )
    rownames(polygon) <- 1:nrow(polygon)
    colnames(polygon) <- c("targetRisk", "targetReturn")
    
    # Return Value:
    ans <- polygon 
    attr(ans, "data") <- data
    attr(ans, "hull") <- hull
    attr(ans, "frontier") <- frontier
    invisible(ans)
}


# -----------------------------------------------------------------------------


feasibleGrid <-
    function(hull, trace=FALSE)
{
    # Description:
    #    Returns best diversified portfolios on top of the feasible Set
    
    # Arguments:
    #    hull - an object as returned from the function markowitzHull
    #    trace - a logical, should the function be traced ?
    
    # Example:
    #    hull <- markowitzHull(100*LPP2005.RET[, 1:6], nFrontierPoints=21) 
    #    grid <- feasibleGrid(hull, TRUE) 
    
    # FUNCTION:
    
    # Data:
    polygon <- hull
    data <- attr(hull, "data")
    hull <- attr(hull, "hull")
    
    # Trace Hull:
    if (trace) {
        plot(polygon)
        box(col="white")
        polygon(polygon, col="grey")
        grid()
    }
    
    # Settings:
    minRisks <- as.vector(hull[, 2])
    maxRisks <- as.vector(hull[, 3])
    minRisk <- min(minRisks)
    maxRisk <- max(maxRisks)
    targetRisks <- seq(minRisk, maxRisk, length = length(minRisks))
    targetReturns <- as.vector(hull[, 1])
    N <- length(targetReturns)
    
    # Get Weights on Grid:
    Grid <- matrix(NA, ncol=N, nrow=N)
    offset <- diff(range(targetRisks[1:2]))/2
    for (i in 1:N) {
        targetReturn <- targetReturns[i]
        for (j in 1:N) {
            targetRisk <- targetRisks[j] + offset
            if (targetRisk >= minRisks[i] && targetRisk <= maxRisks[i]) {
                Grid[j, i] <- 1
                if (trace) points(targetRisk, targetReturn, pch=19)
            }
       }
    }
    
    # Return Value:
    ans <- list(x=targetRisks, y=targetReturns, z=Grid)
    attr(ans, "data") <- data
    attr(ans, "polygon") <- polygon
    attr(ans, "hull") <- hull
    class(ans) <- c("feasibleGrid", "list")
    invisible(ans)
}


# ------------------------------------------------------------------------------


.scaledColors <- 
    function(surface, palette=topo.colors, nlevels=11)
{
    # Description:
    #   scales a color palette
    
    # Arguments:
    #    surface - a list with x,y positions and z values
    #    palette - color palette function
    #    bin - quantile bin width of contour levels
    
    # FUNCTION:
    
    # Extract Surface Risk Values:
    Z <- as.vector(surface$z)
  
    # Scale by Equidistant Quantiles:
    levels <- quantile(Z, probs=seq(from=0, to=1, length=nlevels), na.rm=TRUE)
    
    # Compose Color Palette:
    palette <- palette(nlevels-1)
  
    # Return Value:
    list(palette=palette, levels=levels)
}


###############################################################################


