#' MAIL_Full
#' 
#' \code{MAIL_Full} runs MAIL without data splitting
#' 
#' This is a specific use of the \code{\link{MAIL}} function. 
#' The function uses the following arguments with MAIL: 
#' \itemize{
#'   \item{splitOption}{"Full"}
#'   \item{firstSOILWeightType}{"BIC"}
#'   \item{"smallestModelWeightType"}{"AIC"}
#' }
#' @param XMat a matrix
#' @param yVec a vector
#' @return resList a list 

MAIL_Full = function(XMat,yVec) {

  
  resList = MAIL(XMat,yVec,
                 splitOption="Full",
                 firstSOILWeightType = "BIC",
                 smallestModelWeightType = "AIC",
                 firstSOILPsi = 0.5,
                 smallestModelPsi = 0,
                 sigma2EstFunc = "LPM_AIC_CV_50Split",
                 verbose=FALSE)
  return(resList)
}