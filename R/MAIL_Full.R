#' MAIL_Full
#' 
#' \code{MAIL_Full} runs MAIL without data splitting
#' 
#' This is a specific use of the \code{\link{MAIL}} function. 
#' The function uses the following arguments with MAIL: 
#' \itemize{
#'   \item splitOption = "Full"
#'   \item firstSOILWeightType = "BIC"
#'   \item smallestModelWeightType = "AIC"
#'   \item firstSOILPsi = 0.5
#'   \item smallestModelPsi = 0
#'   sigma2EstFunc = "LPM_AIC_CV_50Split"
#'   verbose = FALSE
#' }
#' @param XMat a n by p numeric matrix
#' @param yVec an n by 1 numeric vector
#' @return resList a list 

MAIL_Full = function(XMat,yVec) {

  
  resList = MAIL(XMat,yVec,
                 splitOption="Full",
                 firstSOILWeightType = "BIC",
                 smallestModelWeightType = "AIC",
                 firstSOILPsi = 0.5,
                 smallestModelPsi = 0,
                 sigma2EstFunc = "LPM_AIC_CV_50Split",
                 trueSD = NULL,
                 verbose=FALSE)
  return(resList)
}