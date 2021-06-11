#' MAIL_Split
#' 
#' \code{MAIL_Split} runs MAIL with data splitting
#' 
#' This is a specific use of the \code{\link{MAIL}} function. 
#' The function uses the following arguments with MAIL: 
#' \itemize{
#'   \item splitOption = "Split"
#'   \item firstSOILWeightType = "BIC"
#'   \item smallestModelWeightType = "AIC"
#'   \item firstSOILPsi = 0.5
#'   \item smallestModelPsi = 0
#'   sigma2EstFunc = "LPM_AIC_CV_50Split"
#'   verbose = FALSE
#' }
#' @param XMat a matrix
#' @param yVec a vector
#' @return resList a list 

MAIL_Split = function(XMat,yVec) {
  resList = MAIL(XMat,yVec,
                 splitOption="Split",
                 firstSOILWeightType = "BIC",
                 smallestModelWeightType = "AIC",
                 firstSOILPsi = 0.5,
                 smallestModelPsi = 0,
                 sigma2EstFunc = "LPM_AIC_CV_50Split",
                 trueSD = NULL,
                 verbose=FALSE)
  return(resList)
}