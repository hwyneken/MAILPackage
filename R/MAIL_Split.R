#' MAIL_Split
#' 
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
                 verbose=FALSE)
  return(resList)
}