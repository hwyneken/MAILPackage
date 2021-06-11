#' type something here

MAIL_Full = function(XMat,yVec) {

  
  res = MAIL(XMat,yVec,"Full",
             firstSOILWeightType = "BIC",
             smallestModelWeightType = "AIC",
             firstSOILPsi = 0.5,
             smallestModelPsi = 0,
             sigma2EstFunc = "LPM_AIC_CV_50Split")
  return(res)
}