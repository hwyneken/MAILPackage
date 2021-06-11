MAIL_Split = function(XMat,yVec) {
  res = MAIL(XMat,yVec,"Split",
             firstSOILWeightType = "BIC",
             smallestModelWeightType = "AIC",
             firstSOILPsi = 0.5,
             smallestModelPsi = 0,
             sigma2EstFunc = "LPM_AIC_CV_50Split")
  return(res)
}