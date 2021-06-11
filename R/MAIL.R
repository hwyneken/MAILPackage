#' MAIL
#' 
#' \code{MAIL} runs the Model-Averaged Inferential Learning method under different parameter settings
#' 
#' The most important choice is whether or not use data splitting. 
#' The advantage of data splitting is to mitigate post selection changes to inference.
#' The advantage of using all of the data is to reduce bias.
#' 
#' @param XMat a n by p numeric matrix
#' @param yVec a n by 1 numeric vector
#' @param splitOption Mandatory - can take the values "Full" or "Split"
#' @param firstSOILWeightype Mandatory - can take values "AIC", "BIC" or "ARM"
#' @param smallestModelWeightType Mandatory - can take values "AIC", "BIC" or "ARM"
#' @param firstSOILPsi Mandatory - can take any value in [0,1]
#' @param smallestModelPsi Mandatory - can take any value in [0,1]
#' @param sigma2EstFunc Mandatory - this is a string of the function that will estimate the error variance using only XMat and yVec. We recommend using "LPM_AIC_CV_50Split". If the error variance is known, use "trueValue" here.
#' @param trueSD Optional unless "trueValue" has given to the previous argument. This is where the user gives the assumed error standard deviation.
#' @param verbose Optional: default is FALSE - set to TRUE if you want to see printed messages about MAIL's progress.


MAIL = function(XMat,yVec,
                splitOption,
                firstSOILWeightType,
                smallestModelWeightType,
                firstSOILPsi,
                smallestModelPsi,
                sigma2EstFunc,
                trueSD=NULL,
                verbose=FALSE) {
  # 1) select variables: high vs low value on the right dataset
  # 2) calculate weights
  # 3) construct a candidate matrix

  if (verbose==TRUE) {
    print("Step 1: Organize Data")
  }
  
  N = dim(XMat)[1]
  p = dim(XMat)[2]
  
  if (splitOption == "Split") {
    expInds = sample(1:N,size=floor(N/2),replace=FALSE)
    
    xExp = XMat[expInds,]
    xCon = XMat[-1*expInds,]
    
    yExp = yVec[expInds]
    yCon = yVec[-1*expInds]
    
    NExp = dim(xExp)[1]
    pExp = dim(xExp)[2]
    
    NCon = dim(xCon)[1]
    pCon = dim(xCon)[2]
  }
  else {
    xExp = XMat
    xCon = XMat
    
    yExp = yVec
    yCon = yVec
    
    NExp = dim(xExp)[1]
    pExp = dim(xExp)[2]
    
    NCon = dim(xCon)[1]
    pCon = dim(xCon)[2]
  }
  
  if (verbose == TRUE) {
    print("Step 2: Run First Model Average")
  }
  
  if (firstSOILWeightType != "ARM") {
    soilRes = SOIL(x=XMat,y=yVec,
                   weight_type=firstSOILWeightType,
                   psi=firstSOILPsi,family="gaussian",method="union")
  }
  else {
    soilRes = SOIL(x=XMat,yVec,
                   weight_type = "ARM",
                   psi=firstSOILPsi,family="gaussian",method="union",
                   n_train = ceiling(NExp/2)+4)
  }
  
  if (verbose == TRUE) {
    print("Step 3: Select Variables for the Nested Candidate Set")
  }
  
  allSOILScores = as.numeric(soilRes$importance)
  numModels = min(c(floor(dim(xExp)[1]/2),floor(dim(xExp)[2]/2)))
  soilCutoff = sort(allSOILScores,decreasing=TRUE)[numModels]
  selectedSet = which(as.numeric(soilRes$importance) >= soilCutoff)
  
  if (length(selectedSet) > numModels) {
    selectedSet = selectedSet[order(allSOILScores[selectedSet],decreasing=TRUE)[1:numModels]]
  }
  numSelected = length(selectedSet)
  
  selectedSOILScores = as.numeric(soilRes$importance)[selectedSet]
  selectedSetSorted = selectedSet[order(selectedSOILScores,decreasing=TRUE)]
  
  
  ### create the candidate matrix
  if (verbose == TRUE) {
    print("Step 4: Create Candidate Set, and Smallest Model")
  }
  
  candMat = matrix(0,nrow=numSelected,ncol=p)
  tempVarSet = c()
  for (i in 1:numSelected) {
    tempVarSet = c(tempVarSet,selectedSetSorted[i])
    candMat[i,tempVarSet] = 1
  }
  
  reRunSOIL_SmallestModel = SOIL(x=xExp,y=yExp,family="gaussian",weight_type=smallestModelWeightType,
                                 psi=smallestModelPsi,n_train_bound = numModels + 2,
                                 n_train = numModels + 4,
                                 candidate_models = candMat,method="customize")
  
  minInd = which.max(reRunSOIL_SmallestModel$weight)
  maxInd = dim(candMat)[1]
  
  if (verbose == TRUE) {
    print("Step 5: Estimate sigma^2")
  }
  
  ##### Variance Estimation
  if (sigma2EstFunc != "trueValue") {
    tempSigma2Func = get(sigma2EstFunc)
    estSigma2 = tempSigma2Func(XMat,yVec)
  }
  else {
    estSigma2 = trueSD^2
  }
  
  ### now that we have an estimate of sigma^2,
  ### we can through out the obviously wrong models that are too large
  ### choose the number of variables as "largestIndex" -
  ### in other words choose min AIC-corrected as the cutoff
  
  
  if (verbose == TRUE) {
    print("Step 6: Estimate Final Weights")
  }
  
  origSelectedSet = selectedSet
  
  candMat = candMat[minInd:maxInd,]
  if (minInd == maxInd) {
    candMat = matrix(candMat,nrow=1)
    modelWeight = 1
  }
  else {
    if (firstSOILWeightType != "ARM") {
      finalSOIL = SOIL(x=XMat,y=yVec,family="gaussian",weight_type=firstSOILWeightType,
                       psi=firstSOILPsi,
                       candidate_models = candMat,method="customize")
    }
    else {
      finalSOIL = SOIL(x=XMat,y=yVec,family="gaussian",weight_type="ARM",
                       psi=firstSOILPsi,n_train = ceiling(NExp/2)+4,
                       candidate_models = candMat,method="customize")
    }
    modelWeight = finalSOIL$weight
  }
  
  if (verbose == TRUE) {
    print("Step 7: Get MAIL Estimates and CI's")
  }
  
  numCand = dim(candMat)[1]
  selectedSet = which(candMat[numCand,] != 0)
  numModels = numCand
  numSelected = length(selectedSet)
  
  tempCoefVec <- rep(0,numSelected)
  tempVarVec <- rep(0,numSelected)
  
  ### need to speed this up
  coefList <- list() # list of coefficients from each submodel
  covMatList <- list() # list of information matrices for each submodel
  for (i in 1:numCand) {
    tempX <- xCon[,which(candMat[i,] != 0)]
    if (sum(candMat[i,] != 0) == 1) {
      tempX <- matrix(tempX,ncol=1)
    }
    colnames(tempX) = paste("V",which(candMat[i,] != 0),sep="")
    tempDF = data.frame(y=yCon)
    tempDF = cbind(tempDF,tempX)
    tempM = lm(y~.,data=tempDF)
    
    coefList[[i]] <- coef(summary(tempM))
    covMatList[[i]] <- summary(tempM)$cov.unscaled
  }
  
  
  for (i in 1:numSelected) {
    tempVar = selectedSet[i]
    tempModelInds = which(candMat[,tempVar] != 0)
    smallestModel = min(tempModelInds)
    tempModelWeight = modelWeight[tempModelInds] / sum(modelWeight[tempModelInds])
    numTempInds = length(tempModelInds)
    
    tempCoefVec2 <- rep(0,times=numTempInds)
    tempVarVec2 <- rep(0,times=numTempInds)
    for (j in 1:numTempInds) {
      tempInd = tempModelInds[j]
      
      
      tempCoefVec2[j] <- tempModelWeight[j]*coefList[[tempInd]][paste0("V",tempVar),1]
      
      tempWeight2 = ifelse(tempInd == numCand,
                           tempModelWeight[j]^2,
                           tempModelWeight[j]^2 + 2*tempModelWeight[j]*sum(tempModelWeight[(j+1):numTempInds]))
      
      tempCovMat <- covMatList[[tempInd]]
      tempVarVec2[j] <- tempWeight2 * diag(tempCovMat)[paste("V",tempVar,sep="")]
    }
    
    tempCoefVec[i] <- sum(tempCoefVec2)
    tempVarVec[i] <- sum(tempVarVec2)
  }
  
  tempVarVec <- tempVarVec *  estSigma2
  
  tempCI <- matrix(0,nrow=numSelected,ncol=2)
  tempCI[,1] <- tempCoefVec - 1.96*sqrt(tempVarVec)
  tempCI[,2] <- tempCoefVec + 1.96*sqrt(tempVarVec)
  
  betaHatMA <- rep(0,times=p)
  betaHatMA[selectedSet] <- tempCoefVec
  
  
  resList <- list(tempCI = tempCI,
                  selectedSet = selectedSet,
                  margVar = tempVarVec,
                  betaHat = betaHatMA,
                  modelWeight = modelWeight,
                  estSigma2 = estSigma2,
                  candMat = candMat,
                  origSelectedSet = origSelectedSet)
  
  return(resList)
  
}