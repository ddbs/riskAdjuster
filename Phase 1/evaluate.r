evaluate <- function(members, claims, riskAdjuster) {
	#check that member ids are unique
	numMem <- length(members$id)
	if(length(unique(members$id)) != length(members$id)) stop("Non-unique member list")
	numCCs <- length(riskAdjuster$CCs)
	vals <- matrix(rep(FALSE, numMem * numCCs), nrow = numMem, ncol = numCCs)
	ccInit <- rep(FALSE, numCCs)
	splitClaims <- split(claims, f=claims$id)
	for (mem in 1:numMem) {
		id <- members$id[mem]
		ccVals <- ccInit
		dxList <- splitClaims[[id]]$icd9
		dxInd <- match(dxList, riskAdjuster$map$icd9)
		ccInd <- match(riskAdjuster$map[dxInd,"hcc"], riskAdjuster$CCs)
		ccVals[ccInd] <- TRUE
		vals[mem,] <- ccVals
		print(paste0("completed member:", mem))
	}	
	#Return a sorted list at the member level
	return(vals)
}

test_members <- read.csv(text = "1\n2\n3", header = FALSE, colClasses = "character")
names(test_members) <- c("id")
test_claims <- read.csv(text = "1,00322\n2,00322\n1,0223\n3,0204", header = FALSE, colClasses = "character")
names(test_claims) <- c("id","icd9")
cmsCCs <- evaluate(test_members, test_claims, cmsHCC)

#hhs-HCC
hhsCCs <- evaluate(members, claims, hhsHCC)