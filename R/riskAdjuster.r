#Risk Adjuster Constructor
riskAdjuster <- function(CCs = character(0), map = df)
    {   
		if (!is.data.frame(map)) stop("one2one needs to be a data frame")
        # Create the basic methods as part of a list to be returned.
		#Default one-to-one mapping
		df <- data.frame(icd9 = character(0), hcc = character(0))
        me = list(
			CCs= CCs,
            # Define the one-to-one mapping from diagnosis to condition category
            map = map
            )
		structure(me, class = append(class(me), "riskAdjuster"))
        return(me)
    }

#Test risk adjuster initialization
test_categories <- c("2","6","39","115")
test_mapping <- read.csv(text = "0031,2\n00322,115\n00323,39\n00324,39\n0064,115\n0074,6\n0202,2\n0203,115\n0204,115\n0205,115\n0212,115\n0221,115\n0223,2\n0310,6\n0312,6", header = FALSE, colClasses = "character")
names(test_mapping) <- c("icd9","hcc")
cmsHCC <- riskAdjuster(test_categories, test_mapping)
cmsHCC
class(cmsHCC) #verify risk adjuster

#Test HHS-HCC
hhsHCC <- riskAdjuster(unique(map$hcc), map)