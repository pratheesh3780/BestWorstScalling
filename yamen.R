library("support.BWS3")
library("support.CEs")
library("survival")
library("crossdes")
library("DoE.base")
options(digits = 6)
# Firstly, we create an OA with four three-level factors 
# using the function oa.design() in DoE.base.
oa <- oa.design(
  nl = c(3,3,3,3),
  randomize = FALSE)
oa
# The resultant design is a matrix with nine rows and four columns. The columns of the OA correspond to attributes, 
# while the rows correspond to profiles.


# we create a BIBD with nine treatments, three treatments 
# per block, and 12 blocks using the find.BIB() function in crossdes. 

set.seed(2)
bibd <- find.BIB(
  trt = 9,
  b = 12,
  k = 6)
bibd

## Check
isGYD(bibd)



#code chunk stores the names of the attributes and 
#their levels into an object atr, and then creates 
#a Case 3 BWS design for this example from the objects 
#bibd, oa, and atr

atr <- list(
  familiarity   = c("owner","absentee","fpo"), 
  pricing  = c("spot","season","fixed"), 
  phytosanitary = c("pesticide","tpc","organic"),
  packaging    = c("gunny","plastic","used")
)

bws3dsgn <- bws3.design(
  bibd = bibd,
  ffd = oa,
  attribute.levels = atr)
class(bws3dsgn)


# the function questionnaire() in support.CEs is used 
# to create Case 3 BWS questions
questionnaire(bws3dsgn)





