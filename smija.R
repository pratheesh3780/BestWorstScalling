library("support.BWS3")
library("support.CEs")
library("survival")
library("crossdes")
library("DoE.base")
options(digits = 6)
# Firstly, we create an OA with four three-level factors 
# using the function oa.design() in DoE.base.
oa <- oa.design(
  nl = c(3, 3, 3, 3),
  randomize = FALSE)
oa
# The resultant design is a matrix with nine rows and four columns. The columns of the OA correspond to attributes, 
# while the rows correspond to profiles.


# we create a BIBD with nine treatments, three treatments 
# per block, and 12 blocks using the find.BIB() function in crossdes. 

set.seed(1)
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
  o_farming   = c("less","medium","high"), 
  grafting   = c("less1","medium1","high1"), 
  landrace = c("less2","medium2","high2"),
  intercrop    = c("less3","medium3","high3")
)

bws3dsgn <- bws3.design(
  bibd = bibd,
  ffd = oa,
  attribute.levels = atr)
class(bws3dsgn)


# the function questionnaire() in support.CEs is used 
# to create Case 3 BWS questions
questionnaire(bws3dsgn)
#######################################ANALYSIS
attach(data)
bws3rsp.var <- colnames(data)[3:26]
bws3rsp.var
bws3rsp <-data
bws3dat <- bws3.dataset(
  data = bws3rsp,
  response = bws3rsp.var,
  choice.sets = bws3dsgn,
  categorical =  c("o_farming", "grafting", "landrace","intercrop"),
  optout = FALSE,
  asc = NULL,
  model = "maxdiff")

names(bws3dat)

head(bws3dat,3)
#model
bws3mf <- RES ~ less + less1 +
  medium + medium1 + medium2 +medium3+less2 + less3 + 
  high+ high1+ high2 + high3 
  #strata(STR)

#logitmodel
model <- clogit(RES ~ less + high + medium + strata(STR), data = bws3dat)
summary(model)


bws3md.cl <- Epi::clogit(
  formula = bws3mf,
  data = bws3dat)
bws3md.cl





