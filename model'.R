oa <- oa.design(
  nl = c(3, 3, 3, 3),
  randomize = FALSE)
oa

set.seed(1)
bibd <- find.BIB(
  trt = 9,
  b = 12,
  k = 3)
bibd

atr <- list(
  chore   = c("milking", "feeding", "nursing"), 
  food    = c("butter", "ice", "caramel"), 
  outdoor = c("horse", "tractor", "cow"),
  fee     = c(3000, 6000, 9000))
bws3dsgn <- bws3.design(
  bibd = bibd,
  ffd = oa,
  attribute.levels = atr)
class(bws3dsgn)


questionnaire(bws3dsgn)


######### CREATE A MODEL dataset bws3rsp, 
########## experimenter should create in this format
bws3rsp <- bws3.response(
  design = bws3dsgn,
  b = c(0, 0, 0, -1, 0.5, -0.2, 0, 1, 0.1, 0, 1, 0.1, 0, -0.0003),
  n = 100,
  optout = TRUE, 
  categorical.attributes = c("chore", "food", "outdoor"),
  continuous.attributes  = c("fee"),
  asc = c(0,0,0,1),
  detail = F,
  seed = 123)

write.csv(bws3rsp,"dataresponse.csv")

bws3rsp.var <- colnames(bws3rsp)[3:26]
bws3rsp.var


bws3dat <- bws3.dataset(
  data = bws3rsp,
  response = bws3rsp.var,
  choice.sets = bws3dsgn,
  categorical.attributes = c("chore", "food", "outdoor"),
  continuous.attributes  = c("fee"),
  optout = TRUE,
  asc = c(0, 0, 0, 1),
  model = "maxdiff")


bws3mf <- RES ~ ASC4 + milking + feeding + butter + ice + 
  horse + tractor + fee + strata(STR)
