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
oa_df <- as.data.frame(oa)

# Save data frame to CSV file
write.csv(oa_df, "oa_table.csv", row.names = FALSE)
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

bibd <- as.data.frame(bibd)

# Save data frame to CSV file
write.csv(bibd, "bibd.csv", row.names = FALSE)

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


data<-smija
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



#clogitmodel
model1 <- RES ~ high + medium +less1 + high1 + high2 + medium2 + high3 + medium3 + strata(STR)

#model2 <- clogit(RES ~ less + high + medium +less1 + high1 + medium1 +less2 + high2 + medium2 + less3 + high3 + medium3 + strata(STR), data = bws3dat)
bws3md.cl <- clogit(
  formula = model1,
  data = bws3dat)
bws3md.cl

result<-summary(bws3md.cl)
coefficients<- result$coefficients
write.csv(coefficients,"resultcoef.csv")

gofm(bws3md.cl)

mwtp(
  output = bws3md.cl,
  monetary.variables =c(5,4,3,1),
  nonmonetary.variables = c("high","medium","less1","high1", 
                              "high2","medium2","high3","medium3"),
 ) 

############################## ANALYSING THE ENTRY
# Create a sample dataframe (Replace this with your actual dataframe)
df <- data[3:26]
numbers <- 1:9
# Apply the table() function to count occurrences of each number in each column
result <- sapply(df, function(x) table(factor(x, levels = numbers)))

# Convert the result matrix to a dataframe
result_df <- as.data.frame(result)

# Print the result dataframe
print(result_df)
