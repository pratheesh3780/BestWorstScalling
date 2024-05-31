library(support.BWS)
set.seed(1)
 des<- find.BIB(
  trt = 9,
  b = 12,
  k = 6)
items.rice <- c("1", "2", "3", "4", "5", "6", "7","8","9")
data<-smija[,-2]
head(data,4)
response.vars <- colnames(data)[2:25]
md.data <- bws.dataset(
  data = data, # data frame containing response dataset
  response.type = 2, # format of response variables: 1 = row number format
  choice.sets = des, # choice sets
  design.type = 2, # 2 if a BIBD is assgined to choice.sets
  item.names = items.rice, # the names of items
  id = "id", # the name of respondent id variable
  response = response.vars, # the names of response variables
  model = "maxdiff") 
cs <- bws.count(md.data, cl = 2)
#countmethod<-summary(cs)
#write.csv(countmethod, "countmethod.csv", row.names = TRUE)
par(mar = c(5, 4, 2, 1))
plot(
  x = cs, 
  score = "bw",       # BW scores are used 
  pos = 4,            # Labels are added to the right of the points
  ylim = c(1, 6), # the y axis ranges from 1.6 to 2.3
  xlim =c(-5, 5))     # the x axis ranges from -3 to 3

par(mar = c(5, 4, 2, 1))
barplot(
  height = cs,
  score = "bw",    # BW scores are used
  mfrow = c(3, 3)) # Bar plots are drawn in a 3-by-3 array

par(mar = c(5, 7, 1, 1))
barplot(
  height = cs,
  score = "sbw", # Standardized BW scores are used
  mean = TRUE,   # Bar plot of mean scores is drawn
  las = 1)
dev.off()
