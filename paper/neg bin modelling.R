rm(list = ls())

## Prepare data and model

set.seed(4125)
data <- data.table(v1 = rbinom(n=100,size = 1,prob = 0.6))
data[, out := ifelse(v1 == 0, 0, 
                     rpois(n=100,lambda = 2))]
#hist(data$out)

data[, cov1 := ifelse(v1 == 0, rbinom(n=.N,size = 1,prob = 0.4), rbinom(n=.N,size = 1,prob = 0.6))]
data[, mean(out), by = cov1]
data[, cov2 := ifelse(v1 == 0, rnorm(n=.N,mean = 30,sd = 10), rnorm(n=.N,mean = 30,sd = 15))]
data[, .(mean(cov2), sd(cov2)), by = v1]

model <- pscl::zeroinfl(out ~ cov1 + cov2, data = data, dist = "negbin")

##  Check negbin pred command
# for documentation, see pages 60ff https://cran.r-project.org/web/packages/pscl/pscl.pdf
# for interpretation, see https://stackoverflow.com/questions/22314921/no-zeros-predicted-from-zeroinfl-object-in-r

### RESPONSE -> expected count (total model)
test.dat1 <- data.table(predict(model, type = c("response")))
summary(test.dat1) # values between 0 and max
#hist(test.dat1$V1) # right-skewed

### ZERO -> expected probability of zero
test.dat2 <- data.table(predict(model, type = c("zero")))
summary(test.dat2) # values between 0 and 1
#hist(test.dat2$V1) # left skewed - should be right skewed as the 

### PROB -> expected probability of any count (including zero?!)
test.dat3 <- data.table(predict(model, type = c("prob")))
#hist(melt(test.dat3)$value) # left skewed
test.dat3$id <- 1:nrow(test.dat3)
test.dat3[, rowSums(.SD), by = id] # cumsum is not 100% but converges at around 98%!

cbind(test.dat2$V1,test.dat3$'0') # not the same output for prob(0) - probably because PROB includes probabilities for 0 and non-0!

### COUNT -> expected count (only count model)
test.dat4 <- data.table(predict(model, type = c("count")))
cbind(test.dat1$'0',test.dat2$V1) # different outputs

##  !! identify difference between "zero" and "prob" for prob(0) !!

pred.data <- data.table(predict(model, type = c("prob")))
names(pred.data) <- paste0("v",names(pred.data))
pred.data$id <- 1:nrow(pred.data)

pred.fun <- function(input = pred.data){
  
  # define parameters
  N <- nrow(input)
  pois.max <- 6 # max value in outcome
  
  # 0 or not
  random <- runif(n = N, min = 0, max = 1)
  input$yes0 <- input$v0 > random
  
  # if not 0, what is the count?
  select <- paste0("v", 1:6)
  pois.dat <- input[yes0 == F,.SD, .SDcols = names(input)[names(input) %in% c("id",select)]]
  
    # normalise probs
  
  ### CK@Jakob: WHY NORMALISE PROBs? 
  
    pois.dat[, cumprob := rowSums(.SD), .SDcols = select, by = id]
    pois.dat[, (select) := lapply(.SD, function(x) x / cumprob), .SDcols = select, by = id]
    pois.dat[, cumprob := rowSums(.SD), .SDcols = select, by = id]
    
    # draw values according to probabilites
    values_to_draw_from <- paste0(1:6)
    ##  example:
    sample(values_to_draw_from, size = 10, replace = TRUE, prob = pois.dat[, .SD, .SDcols = select][1])
    ##  draw only one per person:
    #pois.dat[, sample(values_to_draw_from, size = 100, replace = TRUE, prob = select), by = id]
    pois.dat[, count := sample(values_to_draw_from, size = 1, replace = TRUE, prob = .SD), .SDcols = select, by = id]
    pois.dat$count <- as.integer(pois.dat$count)
  
  # output
    out <- merge(input[,.(id,yes0)],
                 pois.dat[,.(id,count)], by = "id", all.x = T)
    return(out)
  
  }

sim.data <- data.table()
i <- 1
while (i<=1000){
  
  print(i)
  set.seed(1233+i)
  add <- pred.fun()
  add$iter <- i
  sim.data <- rbind(sim.data,add)
  rm(add)
  i <- i +1
  
}

sim.data[, .(yes0_mean = mean(yes0), 
             count_mean = quantile(count,0.5,na.rm = T),
             count_lci = quantile(count,0.025,na.rm = T),
             count_lci = quantile(count,0.975,na.rm = T)), by = id]

