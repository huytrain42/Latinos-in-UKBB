require(ISLR)
library(dplyr)
library(tidyverse)
library(wesanderson)
library(gridExtra)

#Reading in data:
data <- read.delim("C:/Users/Huy/Desktop/Dataset/early")

#Converting:
data$Sex <- as.factor(data$Sex)
data <- data %>% mutate(Age = Age / 10, 
                        EastAsian = EastAsian * 10,
                        African = African * 10,
                        European = European * 10,
                        Native = Native * 10)

#Removing NA, Removing columns not needed:
#Filtering only for abdominal disease for now:
data_filtered <- data %>% drop_na() %>% 
  select(-c(AgeFraction, SexFraction, EthnicBackground, EthnicGroup, EID))

#Looping to fit a glm for every disease and every variable:
df <- data.frame()
for (x in unique(data_filtered$Disease)) {
  
  data_filtered1 <- filter(data_filtered, Disease == x)
  data_filtered1 <- data_filtered1[c(1,2,3,4,5,6,7,9,8)]
  varlist <- colnames(data_filtered1[, 2:ncol(data_filtered1)-2])
  univars <- data.frame()
  
  for (i in seq_along(varlist)) {
    
    mod <- as.formula(sprintf("Status ~ %s", varlist[i]))
    glmmodel <- suppressWarnings(glm(formula = mod, family = binomial, data = data_filtered1))
    
    univars[i,1] <- names(coef(glmmodel))[2]
    univars[i,2] <- (coef(summary(glmmodel))[2,][1])
    univars[i,3] <- (coef(summary(glmmodel))[2,][2])
    univars[i,4] <- coef(summary(glmmodel))[2,][3]
    univars[i,5] <- coef(summary(glmmodel))[2,][4]
    univars[i,6] <- x
    
  }
  
  df <- rbind(df, univars)
}

#Renaming columns:
df <- df %>% 
  rename(
    variables = V1,
    estimate = V2,
    stdError = V3,
    zval = V4,
    pval = V5,
    disease = V6
  )

#Looping and storing plots as a list:
list1 <- unique(df$variables)
p <- list()
for (i in 1:length(list1)) {
  p[[i]] <- ggplot(filter(df, variables == list1[i]), aes(x = estimate, y = disease)) + 
    geom_point(aes(fill=wes_palettes$GrandBudapest1[2]), size=5, shape=21, stroke=1) + 
    theme_bw() +
    theme(legend.position="none") + 
    ylab(list1[i]) + 
    geom_vline(xintercept=0, size=0.3, linetype="dotted")
}

#Arranging plots in a single paage:
n <- length(p)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(p, ncol=nCol))