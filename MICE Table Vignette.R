library(tidyverse); library(mice);library(flextable)

# hey we all love the iris dataset
iris<-iris

# running a basic regression model on iris

fit<-lm(Petal.Width~Species+Sepal.Length+Sepal.Width, data = iris)

# output with jtools (wow that's nice!)
jtools::export_summs(fit)


# generating a pattern for poking holes in iris
set.seed(53705)
n.miss<-round(nrow(iris)*runif((ncol(iris)-1), max = 0.3))


# poking holes in the iris dataset to illustrate mice
iris_miss<-iris%>%
  mutate(Sepal.Length=ifelse(row_number()%%n.miss[1],Sepal.Length,NA))%>%
  mutate(Sepal.Width=ifelse(row_number()%%n.miss[2],Sepal.Width,NA))%>%
  mutate(Petal.Length=ifelse(row_number()%%n.miss[3],Petal.Length,NA))%>%
  mutate(Petal.Width=ifelse(row_number()%%n.miss[4],Petal.Width,NA))

# imputing the missing values with mice
iris_imp<-mice(iris)

# estimating the same model with lm.mids
fit_imp<-lm.mids(Petal.Width~Species+Sepal.Length+Sepal.Width, data = iris_imp)

# standard results from the lm.mids object
summary(pool(fit_imp))

# Output with mice_regression_table
mice_regression_table(pool(fit_imp))
