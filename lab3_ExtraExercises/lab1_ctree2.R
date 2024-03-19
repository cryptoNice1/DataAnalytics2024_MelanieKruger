#### using Ctree
library(party)
data(swiss)
pairs(~ Fertility + Education + Catholic, 
      data = swiss, 
      subset = Education < 20, 
      main = "Swiss data, Education < 20")
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)


