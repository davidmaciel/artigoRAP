if(!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")
}
if(!"tidygraph" %in% rownames(installed.packages())){
  install.packages("tidygraph")
}
if(!"igraph" %in% rownames(installed.packages())){
  install.packages("igraph")
}
if(!"factoextra" %in% rownames(installed.packages())){
  install.packages("factoextra")
}
if(!"smacof" %in% rownames(installed.packages())){
  install.packages("smacof")
}
library(tidyverse)
library(igraph)
library(factoextra)
library(smacof)
data("conselheirosXconselheiros")
data("conselhosXconselhos")


# conselheiros ---------------------------------------------------------------

d_conselheiros <- distances(conselheirosXconselheiros) %>%
  get_dist(method = "pearson")
fit1 <- mds(d_conselheiros, ndim = 3, type = "ratio")



# conselhos ---------------------------------------------------------------
d_conselhos <- distances(conselhosXconselhos) %>%
  get_dist(method = "pearson")

fit2 <- mds(d_conselhos, ndim = 3, type = "ratio")
