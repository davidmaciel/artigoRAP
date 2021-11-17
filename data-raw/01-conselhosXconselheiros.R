## code to prepare `conselhosXconselheiros` dataset goes here
if(!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")
}
if(!"tidygraph" %in% rownames(installed.packages())){
  install.packages("tidygraph")
}
if(!"igraph" %in% rownames(installed.packages())){
  install.packages("igraph")
}
library(tidyverse)
library(tidygraph)
library(igraph)
cons <- read.csv2("data-raw/sources/conselheiros.CSV") %>%
  mutate(
    conselho = str_trim(conselho) %>%
      str_to_sentence(),
    conselheiro = str_trim(conselheiro) %>%
      str_to_title()
  ) %>%
  group_by(conselheiro) %>%
  mutate(id = cur_group_id()) %>%
  group_by(conselho) %>%
  mutate(id_cons = cur_group_id()) %>%
  ungroup()

conselhos <- cons %>% select("node" = conselho) %>%
  mutate(type = T)
conselheiros <- cons %>% select("node" = conselheiro) %>%
  mutate(type = F)
nodes <- bind_rows(conselhos, conselheiros) %>%
  distinct()
edges <- cons %>% select("from" = conselho, "to" = conselheiro)
grafo <- graph_from_data_frame(edges, directed = F, vertices = nodes)
projes <- grafo %>% bipartite.projection()
conselhosXconselhos <- projes$proj2 %>% igraph::as_data_frame("edges")
conselheirosXconselheiros <- projes$proj1 %>% igraph::as_data_frame("edges")
write_csv2(conselhosXconselhos, file = "data-raw/sources/conselhosXconselhos.csv")
write_csv2(conselheirosXconselheiros, file = "data-raw/sources/conselheirosXconselheiros.csv")
usethis::use_data(conselhosXconselheiros, overwrite = TRUE)
