## code to prepare `conselhosXconselheiros` dataset goes here
if (!"tidyverse" %in% rownames(installed.packages())) {
  install.packages("tidyverse")
}
if (!"tidygraph" %in% rownames(installed.packages())) {
  install.packages("tidygraph")
}
if (!"igraph" %in% rownames(installed.packages())) {
  install.packages("igraph")
}
library(tidyverse)
library(tidygraph)
library(igraph)
cons <- read.csv2("data-raw/sources/conselheiros.CSV") %>%
  mutate(
    conselho = str_trim(conselho) %>% str_squish() %>%
      str_to_sentence(),
    conselheiro = str_trim(conselheiro) %>% str_squish() %>%
      str_to_title()
  ) %>%
  mutate(conselho = case_when(
    str_detect(
      conselho,
      "Acompanhamento e controle social do fundo de manutenção"
    ) ~ "fundeb",
    str_detect(conselho, "previcampos") ~ "previcampos",
    T ~ conselho
  )) %>%
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
grafo <-
  graph_from_data_frame(edges, directed = F, vertices = nodes)
projes <- grafo %>% bipartite.projection()
conselhosXconselhos <- projes$proj2
conselheirosXconselheiros <- projes$proj1

conselhosXconselheiros <- grafo
use_data(conselhosXconselhos, overwrite = T)
use_data(conselheirosXconselheiros, overwrite = T)
use_data(conselhosXconselheiros, overwrite = T)
as_data_frame(conselhosXconselhos, what = "edges") %>%
  write_csv2("data-raw/sources/conselhosXconselhos.csv")

as_data_frame(conselheirosXconselheiros, what = "edges") %>%
  write_csv2("data-raw/sources/conselheirosXconselheiros.csv")

