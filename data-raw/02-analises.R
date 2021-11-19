if(!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")
}

if(!"igraph" %in% rownames(installed.packages())){
  install.packages("igraph")
}

if (!"tidygraph" %in% rownames(installed.packages())) {
  install.packages("tidygraph")
}
if (!"visNetwork" %in% rownames(installed.packages())) {
  install.packages("visNetwork")
}
if (!"factoextra" %in% rownames(installed.packages())) {
  install.packages("factoextra")
}
if (!"dendextend" %in% rownames(installed.packages())) {
  install.packages("dendextend")
}
if (!"smacof" %in% rownames(installed.packages())) {
  install.packages("smacof")
}
if (!"glue" %in% rownames(installed.packages())) {
  install.packages("glue")
}
library(tidyverse)
library(igraph)
library(dendextend)
library(tidygraph)
library(visNetwork)
library(factoextra)
library(smacof)
data("conselheirosXconselheiros")
data("conselhosXconselhos")
data("conselhosXconselheiros")
sh_conselheiros <- read.csv2("data-raw/sources/conselheiros-SH.CSV")
sh_conselhos  <- read.csv2("data-raw/sources/conselhos-SH.CSV")
par(mar=c(1,1,1,1))
# conselheiros ---------------------------------------------------------------

## componentes principais com hclust ------------------------------------------------


ma_consp <-sh_conselheiros %>% select(-ID, -Indirects) %>%
  as.matrix() %>% scale()

prc_consp <- princomp(ma_consp)
summary(prc_consp)
sh_conselheiros <- bind_cols(sh_conselheiros,
                             as.data.frame(prc_consp$scores[,1:2]))
clust_consp <- sh_conselheiros %>%
  select(matches("Comp")) %>%
  as.matrix() %>%
  get_dist(method = "euclidean") %>%
  hclust(method = "complete")
plot(clust_consp, labels = F)

sh_conselheiros <- sh_conselheiros %>%
  mutate(grupo = cutree(clust_consp, 2))

## mds -------------------------------------------------------------------
dist_consp <- ma_consp %>%
  get_dist(method = "euclidean")
mds_consp <- mds(dist_consp, ndim = 2, type = "ratio")
 #stress de 0.151. Ajuste adequado

sh_conselheiros <- bind_cols(sh_conselheiros,
                             as.data.frame(mds_consp$conf))

with(sh_conselheiros,
     plot(x = D1, y = D2, col = grupo))

## vizualiação em rede------------------------------------------------------
rotulo <- function(name, conselhos,n_con, effsize, efficiency,
                   constraint, hierarchy){
  glue::glue("
             <i>{name}</i></br>
             <b>conselhos:</b> {conselhos}<br>
             <b>n_conselhos:</b> {n_con}<br>
             <b>effsize:</b> {effsize}<br>
             <b>efficiency:</b> {efficiency}<br>
             <b>constraint:</b> {constraint}<br>
             <b>hierarchy:</b> {hierarchy}")
}

n_grupos <- unique(sh_conselheiros$grupo) %>% length()
paleta <- RColorBrewer::brewer.pal(n_grupos, "Set1")
coordenadas <- tibble(name = V(conselheirosXconselheiros)$name) %>%
  bind_cols(as.data.frame(layout.kamada.kawai(conselheirosXconselheiros))) %>%
  rename("x" = V1, "y" = V2)

nodes <- conselhosXconselheiros %>%
  as_data_frame(what= "edges") %>%
  group_by(to) %>%
  summarise(conselhos = str_c(from, collapse = ", "),
            n_con = n()) %>%
  inner_join(sh_conselheiros, by = c("to" = "ID")) %>%
  rename("name" = to) %>%
  mutate("id" = 1:nrow(.),
         "title" = rotulo(name,conselhos,n_con, EffSize,
                          Efficiency,Constraint,Hierarchy),
         "color.background" = paleta[.$grupo],
         "color.border" = "black",
         "borderWidth" = 1,
         "size" = n_con*10) %>%
  inner_join(coordenadas)

edges <- conselheirosXconselheiros %>%
  as_data_frame(what = "edges") %>%
  left_join(nodes[,c("id","name")], by = c("from" = "name")) %>%
  left_join(nodes[,c("id","name")], by = c("to" = "name")) %>%
  select("from" = id.x, "to" = id.y, weight) %>%
  mutate(smooth.enabled = F,
         color.color = "darkgrey",
         color.opacity = 0.5)

visnet <- visNetwork(nodes, edges) %>%
  visLayout(improvedLayout = T) %>%
  visIgraphLayout(layout = "layout_with_fr",
                  niter = 1000) %>%
  # visPhysics(solver = "forceAtlas2Based",
  #            forceAtlas2Based = list(
  #              "gravity" = -50,
  #              "springConstant" = 0.05,
  #              "damping" = 0.9
  #            ),
  #            stabilization = list("iterations" = 1e3),
  #            enabled = T,
  #            adaptiveTimestep = T) %>%
  visOptions(highlightNearest = T, selectedBy = "name", clickToUse = T) %>%
  visInteraction(dragView = T, zoomView = T)


## exportar resultados -----------------------------------------------------
if(!"results" %in% dir("data-raw/")){
  dir.create("data-raw/results/")
}
visSave(visnet, file = "data-raw/results/conselheirosXconselheiros.html")

