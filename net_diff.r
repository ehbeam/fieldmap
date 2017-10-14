# Clear workspace
rm(list=ls())

# Import libraries
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(tools)

# Set working directory
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")
dir.create("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/differences")

# Load networks
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Set palette
palette <- c("#9a90b4", # Purple
             "#eaab00", # Yellow
             "#cc3300", # Red
             "#79b0db", # Blue
             "#77dd8c") # Green
gray <- gray.colors(102, start = 0.75, end = 0.75)

# Function for converting network names to figure labels
presentable <- function(networks, remove) {
  net.names <- c()
  for (net in networks) {
    name <- file_path_sans_ext(net)
    for (word in remove) {
      name <- gsub(word, "", name)
    }
    name <- gsub("gaussian_", "", name)
    name <- gsub("_anat", "", name)
    name <- gsub("_behav", "", name)
    name <- gsub("_func", "", name)
    name <- gsub("_", ", ", name)
    net.names <- c(net.names, name)
  }
  return(net.names)
}

# Function to plot heatmaps of network differences
plot.differences <- function(network.1, network.2, fig.height = 10, fig.width = 15, n.1 = length(palette), n.2 = length(palette), remove = c()) {
  # Load presentable names 
  net.name.1 <- presentable(network.1, remove)
  net.name.2 <- presentable(network.2, remove)
  
  # Load adjacency matrix and clusters of network 1
  file.1 <- read.csv(network.1)
  dat.1 <- as.matrix(file.1)
  net.1 <- graph.edgelist(dat.1[,1:2], directed = FALSE)
  E(net.1)$weight <- as.numeric(dat.1[,3])
  adj.1 <- as_adjacency_matrix(net.1, attr = "weight", sparse = FALSE)
  net.1.dele <- delete_edges(net.1, which(E(net.1)$weight == 0))
  net.1.delv <- simplify(net.1.dele, remove.loops = TRUE)
  clust.1 <- cluster_fast_greedy(net.1.delv, weights = E(net.1.delv)$weight)
  
  # Load adjacency matrix and clusters of network 2
  file.2 <- read.csv(network.2)
  dat.2 <- as.matrix(file.2)
  net.2 <- graph.edgelist(dat.2[,1:2], directed = FALSE)
  E(net.2)$weight <- as.numeric(dat.2[,3])
  adj.2 <- as_adjacency_matrix(net.2, attr = "weight", sparse = FALSE)
  net.2.dele <- delete_edges(net.2, which(E(net.2)$weight == 0))
  net.2.delv <- simplify(net.2.dele, remove.loops = TRUE)
  clust.2 <- cluster_fast_greedy(net.2.delv, weights = E(net.2.delv)$weight)
  
  # Compute difference matrix
  adj.1.norm <- (adj.1 - min(adj.1, na.rm = TRUE)) / (max(adj.1, na.rm = TRUE) - min(adj.1, na.rm = TRUE))
  adj.2.norm <- (adj.2 - min(adj.2, na.rm = TRUE)) / (max(adj.2, na.rm = TRUE) - min(adj.2, na.rm = TRUE))
  #dif <- adj.1.norm - adj.2.norm
  dif <- adj.1 - adj.2
  
  # Sort rows and columns by community membership and degree centrality
  ord.1 <- order(clust.1$membership, degree(net.1.delv))
  ord.2 <- order(clust.2$membership, degree(net.2.delv))
  dif <- dif[,ord.1]
  dif <- dif[ord.2,]
  
  # Set palette
  palette.1 <- c(palette[1:n.1], gray)
  palette.2 <- c(palette[1:n.2], gray)
  
  # Plot heatmap of difference matrix
  dif.melt <- melt(dif)
  heatmap.dif <- ggplot(data = dif.melt, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "gray") + theme_classic() +
    labs(y = paste("nodes grouped by community in network of", net.name.1, "\n"), 
         x = paste("\nnodes grouped by community in network of", net.name.2)) +
    scale_fill_gradient2(low = "white", high = "black", mid = "gray", 
                         midpoint = (max(dif, na.rm = TRUE) - min(dif, na.rm = TRUE))/2, 
                         limit = c(min(dif, na.rm = TRUE), max(dif, na.rm = TRUE)), 
                         space = "Lab", name = "link weight\ndifference\n") +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 11, 
                                     color = palette.1[sort(clust.1$membership)]),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 11, angle = 45, hjust = 1, 
                                     color = palette.2[sort(clust.2$membership)]),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.key.height = unit(1, "in"),
          legend.key.width = unit(0.25, "in"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 14),
          legend.title = element_text(family = "Lucida Console", size = 20),
          plot.margin = grid::unit(c(0.25, 0.25, 0.25, 0.4), "in"))
  
  # Set file name for heatmap
  file.name.1 <- file_path_sans_ext(network.1)
  file.name.2 <- file_path_sans_ext(network.2)
  file.name <- paste(file.name.1, "_MINUS_", file.name.2, ".png", sep = "")
  
  # Save heatmap
  ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/differences/", file.name, sep = ""), heatmap.dif, height = fig.height, width = fig.width, units = "in")
  
}

## WORDS ONLY ##

# Filter networks
networks.words.anat <- grep("sentences.*lemmas.*anat", networks, value = TRUE)
networks.words.behav <- c(grep("brainmap_behav", networks, value = TRUE), grep("sentences.*lemmas.*behav", networks, value = TRUE))
networks.words.func <- grep("sentences.*lemmas.*func", networks, value = TRUE)

# Execute plotting
plot.differences("texts_studies_sentences_lemmas_anat.csv", "abstracts_studies_sentences_lemmas_anat.csv", n.1 = 2, n.2 = 3, remove = c("studies_", "sentences_", "lemmas_"))
plot.differences("texts_studies_sentences_lemmas_behav.csv", "abstracts_studies_sentences_lemmas_behav.csv", n.1 = 4, n.2 = 4, remove = c("studies_", "sentences_", "lemmas_"))
plot.differences("texts_studies_sentences_lemmas_func.csv", "abstracts_studies_sentences_lemmas_func.csv", n.1 = 5, n.2 = 5, fig.height = 15, fig.width = 22.5, remove = c("studies_", "sentences_", "lemmas_"))
