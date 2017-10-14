# Clear workspace
rm(list=ls())

# Import libraries
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(reshape2)
library(tools)

# Set working directory
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")
dir.create("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/correlations")

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
presentable <- function(networks, remove = c()) {
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

# Load node classes
anat <- c("frontal pole", "insular cortex", "superior frontal gyrus", "middle frontal gyrus", "inferior frontal gyrus", "precentral gyrus", "temporal pole", "superior temporal gyrus", "middle temporal gyrus", "inferior temporal gyrus", "postcentral gyrus", "superior parietal lobule", "supramarginal gyrus", "angular gyrus", "lateral occipital cortex", "intracalcarine cortex", "frontal medial cortex", "supplementary motor cortex", "subcallosal cortex", "paracingulate gyrus", "cingulate gyrus", "precuneous cortex", "cuneal cortex", "frontal orbital cortex", "parahippocampal gyrus", "lingual gyrus", "temporal fusiform cortex", "temporal occipital fusiform cortex", "occipital fusiform gyrus", "frontal operculum cortex", "central opercular cortex", "parietal operculum cortex", "planum polare", "heschl's gyrus", "planum temporale", "supracalcarine cortex", "occipital pole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor learning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit memory", "implicit memory", "working memory", "music", "reasoning", "social cognition", "somatic", "spatial", "temporal cognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "interoception", "baroregulation", "gastrointestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Function to assign class (anatomy or behavior) as node attribute
classify <- function(net) {
  class = c()
  for (i in 1:length(V(net)$name)) {
    if (V(net)$name[i] %in% anat) {
      class <- c(class, "anat")
    }
    if (V(net)$name[i] %in% behav) {
      class <- c(class, "behav")
    }
  }
  return(class)
}


# Function to plot heatmaps of network differences
plot.heatmaps <- function(network.1, network.2, fig.height = 10, fig.width = 16, n.1 = length(palette), n.2 = length(palette), remove = c()) {

  # Load presentable names 
  net.name.1 <- presentable(network.1, remove = remove)
  net.name.2 <- presentable(network.2, remove = remove)
  
  # Load adjacency matrix and clusters of network 1
  file.1 <- read.csv(network.1)
  dat.1 <- as.matrix(file.1)
  net.1 <- graph.edgelist(dat.1[,1:2], directed = FALSE)
  E(net.1)$weight <- as.numeric(dat.1[,3])
  V(net.1)$class <- classify(net.1)
  adj.1 <- as_adjacency_matrix(net.1, attr = "weight", sparse = FALSE)
  adj.1[adj.1 == 0] <- NA
  #adj.1[is.na(adj.1)] <- 0
  net.1.dele <- delete_edges(net.1, which(E(net.1)$weight == 0))
  net.1.delv <- simplify(net.1.dele, remove.loops = TRUE)
  clust.1 <- cluster_fast_greedy(net.1.delv, weights = E(net.1.delv)$weight)
  
  # Load adjacency matrix and clusters of network 2
  file.2 <- read.csv(network.2)
  dat.2 <- as.matrix(file.2)
  net.2 <- graph.edgelist(dat.2[,1:2], directed = FALSE)
  E(net.2)$weight <- as.numeric(dat.2[,3])
  V(net.2)$class <- classify(net.2)
  adj.2 <- as_adjacency_matrix(net.2, attr = "weight", sparse = FALSE)
  adj.2[adj.2 == 0] <- NA
  #adj.2[is.na(adj.2)] <- 0
  net.2.dele <- delete_edges(net.2, which(E(net.2)$weight == 0))
  net.2.delv <- simplify(net.2.dele, remove.loops = TRUE)
  clust.2 <- cluster_fast_greedy(net.2.delv, weights = E(net.2.delv)$weight)

  # Compute correlation matrix
  cor.mat <- cor(adj.1, adj.2, method = "spearman", use = "pairwise.complete.obs")
  
  # Orders for sorting by community membership and degree centrality
  ord.1 <- order(clust.1$membership, V(net.1)$class, degree(net.1.delv))
  ord.2 <- order(clust.2$membership, V(net.2)$class, degree(net.2.delv))
  
  # Sort correlation matrix by communities
  cor.mat.1v2 <- cor.mat[ord.1,]
  cor.mat.1v2 <- cor.mat.1v2[,ord.2]
  
  # Sort adjacency matrices by communities
  adj.1.sort <- adj.1[ord.1,]
  adj.1.sort <- adj.1.sort[,ord.1]
  adj.2.sort <- adj.2[ord.2,]
  adj.2.sort <- adj.2.sort[,ord.2]
  
  # Set palette
  palette.1 <- c(palette[1:n.1], gray)
  palette.2 <- c(palette[1:n.2], gray)
  
  # Plot heatmap of correlation matrix with rows sorted by network 1 communities, columns sorted by network 2 communities
  cor.melt <- melt(cor.mat.1v2, na.rm = FALSE)
  heatmap.cor <- ggplot(data = cor.melt, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "gray") + theme_classic() +
    labs(y = paste("nodes grouped by community in network of", net.name.2, "\n"), 
         x = paste("\nnodes grouped by community in network of", net.name.1)) +
    scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                         midpoint = 0, 
                         limit = c(min(cor.mat.1v2, na.rm = TRUE), max(cor.mat.1v2, na.rm = TRUE)), 
                         na.value = "white",
                         space = "Lab", name = "spearman\ncorrelation\n") +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 11.5, 
                                     color = palette.2[sort(clust.2$membership)]),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 11.5, angle = 45, hjust = 1, 
                                     color = palette.1[sort(clust.1$membership)]),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.key.height = unit(1, "in"),
          legend.key.width = unit(0.25, "in"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 14),
          legend.title = element_text(family = "Lucida Console", size = 20),
          plot.margin = grid::unit(c(0.25, 0.25, 0.25, 0.4), "in"))
  
  # Plot heatmap of network 1
  melt.1 <- melt(adj.1.sort, na.rm = TRUE)
  heatmap.1 <- ggplot(data = melt.1, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "gray") + theme_classic() +
    labs(y = paste("nodes grouped by community in network of", net.name.1, "\n"), 
         x = paste("\nnodes grouped by community in network of", net.name.1)) +
    scale_fill_gradient2(low = "white", high = "black", mid = "gray",
                         midpoint = (max(adj.1, na.rm = TRUE) - min(adj.1, na.rm = TRUE))/2,
                         na.value = "white",
                         limit = c(0, max(adj.1, na.rm = TRUE)), 
                         space = "Lab", name = "link\nweight\n") +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 11.5, 
                                     color = palette.1[sort(clust.1$membership)]),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 11.5, angle = 45, hjust = 1, 
                                     color = palette.1[sort(clust.1$membership)]),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.key.height = unit(1, "in"),
          legend.key.width = unit(0.25, "in"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 14),
          legend.title = element_text(family = "Lucida Console", size = 20),
          plot.margin = grid::unit(c(0.25, 0.25, 0.25, 0.4), "in"))
  
  # Plot heatmap of network 2
  melt.2 <- melt(adj.2.sort, na.rm = FALSE)
  heatmap.2 <- ggplot(data = melt.2, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "gray") + theme_classic() +
    labs(y = paste("nodes grouped by community in network of", net.name.2, "\n"), 
         x = paste("\nnodes grouped by community in network of", net.name.2)) +
    scale_fill_gradient2(low = "white", high = "black", mid = "gray",
                         midpoint = (max(adj.2, na.rm = TRUE) - min(adj.2, na.rm = TRUE))/2,
                         na.value = "white",
                         limit = c(0, max(adj.2, na.rm = TRUE)),
                         space = "Lab", name = "link\nweight\n") +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 11.5, 
                                     color = palette.2[sort(clust.2$membership)]),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 11.5, angle = 45, hjust = 1, 
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
  file.name <- paste(file.name.1, "_COR_", file.name.2, sep = "")
  
  # Save heatmaps
  ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/correlations/", file.name, ".png", sep = ""), heatmap.cor, height = fig.height, width = fig.width, units = "in")
  ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/heatmaps/", file.name.1, ".png", sep = ""), heatmap.1, height = fig.height, width = fig.width, units = "in")
  ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/heatmaps/", file.name.2, ".png", sep = ""), heatmap.2, height = fig.height, width = fig.width, units = "in")
  
}

## COORDS (RAW, PROBABILISTIC, ABSTRACTS, LEMMAS) ##
plot.heatmaps("abstracts_studies_sentences_lemmas_func.csv", "coordinates_experiments_raw_probabilistic_lemmas_func_abstracts.csv", n.1 = 5, n.2 = 5, fig.height = 15, fig.width = 24, remove = c("studies_", "sentences_", "lemmas_", "raw_", "probabilistic_", "experiments_"))
plot.heatmaps("abstracts_studies_sentences_lemmas_anat.csv", "coordinates_experiments_raw_probabilistic_anat.csv", n.1 = 3, n.2 = 3, remove = c("studies_", "sentences_", "lemmas_", "raw_", "probabilistic_", "experiments_"))
plot.heatmaps("texts_studies_sentences_lemmas_anat.csv", "coordinates_experiments_raw_probabilistic_anat.csv", n.1 = 2, n.2 = 3, remove = c("studies_", "sentences_", "lemmas_", "raw_", "probabilistic_", "experiments_"))

## WORDS ONLY ##
plot.heatmaps("texts_studies_sentences_lemmas_func.csv", "abstracts_studies_sentences_lemmas_func.csv", n.1 = 5, n.2 = 5, fig.height = 15, fig.width = 24, remove = c("studies_", "sentences_", "lemmas_"))
plot.heatmaps("texts_studies_sentences_lemmas_anat.csv", "abstracts_studies_sentences_lemmas_anat.csv", n.1 = 2, n.2 = 3, remove = c("studies_", "sentences_", "lemmas_"))
plot.heatmaps("texts_studies_sentences_lemmas_behav.csv", "abstracts_studies_sentences_lemmas_behav.csv", n.1 = 4, n.2 = 4, remove = c("studies_", "sentences_", "lemmas_"))
plot.heatmaps("abstracts_studies_sentences_lemmas_anat.csv", "abstracts_studies_sentences_anat.csv", n.1 = 3, n.2 = 3, remove = c("studies_", "sentences_"))
plot.heatmaps("texts_studies_sentences_lemmas_anat.csv", "texts_studies_sentences_anat.csv", n.1 = 2, n.2 = 3, remove = c("studies_", "sentences_"))

