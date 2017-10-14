# Clear workspace
rm(list=ls())

# Import libraries
library(ggplot2)
library(reshape2)

# Set working directory
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/clusters/nodes"
setwd(path)

# Load data
data <- read.csv(paste(path, "/community_metrics_fastgreedy.csv", sep = ""), header = TRUE, row.names = 1)
nodes <- tail(colnames(data), 102)

# Function for aligning two networks
align <- function(net.1, net.2, data, nodes) {
  
  # Load community memberships
  print(paste(net.1,net.2))
  rows <- data[c(net.1, net.2),]
  rows.nodes <- t(rows[,nodes])
  
  # Group the most similar clusters
  max.net.1 <- max(rows.nodes[,1], na.rm = TRUE)
  max.net.2 <- max(rows.nodes[,2], na.rm = TRUE)
  match.mat <- matrix(nrow = max.net.1, ncol = max.net.2)
  for (i in 1:max.net.1) {
    for (j in 1:max.net.2) {
      nodes.net.1 <- names(subset(rows.nodes[,1], rows.nodes[,1] == i))
      nodes.net.2 <- names(subset(rows.nodes[,2], rows.nodes[,2] == j))
      matches <- sum(!is.na(match(nodes.net.1, nodes.net.2)))
      match.mat[i,j] = matches
    }
  }
  print(match.mat)
  num.matches <- 0
  num.nodes <- 0
  if (dim(match.mat)[1] >= dim(match.mat)[2]) {
    for (i in 1:ncol(match.mat)) {
      num.matches <- sum(num.matches, max(match.mat[,i]))
      max.row <- which(match.mat[,i] == max(match.mat[,i]), arr.ind = TRUE)
      num.nodes.1 <- length(which(rows.nodes[,1] == max.row))
      num.nodes.2 <- length(which(rows.nodes[,2] == i))
      num.nodes <- sum(num.nodes, num.nodes.1, num.nodes.2)
    }
  }
  if (dim(match.mat)[2] > dim(match.mat)[1]) {
    for (i in 1:nrow(match.mat)) {
      num.matches <- sum(num.matches, max(match.mat[i,]))
      max.col <- which(match.mat[i,] == max(match.mat[i,]), arr.ind = TRUE)
      num.nodes.1 <- length(which(rows.nodes[,1] == i))
      num.nodes.2 <- length(which(rows.nodes[,2] == max.col))
      num.nodes <- sum(num.nodes, num.nodes.1, num.nodes.2)
    }
  }
  
  # Compute alignment
  num.matched.nodes <- 2 * num.matches
  alignment <- num.matched.nodes / num.nodes
  print(paste("alignment:", alignment))
  print("--------------------")
  return(alignment)
}

# Function for converting network names to figure labels
presentable <- function(networks) {
  net.names <- c()
  for (net in networks) {
    name <- file_path_sans_ext(net)
    name <- gsub("_", " ", name)
    name <- gsub("sentences ", "", name)
    name <- gsub("experiments ", "", name)
    name <- gsub("studies ", "", name)
    name <- gsub("gaussian ", "", name)
    name <- gsub("anat", "", name)
    name <- gsub("behav", "", name)
    name <- gsub("func", "", name)
    name <- gsub("^\\s+|\\s+$", "", name)
    name <- gsub(" ", ", ", name)
    name <- gsub(", , ", ", ", name)
    net.names <- c(net.names, name)
  }
  return(net.names)
}

## WORDS AND COORDS ##

# Load networks by type
networks <- c(grep("thres", rownames(data), value = TRUE, invert = TRUE))
networks.words <- c(grep("brainmap_behav", networks, value = TRUE), grep("abstracts_studies", networks, value = TRUE), grep("texts_studies", networks, value = TRUE))
networks.raw <- grep("raw", networks, value = TRUE)
networks.gaussian <- sort(gsub("_5mm", "_05mm", grep("gaussian", networks, value = TRUE)))
networks.sorted <- c(networks.words, networks.raw, networks.gaussian)
networks.anat <- gsub("05", "5", grep("anat", networks.sorted, value = TRUE))
networks.func <- gsub("05", "5", grep("func", networks.sorted, value = TRUE))
networks.func.coords.brainmap <- grep("func_brainmap", networks.func, value = TRUE)
networks.func.coords.abstracts <- grep("func_abstracts", networks.func, value = TRUE)
networks.func.coords.texts <- grep("func_texts", networks.func, value = TRUE)
networks.func.abstracts <- grep("abstracts_studies", networks.func, value = TRUE)
networks.func.texts <- grep("texts_studies", networks.func, value = TRUE)
networks.func <- c(networks.func.abstracts, networks.func.texts, networks.func.coords.brainmap, networks.func.coords.abstracts, networks.func.coords.texts)

# Load names for alignment matrices
net.names.anat <- presentable(networks.anat)
net.names.func <- presentable(networks.func)

# ANATOMICAL: Heat map of alignments
alignments.anat <- matrix(nrow = length(networks.anat), ncol = length(networks.anat))
colnames(alignments.anat) <- net.names.anat
rownames(alignments.anat) <- net.names.anat
for (i in 1:length(networks.anat)) {
  for (j in 1:length(networks.anat)) {
    alignments.anat[i,j] <- align(networks.anat[i], networks.anat[j], data, nodes)
  }
}
melt.anat <- melt(alignments.anat)
heatmap.anat <- ggplot(data = melt.anat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "community\nalignment\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 10),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/alignment_anat.png", heatmap.anat, height = 8, width = 10, units = "in")

# FUNCTIONAL: Heat map of alignments
alignments.func <- matrix(nrow = length(networks.func), ncol = length(networks.func))
colnames(alignments.func) <- net.names.func
rownames(alignments.func) <- net.names.func
for (i in 1:length(networks.func)) {
  for (j in 1:length(networks.func)) {
    alignments.func[i,j] <- align(networks.func[i], networks.func[j], data, nodes)
  }
}
melt.func <- melt(alignments.func)
heatmap.func <- ggplot(data = melt.func, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "community\nalignment\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 8),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/alignment_func.png", heatmap.func, height = 10, width = 14, units = "in")

## WORDS ONLY ##

# Load networks by type
networks.behav <- grep("behav", networks.sorted, value = TRUE)
networks.words.anat <- grep("anat", networks.words, value = TRUE)
networks.words.func <- grep("func", networks.words, value = TRUE)

# Load names for alignment matrices
net.names.words.anat <- presentable(networks.words.anat)
net.names.behav <- presentable(networks.behav)
net.names.words.func <- presentable(networks.words.func)

# ANATOMICAL: Heat map of alignments
alignments.words.anat <- matrix(nrow = length(networks.words.anat), ncol = length(networks.words.anat))
colnames(alignments.words.anat) <- net.names.words.anat
rownames(alignments.words.anat) <- net.names.words.anat
for (i in 1:length(networks.words.anat)) {
  for (j in 1:length(networks.words.anat)) {
    alignments.words.anat[i,j] <- align(networks.words.anat[i], networks.words.anat[j], data, nodes)
  }
}
melt.words.anat <- melt(alignments.words.anat)
heatmap.words.anat <- ggplot(data = melt.words.anat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "community\nalignment\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 10),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/alignment_words_anat.png", heatmap.words.anat, height = 3, width = 4.5, units = "in")

# BEHAVIORAL: Heat map of alignments
alignments.behav <- matrix(nrow = length(networks.behav), ncol = length(networks.behav))
colnames(alignments.behav) <- net.names.behav
rownames(alignments.behav) <- net.names.behav
for (i in 1:length(networks.behav)) {
  for (j in 1:length(networks.behav)) {
    alignments.behav[i,j] <- align(networks.behav[i], networks.behav[j], data, nodes)
  }
}
melt.behav <- melt(alignments.behav)
heatmap.behav <- ggplot(data = melt.behav, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "community\nalignment\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 10),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/alignment_words_behav.png", heatmap.behav, height = 3, width = 4.5, units = "in")

# FUNCTIONAL: Heat maps of alignment
alignments.words.func <- matrix(nrow = length(networks.words.func), ncol = length(networks.words.func))
colnames(alignments.words.func) <- net.names.words.func
rownames(alignments.words.func) <- net.names.words.func
for (i in 1:length(networks.words.func)) {
  for (j in 1:length(networks.words.func)) {
    alignments.words.func[i,j] <- align(networks.words.func[i], networks.words.func[j], data, nodes)
  }
}
melt.words.func <- melt(alignments.words.func)
heatmap.words.func <- ggplot(data = melt.words.func, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name="community\nalignment\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14, hjust = -1),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 10),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        #legend.position = "bottom",
        #legend.key.size = unit(2, "cm"),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/alignment_words_func.png", heatmap.words.func, height = 3, width = 4.5, units = "in")

