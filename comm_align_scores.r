# Clear workspace
rm(list=ls())

# Import libraries
library(base)
library(extrafont)
library(ggplot2)
library(igraph)
library(Hmisc)
library(MASS)
library(RColorBrewer)
library(stringr)

# Point working directory to networks
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")

# Set path for plots
plot.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignscores/"
dir.create(plot.path)

# Load network file list
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Set color palette
palette <- c("#9a90b4", # Purple
             "#eaab00", # Yellow
             "#cc3300", # Red
             "#79b0db", # Blue
             "#77dd8c", # Green
             "#df9cc5", # Pink
             "#5a54b6", # Indigo
             "#e1841b", # Orange
             "#58c1b3", # Teal
             "#a98f65", # Brown
             "#ae68ac", # Magenta
             "#727272", # Gray
             "#a0aaf7", # Lilac
             "#fff87c", # Light yellow
             "#a3546e", # Burgundy
             "#ffce9e", # Tan
             "#c37070", # Rose
             brewer.pal(12, "Set3"), 
             brewer.pal(8, "Accent"))

# Colors for barchart of words
palette.words <- c(palette[2], adjustcolor(palette[2], 0.5), palette[3], adjustcolor(palette[3], 0.5))

#### COORDINATES ####

# Function to load igraph from edge list
load.clust <- function(network) {
  file <- read.csv(network)
  dat <- as.matrix(file)
  net <- graph.edgelist(dat[,1:2], directed = FALSE)
  E(net)$weight <- as.numeric(dat[,3])
  net <- delete_edges(net, which(E(net)$weight == 0))
  net <- simplify(net, remove.loops = TRUE)
  clust <- cluster_fast_greedy(net, weights = E(net)$weight)
  return(clust)
}

# Function to sort list of smoothed networks
sorted.sigma <- function(networks) {
  networks.raw <- grep("_raw_", networks, value = TRUE)
  networks.smooth <- grep("_raw_", networks, value = TRUE, invert = TRUE)
  networks.smooth <- gsub("_5mm_", "_05mm_", networks.smooth)
  networks.smooth <- sort(networks.smooth)
  networks.smooth <- gsub("_05mm_", "_5mm_", networks.smooth)
  return(c(networks.raw, networks.smooth))
}

# Function to plot scatterplot of f-score by sigma, stratified by linking strategy, points labeled by type (anat or func)
plot.align.coords <- function(networks, file.name, type) {
  
  # Initialize variables for plotting
  sigmas <- c()
  alignments.abs.bags <- c()
  alignments.tex.bags <- c()
  alignments.abs.wind <- c()
  alignments.tex.wind <- c()
  
  # Load community memberships from abstracts and texts
  clust.abs.bags <- load.clust(paste("abstracts_studies_lemmas_", type, ".csv", sep = ""))
  clust.tex.bags <- load.clust(paste("texts_studies_lemmas_", type, ".csv", sep = ""))
  clust.abs.wind <- load.clust(paste("abstracts_studies_sentences_lemmas_", type, ".csv", sep = ""))
  clust.tex.wind <- load.clust(paste("texts_studies_sentences_lemmas_", type, ".csv", sep = ""))
  
  # Compute data for plotting
  for (network in networks) {
    
    # Update sigma
    sigma <- 0
    if (grepl("gaussian", network)) {sigma <- gsub("(.*gaussian_)|(.)|(mm.*)", "\\2", network)}
    sigmas <- c(sigmas, as.numeric(sigma))
    
    # Load clustering
    clust <- load.clust(network)
    
    # Update alignments
    alignments.abs.bags <- c(alignments.abs.bags, compare(membership(clust), membership(clust.abs.bags), method = "nmi"))
    alignments.tex.bags <- c(alignments.tex.bags, compare(membership(clust), membership(clust.tex.bags), method = "nmi"))
    alignments.abs.wind <- c(alignments.abs.wind, compare(membership(clust), membership(clust.abs.wind), method = "nmi"))
    alignments.tex.wind <- c(alignments.tex.wind, compare(membership(clust), membership(clust.tex.wind), method = "nmi"))
    
  }
  
  # Build data frame
  data <- data.frame(c(sigmas, sigmas, sigmas, sigmas), c(alignments.abs.bags, alignments.tex.bags, alignments.abs.wind, alignments.tex.wind), 
                     c(rep("abstracts", length(networks)), rep("texts", length(networks)), rep("abstracts", length(networks)), rep("texts", length(networks))),
                     c(rep("no window", length(networks)), rep("no window", length(networks)), rep("windowed", length(networks)), rep("windowed", length(networks))))
  colnames(data) <- c("sigma", "alignment", "words", "window")
  print(networks)
  print(data)
  
  # Plot scatter
  plot <- ggplot(data, aes(y = alignment, x = sigma)) +
    theme_classic() +
    geom_point(aes(color = words, shape = window), size = 1.5, stroke = 1.1) +
    scale_color_manual(values = adjustcolor(palette[2:3], 0.75)) +
    scale_shape_manual(values = c(16, 1)) +
    scale_y_continuous(limits = c(0, 0.8), breaks = c(0.15, 0.35, 0.55, 0.75)) +
    theme(legend.position = "none",
          axis.line = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 11),
          axis.title = element_blank(),
          panel.grid.major = element_line(colour = "gray", size = 0.2),
          panel.grid.minor = element_line(colour = "gray", size = 0.2),
          plot.margin = grid::unit(c(0.15, 0.15, 0.15, 0.15), "in"))
  
  # Save plot
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 1.4, width = 2.75, units = "in")
  
  # Export for multiplot
  return(plot)
  
}

### EXPERIMENTS ###

## WINNER-TAKES-ALL ##
networks.wta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_winner-takes-all", networks, value = TRUE))
networks.wta.experiments.anat <- grep("anat", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.texts <- grep("lemmas_func_texts", networks.wta.experiments, value = TRUE)
p1 <- plot.align.coords(networks.wta.experiments.anat, "coords&words_exp_wta_anat", "anat")
p2 <- plot.align.coords(networks.wta.experiments.func.abstracts, "coords&words_exp_wta_func_abs", "func")
p3 <- plot.align.coords(networks.wta.experiments.func.texts, "coords&words_exp_wta_func_tex", "func")

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.experiments.anat <- grep("anat", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.texts <- grep("lemmas_func_texts", networks.pwta.experiments, value = TRUE)
p4 <- plot.align.coords(networks.pwta.experiments.anat, "coords&words_exp_pwta_anat", "anat")
p5 <- plot.align.coords(networks.pwta.experiments.func.abstracts, "coords&words_exp_pwta_func_abs", "func")
p6 <- plot.align.coords(networks.pwta.experiments.func.texts, "coords&words_exp_pwta_func_tex", "func")

## PROBABILISTIC ##
networks.p.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic_", networks, value = TRUE))
networks.p.experiments.anat <- grep("anat", networks.p.experiments, value = TRUE)
networks.p.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.p.experiments, value = TRUE)
networks.p.experiments.func.texts <- grep("lemmas_func_texts", networks.p.experiments, value = TRUE)
p7 <- plot.align.coords(networks.p.experiments.anat, "coords&words_exp_prob_anat", "anat")
p8 <- plot.align.coords(networks.p.experiments.func.abstracts, "coords&words_exp_prob_func_abs", "func")
p9 <- plot.align.coords(networks.p.experiments.func.texts, "coords&words_exp_prob_func_tex", "func")

# Export multiplot
plot.exp <- arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)
ggsave(file = paste(plot.path, "multiplot_coords_exp", ".png", sep = ""), plot.exp, height = 4.5, width = 9, units = "in")


### STUDIES ###

## WINNER-TAKES-ALL ##
networks.wta.studies <- sorted.sigma(grep("coordinates_studies_.*_winner-takes-all", networks, value = TRUE))
networks.wta.studies.anat <- grep("anat", networks.wta.studies, value = TRUE)
networks.wta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.studies, value = TRUE)
networks.wta.studies.func.texts <- grep("lemmas_func_texts", networks.wta.studies, value = TRUE)
p10 <- plot.align.coords(networks.wta.studies.anat, "coords&words_stud_wta_anat", "anat")
p11 <- plot.align.coords(networks.wta.studies.func.abstracts, "coords&words_stud_wta_func_abs", "func")
p12 <- plot.align.coords(networks.wta.studies.func.texts, "coords&words_stud_wta_func_tex", "func")

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.studies.anat <- grep("anat", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.texts <- grep("lemmas_func_texts", networks.pwta.studies, value = TRUE)
p13 <- plot.align.coords(networks.pwta.studies.anat, "coords&words_stud_pwta_anat", "anat")
p14 <- plot.align.coords(networks.pwta.studies.func.abstracts, "coords&words_stud_pwta_func_abs", "func")
p15 <- plot.align.coords(networks.pwta.studies.func.texts, "coords&words_stud_pwta_func_tex", "func")

## PROBABILISTIC ##
networks.p.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic_", networks, value = TRUE))
networks.p.studies.anat <- grep("anat", networks.p.studies, value = TRUE)
networks.p.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.p.studies, value = TRUE)
networks.p.studies.func.texts <- grep("lemmas_func_texts", networks.p.studies, value = TRUE)
p16 <- plot.align.coords(networks.p.studies.anat, "coords&words_stud_prob_anat", "anat")
p17 <- plot.align.coords(networks.p.studies.func.abstracts, "coords&words_stud_prob_func_abs", "func")
p18 <- plot.align.coords(networks.p.studies.func.texts, "coords&words_stud_prob_func_tex", "func")

# Export multiplot
plot.stud <- arrangeGrob(p10, p11, p12, p13, p14, p15, p16, p17, p18, ncol = 3)
ggsave(file = paste(plot.path, "multiplot_coords_stud", ".png", sep = ""), plot.stud, height = 4.5, width = 9, units = "in")


#### WORDS ####

# Function to plot scatterplot of f-score by sigma, stratified by linking strategy, points labeled by type (anat or func)
plot.align.words <- function(networks, file.name) {
  
  # Initialize variables for plotting
  names <- c()
  modularities <- c()
  alignments <- c()
  
  # Compute data for plotting
  for (network in networks) {
    
    # Update name
    name <- file_path_sans_ext(network)
    names <- c(names, name)
    
    # Update modularity
    clust <- load.clust(network)
    modularities <- c(modularities, as.numeric(modularity(clust)))
    
    # Load alignment from complementary network
    if (grepl("abstracts", network)) { network.comp <- gsub("abstracts", "texts", network) }
    if (grepl("texts", network)) { network.comp <- gsub("texts", "abstracts", network) }
    clust.comp <- load.clust(network.comp)
    
    # Update alignments
    alignments <- c(alignments, compare(membership(clust), membership(clust.comp), method = "nmi"))
    
  }
  
  # Compute f-score, the harmonic mean of modularity and alignment
  f <- 2 * modularities * alignments / (modularities + alignments)
  
  # Build data frame
  data <- data.frame(names, f)
  colnames(data) <- c("name", "f")
  
  print(networks)
  print(data)
  
  # Plot barchart
  plot <- ggplot(data, aes(x = factor(name, levels = unique(name)), y = f)) +
    geom_bar(aes(fill = factor(name, levels = unique(name))), position = position_dodge(width = 0.8), stat = "identity") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 0.45)) +
    scale_fill_manual(values = palette.words) +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 20, hjust = 0.5),
          legend.position = "none",
          #legend.position = "bottom",
          #legend.direction = "horizontal",
          #legend.text = element_text(family = "Lucida Sans Unicode", size = 14),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_blank(),
          plot.margin = grid::unit(c(0.05, 0.05, 0.05, 0.05), "in"))
  
  # Save plot
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 1.4, width = 2.75, units = "in")
  
}

# Filter networks
networks.words.sentences.anat <- grep("sentences.*anat", networks, value = TRUE)
networks.words.bags.anat <- grep("sentences", c(grep("abstracts.*anat", networks, value = TRUE), grep("texts.*anat", networks, value = TRUE)), value = TRUE, invert = TRUE)
networks.words.sentences.behav <- c(grep("sentences.*behav", networks, value = TRUE))
networks.words.bags.behav <- c(grep("sentences", c(grep("abstracts.*behav", networks, value = TRUE), grep("texts.*behav", networks, value = TRUE)), value = TRUE, invert = TRUE))
networks.words.sentences.func <- grep("sentences.*func", networks, value = TRUE)
networks.words.bags.func <- grep("sentences", c(grep("abstracts.*func", networks, value = TRUE), grep("texts.*func", networks, value = TRUE)), value = TRUE, invert = TRUE)
plot.align.words(networks.words.bags.anat, "words_bags_anat")
plot.align.words(networks.words.bags.behav, "words_bags_behav")
plot.align.words(networks.words.bags.func, "words_bags_func")
plot.align.words(networks.words.sentences.anat, "words_sentences_anat")
plot.align.words(networks.words.sentences.behav, "words_sentences_behav")
plot.align.words(networks.words.sentences.func, "words_sentences_func")

