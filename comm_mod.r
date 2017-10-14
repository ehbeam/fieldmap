# Clear workspace
rm(list=ls())

# Import libraries
library(base)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(MASS)
library(RColorBrewer)
library(stringr)

# Set path for plots
plot.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/modularity/"
dir.create(plot.path)

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

# Load data
data <- read.csv("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/clusters/community_metrics_fastgreedy.csv")

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


#### WORDS ####

# Plot modularity vs. threshold for words, with lemmas and input labeled, stratified by level
barchart.words.lemmas.mod <- function(data, file.name, colors = palette) {
  NETWORK.lab <- gsub(", func", "", gsub("  ,", ",", gsub("lemmas", "lemmas  ", gsub("texts", "texts  ", gsub("abstracts", "abstracts  ", gsub("brainmap", "brainmap  ", gsub("sentences, ", "", gsub(", behav", "", gsub(", anat", "", gsub("studies, ", "", gsub(".csv", "", gsub("_", ", ", data$NETWORK))))))))))))
  plot <- ggplot(data, aes(x = factor(NETWORK.lab, levels = unique(NETWORK.lab)), y = FAST_GREEDY_Modularity)) +
    geom_bar(aes(fill = factor(NETWORK.lab, levels = unique(NETWORK.lab))), position = position_dodge(width = 0.8), stat = "identity") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 0.4)) +
    guides(fill = guide_legend(title = "lemmas\n")) +
    scale_fill_manual(values = colors) +
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
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 2, width = 2.75, units = "in")
}

# Filter networks
data.words.bags.anat <- subset(data, TYPE == "anatomy" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "all")
data.words.sentences.anat <- subset(data, TYPE == "anatomy" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "sentences")
data.brainmap <- subset(data, NETWORK == "brainmap_behav.csv")
data.words.bags.behav <- rbind(data.brainmap, subset(data, TYPE == "behavior" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "all"))
data.words.sentences.behav <- rbind(data.brainmap, subset(data, TYPE == "behavior" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "sentences"))
data.words.bags.func <- subset(data, TYPE == "function" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "all")
data.words.sentences.func <- subset(data, TYPE == "function" & is.na(THRESHOLD) & INPUT %in% c("abstracts", "brainmap", "texts") & WINDOW == "sentences")

# Execute plotting
colors.anat.func <- c(palette[2], adjustcolor(palette[2], 0.5), palette[3], adjustcolor(palette[3], 0.5))
barchart.words.lemmas.mod(data.words.bags.anat, "words_bags_anat", colors = colors.anat.func)
barchart.words.lemmas.mod(data.words.sentences.anat, "words_sentences_anat", colors = colors.anat.func)
barchart.words.lemmas.mod(data.words.bags.behav, "words_bags_behav", colors = c(palette[4], colors.anat.func))
barchart.words.lemmas.mod(data.words.sentences.behav, "words_sentences_behav", colors = c(palette[4], colors.anat.func))
barchart.words.lemmas.mod(data.words.bags.func, "words_bags_func", colors = colors.anat.func)
barchart.words.lemmas.mod(data.words.sentences.func, "words_sentences_func", colors = colors.anat.func)


#### COORDS ####

# Load network file list
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Function to sort list of smoothed networks
sorted.sigma <- function(networks) {
  networks.raw <- grep("_raw_", networks, value = TRUE)
  networks.smooth <- grep("_raw_", networks, value = TRUE, invert = TRUE)
  networks.smooth <- gsub("_5mm_", "_05mm_", networks.smooth)
  networks.smooth <- sort(networks.smooth)
  networks.smooth <- gsub("_05mm_", "_5mm_", networks.smooth)
  return(c(networks.raw, networks.smooth))
}

# Function to plot scatterplot of modularity by sigma
plot.modularity.by.sigma <- function(networks, file.name, color = palette[1]) {

  lines = c()
  
  # Load modularity from abstracts and texts
  if (grepl("anat", networks[1])) {
    lines <- c(lines, modularity(load.clust("abstracts_studies_lemmas_anat.csv")))
    lines <- c(lines, modularity(load.clust("abstracts_studies_sentences_lemmas_anat.csv")))
    lines <- c(lines, modularity(load.clust("texts_studies_lemmas_anat.csv")))
    lines <- c(lines, modularity(load.clust("texts_studies_sentences_lemmas_anat.csv")))
  }
  if (grepl("func_texts", networks[1])) {
    lines <- c(lines, modularity(load.clust("texts_studies_lemmas_func.csv")))
    lines <- c(lines, modularity(load.clust("texts_studies_sentences_lemmas_func.csv")))
  }
  if (grepl("func_abstracts", networks[1])) {
    lines <- c(lines, modularity(load.clust("abstracts_studies_lemmas_func.csv")))
    lines <- c(lines, modularity(load.clust("abstracts_studies_sentences_lemmas_func.csv")))
  }
  
  # Set palette for lines
  if (grepl("func_texts", networks[1])) {
    palette.lines <- c(palette[3], adjustcolor(palette[3], 0.5))
  }
  else {
    palette.lines <- c(palette[2], palette[2], palette[3], palette[3])
  }
  
  # Initialize variables for plotting
  sigmas <- c()
  modularities <- c()
  
  for (network in networks) {
    
    # Update sigma
    sigma <- 0
    if (grepl("gaussian", network)) {sigma <- gsub("(.*gaussian_)|(.)|(mm.*)", "\\2", network)}
    sigmas <- c(sigmas, as.numeric(sigma))
    
    # Update modularity
    clust <- load.clust(network)
    modularities <- c(modularities, as.numeric(modularity(clust)))
  }
  
  # Build data frame
  data <- data.frame(sigmas, modularities)
  colnames(data) <- c("sigma", "modularity")
  
  # Plot scatter
  plot <- ggplot(data, aes(y = modularity, x = sigma)) +   
    theme_classic() +
    geom_point(size = 1.75, color = color) +
    scale_y_continuous(limits = c(-.001, 0.31), breaks = c(0.05, 0.15, 0.25)) +
    scale_x_continuous(limits = c(0, 50)) +
    theme(legend.position = "none",
          axis.line = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 11),
          axis.title = element_blank(),
          #panel.grid.major = element_line(colour = "gray", size = 0.2),
          #panel.grid.minor = element_line(colour = "gray", size = 0.2),
          plot.margin = grid::unit(c(0.15, 0.15, 0.15, 0.15), "in"))
    for (i in 1:length(lines)) {
      line.type <- 1
      if (i %% 2 == 0) { line.type <- 2 }
      plot <- plot + geom_hline(yintercept = lines[i], alpha = 0.75, lty = line.type, color = palette.lines[i])
    }
  
  # Save plot
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 1.4, width = 2.75, units = "in")

  # Return plot for multiplot
  return(plot)
  
}

# Plot modularity vs. threshold for coordinates, with smoothing kernel on x axis, stratified by linking strategy

### EXPERIMENTS ###

# Winner-takes-all
networks.wta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_winner-takes-all", networks, value = TRUE))
networks.wta.experiments.anat <- grep("anat", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.texts <- grep("lemmas_func_texts", networks.wta.experiments, value = TRUE)
p1 <- plot.modularity.by.sigma(networks.wta.experiments.anat, "coords_exp_wta_anat", color = "gray40")
p2 <- plot.modularity.by.sigma(networks.wta.experiments.func.abstracts, "coords_exp_wta_func_abstracts", color = "gray40")
p3 <- plot.modularity.by.sigma(networks.wta.experiments.func.texts, "coords_exp_wta_func_texts", color = "gray40")

# Probabilistic winner-takes-all
networks.pwta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.experiments.anat <- grep("anat", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.texts <- grep("lemmas_func_texts", networks.pwta.experiments, value = TRUE)
p4 <- plot.modularity.by.sigma(networks.pwta.experiments.anat, "coords_exp_pwta_anat", color = "gray40")
p5 <- plot.modularity.by.sigma(networks.pwta.experiments.func.abstracts, "coords_exp_pwta_func_abstracts", color = "gray40")
p6 <- plot.modularity.by.sigma(networks.pwta.experiments.func.texts, "coords_exp_pwta_func_texts", color = "gray40")

# Probabilistic
networks.p.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic_", networks, value = TRUE))
networks.p.experiments.anat <- grep("anat", networks.p.experiments, value = TRUE)
networks.p.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.p.experiments, value = TRUE)
networks.p.experiments.func.texts <- grep("lemmas_func_texts", networks.p.experiments, value = TRUE)
p7 <- plot.modularity.by.sigma(networks.p.experiments.anat, "coords_exp_prob_anat", color = "gray40")
p8 <- plot.modularity.by.sigma(networks.p.experiments.func.abstracts, "coords_exp_prob_func_abstracts", color = "gray40")
p9 <- plot.modularity.by.sigma(networks.p.experiments.func.texts, "coords_exp_prob_func_texts", color = "gray40")

# Export multiplot
plot.exp <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
ggsave(file = paste(plot.path, "multiplot_coords_exp", ".png", sep = ""), plot, height = 7, width = 10, units = "in")

### STUDIES ###

# Winner-takes-all
networks.wta.studies <- sorted.sigma(grep("coordinates_studies_.*_winner-takes-all", networks, value = TRUE))
networks.wta.studies.anat <- grep("anat", networks.wta.studies, value = TRUE)
networks.wta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.studies, value = TRUE)
networks.wta.studies.func.texts <- grep("lemmas_func_texts", networks.wta.studies, value = TRUE)
p10 <- plot.modularity.by.sigma(networks.wta.studies.anat, "coords_stud_wta_anat", color = "gray40")
p11 <- plot.modularity.by.sigma(networks.wta.studies.func.abstracts, "coords_stud_wta_func_abstracts", color = "gray40")
p12 <- plot.modularity.by.sigma(networks.wta.studies.func.texts, "coords_stud_wta_func_texts", color = "gray40")

# Probabilistic winner-takes-all
networks.pwta.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.studies.anat <- grep("anat", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.texts <- grep("lemmas_func_texts", networks.pwta.studies, value = TRUE)
p13 <- plot.modularity.by.sigma(networks.pwta.studies.anat, "coords_stud_pwta_anat", color = "gray40")
p14 <- plot.modularity.by.sigma(networks.pwta.studies.func.abstracts, "coords_stud_pwta_func_abstracts", color = "gray40")
p15 <- plot.modularity.by.sigma(networks.pwta.studies.func.texts, "coords_stud_pwta_func_texts", color = "gray40")

# Probabilistic
networks.p.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic_", networks, value = TRUE))
networks.p.studies.anat <- grep("anat", networks.p.studies, value = TRUE)
networks.p.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.p.studies, value = TRUE)
networks.p.studies.func.texts <- grep("lemmas_func_texts", networks.p.studies, value = TRUE)
p16 <- plot.modularity.by.sigma(networks.p.studies.anat, "coords_stud_prob_anat", color = "gray40")
p17 <- plot.modularity.by.sigma(networks.p.studies.func.abstracts, "coords_stud_prob_func_abstracts", color = "gray40")
p18 <- plot.modularity.by.sigma(networks.p.studies.func.texts, "coords_stud_prob_func_texts", color = "gray40")

# Export multiplot
plot.stud <- grid.arrange(p10, p11, p12, p13, p14, p15, p16, p17, p18, cols = 3)
ggsave(file = paste(plot.path, "multiplot_coords_exp", ".png", sep = ""), plot, height = 7, width = 10, units = "in")
