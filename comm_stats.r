# Clear workspace
rm(list=ls())

# Import libraries
library(base)
library(extrafont)
library(ggplot2)
library(Hmisc)
library(MASS)
library(RColorBrewer)
library(stringr)

# Color palettes
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
path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
data <- read.csv(paste(path, "/clusters/nodes/community_metrics_fastgreedy.csv", sep = ""))

# Subset data by type (anatomy, behavior, function)
data.unthres <- subset(data, THRESHOLD == NA)
data.anat <- data[data$TYPE == "anatomy",]
data.behav <- data[data$TYPE == "behavior",]
data.func <- data[data$TYPE == "function",]

# Load variables by type
input.anat <- as.factor(data.anat$INPUT)
strategy.anat <- as.factor(data.anat$STRATEGY)
sigma.anat <- as.numeric(data.anat$SIGMA)
sigma.vis.anat <- as.numeric(data.anat$SIGMA_VIS)
nodes.anat <- as.numeric(data.anat$NODES)
edges.anat <- as.numeric(data.anat$EDGES)
threshold.anat <- as.numeric(data.anat$THRESHOLD)
number.clusters.anat <- as.numeric(data.anat$FAST_GREEDY_Communities)
modularity.anat <- as.numeric(data.anat$FAST_GREEDY_Modularity)
input.behav <- as.factor(data.behav$INPUT)
strategy.behav <- as.factor(data.behav$STRATEGY)
sigma.behav <- as.numeric(data.behav$SIGMA)
sigma.vis.behav <- as.numeric(data.behav$SIGMA_VIS)
nodes.behav <- as.numeric(data.behav$NODES)
edges.behav <- as.numeric(data.behav$EDGES)
threshold.behav <- as.numeric(data.behav$THRESHOLD)
number.clusters.behav <- as.numeric(data.behav$FAST_GREEDY_Communities)
modularity.behav <- as.numeric(data.behav$FAST_GREEDY_Modularity)
input.func <- as.factor(data.func$INPUT)
strategy.func <- as.factor(data.func$STRATEGY)
sigma.func <- as.numeric(data.func$SIGMA)
sigma.vis.func <- as.numeric(data.func$SIGMA_VIS)
nodes.func <- as.numeric(data.func$NODES)
edges.func <- as.numeric(data.func$EDGES)
threshold.func <- as.numeric(data.func$THRESHOLD)
number.clusters.func <- as.numeric(data.func$FAST_GREEDY_Communities)
modularity.func <- as.numeric(data.func$FAST_GREEDY_Modularity)

# Subset data by input (coords, abstracts, texts)
INPUT_RAW <- as.factor(data$INPUT_RAW)
data.coords.anat <- data.anat[which(data.anat$INPUT_RAW == "coords"),]
data.coords.func <- data.func[which(data.func$INPUT_RAW == "coords"),]
data.coords.func.abstracts <- data.func[which(data.coords.func$BEHAV_INPUT == "abstracts"),]
data.coords.func.brainmap <- data.func[which(data.coords.func$BEHAV_INPUT == "brainmap"),]
data.coords.func.texts <- data.func[which(data.coords.func$BEHAV_INPUT == "texts"),]
data.words.anat <- data.anat[which(data.anat$INPUT_RAW != "coords"),]
data.words.behav <- data.behav[which(data.behav$INPUT_RAW != "coords"),]
data.words.func <- data.func[which(data.func$INPUT_RAW != "coords"),]

# Load variables by type for coords
input.coords.anat <- as.factor(data.coords.anat$INPUT)
level.coords.anat <- as.factor(data.coords.anat$LEVEL)
strategy.coords.anat <- as.factor(data.coords.anat$STRATEGY)
sigma.coords.anat <- as.numeric(data.coords.anat$SIGMA)
sigma.vis.coords.anat <- as.numeric(data.coords.anat$SIGMA_VIS)
nodes.coords.anat <- as.numeric(data.coords.anat$NODES)
edges.coords.anat <- as.numeric(data.coords.anat$EDGES)
threshold.coords.anat <- as.numeric(data.coords.anat$THRESHOLD)
number.clusters.coords.anat <- as.numeric(data.coords.anat$FAST_GREEDY_Communities)
modularity.coords.anat <- as.numeric(data.coords.anat$FAST_GREEDY_Modularity)
input.coords.func <- as.factor(data.coords.func$INPUT)
level.coords.func <- as.factor(data.coords.func$LEVEL)
strategy.coords.func <- as.factor(data.coords.func$STRATEGY)
sigma.coords.func <- as.numeric(data.coords.func$SIGMA)
sigma.vis.coords.func <- as.numeric(data.coords.func$SIGMA_VIS)
nodes.coords.func <- as.numeric(data.coords.func$NODES)
edges.coords.func <- as.numeric(data.coords.func$EDGES)
threshold.coords.func <- as.numeric(data.coords.func$THRESHOLD)
number.clusters.coords.func <- as.numeric(data.coords.func$FAST_GREEDY_Communities)
modularity.coords.func <- as.numeric(data.coords.func$FAST_GREEDY_Modularity)
input.coords.func.abstracts <- as.factor(data.coords.func.abstracts$INPUT)
level.coords.func.abstracts <- as.factor(data.coords.func.abstracts$LEVEL)
strategy.coords.func.abstracts <- as.factor(data.coords.func.abstracts$STRATEGY)
sigma.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$SIGMA)
sigma.vis.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$SIGMA_VIS)
nodes.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$NODES)
edges.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$EDGES)
threshold.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$THRESHOLD)
number.clusters.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$FAST_GREEDY_Communities)
modularity.coords.func.abstracts <- as.numeric(data.coords.func.abstracts$FAST_GREEDY_Modularity)
input.coords.func.brainmap <- as.factor(data.coords.func.brainmap$INPUT)
level.coords.func.brainmap <- as.factor(data.coords.func.brainmap$LEVEL)
strategy.coords.func.brainmap <- as.factor(data.coords.func.brainmap$STRATEGY)
sigma.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$SIGMA)
sigma.vis.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$SIGMA_VIS)
nodes.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$NODES)
edges.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$EDGES)
threshold.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$THRESHOLD)
number.clusters.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$FAST_GREEDY_Communities)
modularity.coords.func.brainmap <- as.numeric(data.coords.func.brainmap$FAST_GREEDY_Modularity)
input.coords.func.texts <- as.factor(data.coords.func.texts$INPUT)
level.coords.func.texts <- as.factor(data.coords.func.texts$LEVEL)
strategy.coords.func.texts <- as.factor(data.coords.func.texts$STRATEGY)
sigma.coords.func.texts <- as.numeric(data.coords.func.texts$SIGMA)
sigma.vis.coords.func.texts <- as.numeric(data.coords.func.texts$SIGMA_VIS)
nodes.coords.func.texts <- as.numeric(data.coords.func.texts$NODES)
edges.coords.func.texts <- as.numeric(data.coords.func.texts$EDGES)
threshold.coords.func.texts <- as.numeric(data.coords.func.texts$THRESHOLD)
number.clusters.coords.func.texts <- as.numeric(data.coords.func.texts$FAST_GREEDY_Communities)
modularity.coords.func.texts <- as.numeric(data.coords.func.texts$FAST_GREEDY_Modularity)

# Load variables by type for words
input.words.anat <- as.factor(data.words.anat$INPUT)
lemmas.words.anat <- as.factor(data.words.anat$LEMMAS)
strategy.words.anat <- as.factor(data.words.anat$STRATEGY)
sigma.words.anat <- as.numeric(data.words.anat$SIGMA)
sigma.vis.words.anat <- as.numeric(data.words.anat$SIGMA_VIS)
nodes.words.anat <- as.numeric(data.words.anat$NODES)
edges.words.anat <- as.numeric(data.words.anat$EDGES)
threshold.words.anat <- as.numeric(data.words.anat$THRESHOLD)
number.clusters.words.anat <- as.numeric(data.words.anat$FAST_GREEDY_Communities)
modularity.words.anat <- as.numeric(data.words.anat$FAST_GREEDY_Modularity)
input.words.behav <- as.factor(data.words.behav$INPUT)
lemmas.words.behav <- as.factor(data.words.behav$LEMMAS)
strategy.words.behav <- as.factor(data.words.behav$STRATEGY)
sigma.words.behav <- as.numeric(data.words.behav$SIGMA)
sigma.vis.words.behav <- as.numeric(data.words.behav$SIGMA_VIS)
nodes.words.behav <- as.numeric(data.words.behav$NODES)
edges.words.behav <- as.numeric(data.words.behav$EDGES)
threshold.words.behav <- as.numeric(data.words.behav$THRESHOLD)
number.clusters.words.behav <- as.numeric(data.words.behav$FAST_GREEDY_Communities)
modularity.words.behav <- as.numeric(data.words.behav$FAST_GREEDY_Modularity)
input.words.func <- as.factor(data.words.func$INPUT)
lemmas.words.func <- as.factor(data.words.func$LEMMAS)
strategy.words.func <- as.factor(data.words.func$STRATEGY)
sigma.words.func <- as.numeric(data.words.func$SIGMA)
sigma.vis.words.func <- as.numeric(data.words.func$SIGMA_VIS)
nodes.words.func <- as.numeric(data.words.func$NODES)
edges.words.func <- as.numeric(data.words.func$EDGES)
threshold.words.func <- as.numeric(data.words.func$THRESHOLD)
number.clusters.words.func <- as.numeric(data.words.func$FAST_GREEDY_Communities)
modularity.words.func <- as.numeric(data.words.func$FAST_GREEDY_Modularity)

# Create directory for cluster plots
plot.path <- paste(path, "/plots/clusters", sep = "")
dir.create(paste(path, "/plots", sep = ""))
dir.create(plot.path)
setwd(plot.path)

# Barchart of modularity, bars labeled by lemmas and inputs, stratified by windowing
data.words.bags.anat <- subset(data, data$TYPE == "anatomy" & data$INPUT %in% c("abstracts", "brainmap", "texts"))
data.words.sentences.anat
barchart.words.lemmas. <- function(data, level, title, filename) {
  plot <- ggplot(data, aes(x = threshold, y = modularity)) +
    theme_classic() +
    labs(x = "\nthreshold for edges", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(lemmas), shape = factor(input)), size = 2.5, stroke = 1.2) +
    guides(color = guide_legend("\nlemmas\n"), shape = guide_legend("\ninput\n"), size = guide_legend("\nsigma\n")) +
    scale_x_continuous(limits = c(0, xlim), breaks = c(25, 50, 100, 200, 400, 800, 1600)) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_colour_manual(values = adjustcolor(c(palette[3], palette[2]), 0.85)) +
    scale_shape_manual(values = shapes) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 16),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 5, width = 14, units = "in") 
}
scatter.words.lemmas.thres(data.words.anat, threshold.words.anat, modularity.words.anat, lemmas.words.anat, input.words.anat, "anatomical network\n(abstracts & full texts)", "/anat_mod_thres_lemmas_words.png")
scatter.words.lemmas.thres(data.words.behav, threshold.words.behav, modularity.words.behav, lemmas.words.behav, input.words.behav, "behavioral network\n(abstracts, brainmap, & full texts)", "/behav_mod_thres_lemmas_words.png")
scatter.words.lemmas.thres(data.words.func, threshold.words.func, modularity.words.func, lemmas.words.func, input.words.func, "functional network\n(abstracts & full texts)", "/func_mod_thres_lemmas_words.png")

# Function for ANATOMICAL scatterplot of modularity vs. threshold, points labeled by level and input
scatter.coords.level.thres <- function(data, threshold, modularity, level, strategy, input, sigma.vis, title, filename, xlim = 1600, ylim = 0.35) {
  plot <- ggplot(data, aes(x = threshold, y = modularity)) +
    theme_classic() +
    labs(x = "\nthreshold for edges", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(level), shape = factor(strategy), size = sigma.vis), stroke = 1.2) +
    guides(color = guide_legend("\nlevel\n"), shape = guide_legend("\nstrategy\n"), size = guide_legend("\nsigma\n"), stroke = guide_legend("\nstrategy\n")) +
    scale_x_continuous(limits = c(0, xlim), breaks = c(25, 50, 100, 200, 400, 800, 1600)) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 6)) +
    scale_size(range = c(1, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 16),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 10, width = 12, units = "in") 
}
scatter.coords.level.thres(data.coords.anat, threshold.coords.anat, modularity.coords.anat, level.coords.anat, strategy.coords.anat, input.coords.anat, sigma.vis.coords.anat, "anatomical network\n(coordinates)", "/anat_mod_thres_sigma_level_coords.png", ylim = 0.22)

# Function for FUNCTIONAL scatterplot of modularity vs. threshold, points labeled by level and input
scatter.coords.level.thres <- function(data, threshold, modularity, level, strategy, input, sigma.vis, title, filename, xlim = 1600, ylim = 0.35) {
  plot <- ggplot(data, aes(x = threshold, y = modularity)) +
    theme_classic() +
    labs(x = "\nthreshold for edges", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(level), shape = factor(strategy), size = sigma.vis), stroke = 1.2) +
    guides(color = guide_legend("\nlevel\n"), shape = guide_legend("\nstrategy\n"), size = guide_legend("\nsigma\n"), stroke = guide_legend("\nstrategy\n")) +
    scale_x_continuous(limits = c(0, xlim), breaks = c(25, 50, 100, 200, 400, 800, 1600)) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 6, 3)) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 13, angle = 45, hjust = 1),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 16),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 10, width = 12, units = "in") 
}

scatter.coords.level.thres(data.coords.func, threshold.coords.func, modularity.coords.func, level.coords.func, strategy.coords.func, input.coords.func, sigma.vis.coords.func, "functional network\n(coordinates)", "/func_mod_thres_sigma_level_coords.png")
scatter.coords.level.thres(data.coords.func.abstracts, threshold.coords.func.abstracts, modularity.coords.func.abstracts, level.coords.func.abstracts, strategy.coords.func.abstracts, input.coords.func.abstracts, sigma.vis.coords.func.abstracts, "functional network\n(coordinates & abstracts)", "/func_abstracts_mod_thres_sigma_level_coords.png")
scatter.coords.level.thres(data.coords.func.brainmap, threshold.coords.func.brainmap, modularity.coords.func.brainmap, level.coords.func.brainmap, strategy.coords.func.brainmap, input.coords.func.brainmap, sigma.vis.coords.func.brainmap, "functional network\n(coordinates & brainmap)", "/func_brainmap_mod_thres_sigma_level_coords.png")
scatter.coords.level.thres(data.coords.func.texts, threshold.coords.func.texts, modularity.coords.func.texts, level.coords.func.texts, strategy.coords.func.texts, input.coords.func.texts, sigma.vis.coords.func.texts, "functional network\n(coordinates & texts)", "/func_texts_mod_thres_sigma_level_coords.png")

# Function for scatterplot of modularity vs. threshold, points labeled by strategy and input
scatter.thres <- function(data, threshold, modularity, strategy, input, sigma.vis, title, filename, ylim = 0.375) {
  plot <- ggplot(data, aes(x = threshold, y = modularity)) +
    theme_classic() +
    labs(x = "\nthreshold for edges", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(strategy), shape = factor(input), size = sigma.vis), stroke = 1.2) +
    guides(color = guide_legend("\nstrategy\n"), shape = guide_legend("\ninput\n"), size = guide_legend("\nsigma\n")) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 5, 0, 6, 3, 4, 8)) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 8, width = 12, units = "in") 
}

scatter.thres(data.anat, threshold.anat, modularity.anat, strategy.anat, input.anat, sigma.vis.anat, "anatomical network", "/anat_mod_thres.png", ylim = 0.25)
scatter.thres(data.func, threshold.func, modularity.func, strategy.func, input.func, sigma.vis.func, "functional network", "/func_mod_thres.png")
scatter.thres(data.coords.anat, threshold.coords.anat, modularity.coords.anat, strategy.coords.anat, input.coords.anat, sigma.vis.coords.anat, "anatomical network\n(coordinates)", "/anat_mod_thres_sigma_coords.png", ylim = 0.25)
scatter.thres(data.coords.func, threshold.coords.func, modularity.coords.func, strategy.coords.func, input.coords.func, sigma.vis.coords.func, "functional network\n(coordinates)", "/func_mod_thres_sigma_coords.png")
scatter.thres(data.words.anat, threshold.words.anat, modularity.words.anat, strategy.words.anat, input.words.anat, sigma.vis.words.anat, "anatomical network\n(abstracts & full texts)", "/anat_mod_thres_sigma_words.png", ylim = 0.25)
scatter.thres(data.words.behav, threshold.words.behav, modularity.words.behav, strategy.words.behav, input.words.behav, sigma.vis.words.behav, "behavioral network\n(abstracts & full texts)", "/behav_mod_thres_sigma_words.png")
scatter.thres(data.words.func, threshold.words.func, modularity.words.func, strategy.words.func, input.words.func, sigma.vis.words.func, "functional network\n(abstracts & full texts)", "/func_mod_thres_sigma_words.png")

# Function for scatterplot of modularity vs. sigma, points labeled by strategy and input
scatter.sigma <- function(data, threshold, modularity, strategy, input, sigma, title, filename, ylim = 0.375) {
  plot <- ggplot(data, aes(x = sigma, y = modularity)) +
    theme_classic() +
    labs(x = "\nsigma for gaussian smoothing", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(strategy), shape = factor(input), size = threshold), stroke = 1.2) +
    guides(color = guide_legend("\nstrategy\n"), shape = guide_legend("\ninput\n"), size = guide_legend("\nthreshold\n")) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_x_continuous(limits = c(-0.01, 55)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 5, 0, 6, 3, 4, 8)) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 8, width = 12, units = "in") 
}

scatter.sigma(data.anat, threshold.anat, modularity.anat, strategy.anat, input.anat, sigma.vis.anat, "anatomical network", "/anat_mod_sigma.png", ylim = 0.25)
scatter.sigma(data.func, threshold.func, modularity.func, strategy.func, input.func, sigma.vis.func, "functional network", "/func_mod_sigma.png")
scatter.sigma(data.coords.anat, threshold.coords.anat, modularity.coords.anat, strategy.coords.anat, input.coords.anat, sigma.vis.coords.anat, "anatomical network\n(coordinates)", "/anat_mod_sigma_thres_coords.png", ylim = 0.25)
scatter.sigma(data.coords.func, threshold.coords.func, modularity.coords.func, strategy.coords.func, input.coords.func, sigma.vis.coords.func, "functional network\n(coordinates)", "/func_mod_sigma_thres_coords.png")
scatter.sigma(data.words.anat, threshold.words.anat, modularity.words.anat, strategy.words.anat, input.words.anat, sigma.vis.words.anat, "anatomical network\n(abstracts & full texts)", "/anat_mod_sigma_thres_words.png", ylim = 0.25)
scatter.sigma(data.words.behav, threshold.words.behav, modularity.words.behav, strategy.words.behav, input.words.behav, sigma.vis.words.behav, "behavioral network\n(abstracts & full texts)", "/behav_mod_sigma_thres_words.png")
scatter.sigma(data.words.func, threshold.words.func, modularity.words.func, strategy.words.func, input.words.func, sigma.vis.words.func, "functional network\n(abstracts & full texts)", "/func_mod_sigma_thres_words.png")


# Function for scatterplot of modularity vs. edges, points labeled by strategy and input
scatter.edges <- function(data, edges, modularity, strategy, input, sigma.vis, title, filename, ylim = 0.375) {
  plot <- ggplot(data, aes(x = edges, y = modularity)) +
    theme_classic() +
    labs(x = "\nnumber of edges above threshold", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(strategy), shape = factor(input), size = sigma.vis), stroke = 1.2) +
    guides(color = guide_legend("\nstrategy\n"), shape = guide_legend("\ninput\n"), size = guide_legend("\nsigma\n")) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 5, 0, 6, 3, 4, 8)) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 8, width = 12, units = "in") 
}

scatter.edges(data.anat, edges.anat, modularity.anat, strategy.anat, input.anat, sigma.vis.anat, "anatomical network", "/anat_mod_edges.png", ylim = 0.25)
scatter.edges(data.func, edges.func, modularity.func, strategy.func, input.func, sigma.vis.func, "functional network", "/func_mod_edges.png")
scatter.edges(data.coords.anat, edges.coords.anat, modularity.coords.anat, strategy.coords.anat, input.coords.anat, sigma.vis.coords.anat, "anatomical network\n(coordinates)", "/anat_mod_edges_sigma_coords.png", ylim = 0.25)
scatter.edges(data.coords.func, edges.coords.func, modularity.coords.func, strategy.coords.func, input.coords.func, sigma.vis.coords.func, "functional network\n(coordinates)", "/func_mod_edges_sigma_coords.png")
scatter.edges(data.words.anat, edges.words.anat, modularity.words.anat, strategy.words.anat, input.words.anat, sigma.vis.words.anat, "anatomical network\n(abstracts & full texts)", "/anat_mod_edges_sigma_words.png", ylim = 0.25)
scatter.edges(data.words.behav, edges.words.behav, modularity.words.behav, strategy.words.behav, input.words.behav, sigma.vis.words.behav, "behavioral network\n(abstracts & full texts)", "/behav_mod_edges_sigma_words.png")
scatter.edges(data.words.func, edges.words.func, modularity.words.func, strategy.words.func, input.words.func, sigma.vis.words.func, "functional network\n(abstracts & full texts)", "/func_mod_edges_sigma_words.png")

# Function for scatterplot of modularity vs. sigma, points labeled by strategy and input
scatter.sigma <- function(data, edges, modularity, strategy, input, sigma, title, filename, ylim = 0.375) {
  plot <- ggplot(data, aes(x = sigma, y = modularity)) +
    theme_classic() +
    labs(x = "\nsigma for gaussian smoothing", y = "modularity\n", title = paste(title, "\n", sep = "")) +
    geom_point(aes(color = factor(strategy), shape = factor(input), size = edges), stroke = 1.2) +
    guides(color = guide_legend("\nstrategy\n"), shape = guide_legend("\ninput\n"), size = guide_legend("\nedges\n")) +
    scale_y_continuous(limits = c(0, ylim)) +
    scale_x_continuous(limits = c(-0.01, 55)) +
    scale_colour_manual(values = adjustcolor(palette, 0.85)) +
    scale_shape_manual(values = c(2, 1, 5, 0, 6, 3, 4, 8)) +
    scale_size(range = c(2, 10)) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24, color = "gray60", hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_text(family = "Lucida Console", size = 16, hjust = 0.5),
          legend.title = element_text(family = "Lucida Console", size = 14, color = "gray60"),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
  ggsave(file = paste(plot.path, filename, sep = ""), plot, height = 8, width = 12, units = "in") 
}

scatter.sigma(data.anat, edges.anat, modularity.anat, strategy.anat, input.anat, sigma.vis.anat, "anatomical network", "/anat_mod_sigma.png", ylim = 0.25)
scatter.sigma(data.func, edges.func, modularity.func, strategy.func, input.func, sigma.vis.func, "functional network", "/func_mod_sigma.png")
scatter.sigma(data.coords.anat, edges.coords.anat, modularity.coords.anat, strategy.coords.anat, input.coords.anat, sigma.vis.coords.anat, "anatomical network\n(coordinates)", "/anat_mod_sigma_edges_coords.png", ylim = 0.25)
scatter.sigma(data.coords.func, edges.coords.func, modularity.coords.func, strategy.coords.func, input.coords.func, sigma.vis.coords.func, "functional network\n(coordinates)", "/func_mod_sigma_edges_coords.png")
scatter.sigma(data.words.anat, edges.words.anat, modularity.words.anat, strategy.words.anat, input.words.anat, sigma.vis.words.anat, "anatomical network\n(abstracts & full texts)", "/anat_mod_sigma_edges_words.png", ylim = 0.25)
scatter.sigma(data.words.behav, edges.words.behav, modularity.words.behav, strategy.words.behav, input.words.behav, sigma.vis.words.behav, "behavioral network\n(abstracts & full texts)", "/behav_mod_sigma_edges_words.png")
scatter.sigma(data.words.func, edges.words.func, modularity.words.func, strategy.words.func, input.words.func, sigma.vis.words.func, "functional network\n(abstracts & full texts)", "/func_mod_sigma_edges_words.png")
