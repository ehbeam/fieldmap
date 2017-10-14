# Clear workspace
rm(list=ls())

# Import libraries
library(igraph)
library(ggplot2)
library(optimbase)
library(reshape2)
library(tools)

# Point working directory to networks
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")

# Load network file list
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Function for converting network names to figure labels
presentable <- function(networks, remove) {
  net.names <- c()
  for (net in networks) {
    name <- file_path_sans_ext(net)
    for (word in remove) {
      name <- gsub(word, "", name)
    }
    name <- gsub("_probabilistic-winner-takes-all_", "prob-wta", name)
    name <- gsub("_winner-takes-all_", "wta", name)
    name <- gsub("_probabilistic_", "prob", name)
    name <- gsub("raw", "0mm", name)
    name <- gsub("mm", " mm", name)
    name <- gsub("gaussian_", "", name)
    name <- gsub("_anat", "", name)
    name <- gsub("_behav", "", name)
    name <- gsub("_func", "", name)
    name <- gsub("_", ", ", name)
    net.names <- c(net.names, name)
  }
  return(net.names)
}

# Function to sort list of thresholded networks
sorted.thres <- function(networks) {
  networks <- gsub("thres100", "thres0100", networks)
  networks <- gsub("thres200", "thres0200", networks)
  networks <- gsub("thres25", "thres0025", networks)
  networks <- gsub("thres400", "thres0400", networks)
  networks <- gsub("thres50", "thres0050", networks)
  networks <- gsub("thres800", "thres0800", networks)
  networks <- sort(networks)
  networks <- gsub("thres0100", "thres100", networks)
  networks <- gsub("thres0200", "thress200", networks)
  networks <- gsub("thres0025", "thres25", networks)
  networks <- gsub("thres0400", "thress400", networks)
  networks <- gsub("thres0050", "thres50", networks)
  networks <- gsub("thres0800", "thres800", networks)
  return(networks)
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

# Function to plot alignments
plot.comparisons <- function(networks, file.name, alignment.method, fig.height, fig.width, remove = c()){
  
  # Load presentable names 
  net.names <- presentable(networks, remove)
  
  # Initialize matrix of community alignments
  alignments <- matrix(nrow = length(networks), ncol = length(networks))
  colnames(alignments) <- net.names
  rownames(alignments) <- net.names
  
  for (i in 1:length(networks)) {
    for (j in 1:length(networks)) {
      
      # Load community structure of network 1
      file.1 <- read.csv(networks[i])
      dat.1 <- as.matrix(file.1)
      net.1 <- graph.edgelist(dat.1[,1:2], directed = FALSE)
      E(net.1)$weight <- as.numeric(dat.1[,3])
      net.1 <- delete_edges(net.1, which(E(net.1)$weight == 0))
      net.1 <- simplify(net.1, remove.loops = TRUE)
      clust.1 <- cluster_fast_greedy(net.1, weights = E(net.1)$weight)
      
      # Load community structure of network 2
      file.2 <- read.csv(networks[j])
      dat.2 <- as.matrix(file.2)
      net.2 <- graph.edgelist(dat.2[,1:2], directed = FALSE)
      E(net.2)$weight <- as.numeric(dat.2[,3])
      net.2 <- delete_edges(net.2, which(E(net.2)$weight == 0))
      net.2 <- simplify(net.2, remove.loops = TRUE)
      clust.2 <- cluster_fast_greedy(net.2, weights = E(net.2)$weight)
      
      # Load community alignments
      alignments[i,j] <- compare(membership(clust.1), membership(clust.2), method = alignment.method)
      
    }
  }
  
  # Plot heatmap of alignments
  alignments.melt <- melt(alignments)
  heatmap.alignments <- ggplot(data = alignments.melt, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile() + theme_classic() +
    scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00", 
                         midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                         name = "") +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 12, angle = 65, hjust = 1),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_blank(),
          legend.position = "none",
          #legend.position = "left",
          #legend.direction = "vertical",
          #legend.key.height = unit(3, "cm"),
          #legend.key.width = unit(0.6, "cm"),
          #legend.text = element_text(family = "Lucida Sans Unicode", size = 14),
          plot.margin = grid::unit(c(0.15, 0.15, 0.15, 0.15), "in"))
  ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/alignments/", file.name, "_", alignment.method, ".png", sep = ""), heatmap.alignments, height = fig.height, width = fig.width, units = "in")

}

### WORDS ONLY ###

# Filter networks
networks.words.sentences.anat <- grep("sentences.*anat", networks, value = TRUE)
networks.words.bags.anat <- grep("sentences", c(grep("abstracts.*anat", networks, value = TRUE), grep("texts.*anat", networks, value = TRUE)), value = TRUE, invert = TRUE)
networks.words.sentences.behav <- c(grep("brainmap_behav", networks, value = TRUE), grep("sentences.*behav", networks, value = TRUE))
networks.words.bags.behav <- c(grep("brainmap_behav", networks, value = TRUE), grep("sentences", c(grep("abstracts.*behav", networks, value = TRUE), grep("texts.*behav", networks, value = TRUE)), value = TRUE, invert = TRUE))
networks.words.sentences.func <- grep("sentences.*func", networks, value = TRUE)
networks.words.bags.func <- grep("sentences", c(grep("abstracts.*func", networks, value = TRUE), grep("texts.*func", networks, value = TRUE)), value = TRUE, invert = TRUE)

# Execute network comparisons
# plot.comparisons(networks.words.sentences.anat, "words_sentences_anat", "nmi", 3, 3.35, remove = c("studies_", "sentences_"))
# plot.comparisons(networks.words.sentences.behav, "words_sentences_behav", "nmi", 3, 3.35, remove = c("studies_", "sentences_"))
# plot.comparisons(networks.words.sentences.func, "words_sentences_func", "nmi", 3, 3.35, remove = c("studies_", "sentences_"))
# plot.comparisons(networks.words.bags.anat, "words_bags_anat", "nmi", 3, 3.35, remove = c("studies_"))
# plot.comparisons(networks.words.bags.behav, "words_bags_behav", "nmi", 3, 3.35, remove = c("studies_"))
# plot.comparisons(networks.words.bags.func, "words_bags_func", "nmi", 3, 3.35, remove = c("studies_"))


### WINDOWED WORDS & COORDS ###

# Filter words by sentences and lemmas
networks.words.sentences.lemmas.anat <- grep("lemmas", networks.words.sentences.anat, value = TRUE)
networks.words.sentences.lemmas.func <- grep("lemmas", networks.words.sentences.func, value = TRUE)

### EXPERIMENTS ###

## WINNER-TAKES-ALL ##
networks.wta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_winner-takes-all", networks, value = TRUE))
networks.wta.experiments.anat <- grep("anat", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.texts <- grep("lemmas_func_texts", networks.wta.experiments, value = TRUE)
plot.comparisons(c(networks.wta.experiments.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_exp_wta_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "sentences_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.wta.experiments.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_wta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.wta.experiments.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_wta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.experiments.anat <- grep("anat", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.texts <- grep("lemmas_func_texts", networks.pwta.experiments, value = TRUE)
plot.comparisons(c(networks.pwta.experiments.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_exp_pwta_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.pwta.experiments.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_pwta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.pwta.experiments.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_pwta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC ##
networks.p.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic_", networks, value = TRUE))
networks.p.experiments.anat <- grep("anat", networks.p.experiments, value = TRUE)
networks.p.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.p.experiments, value = TRUE)
networks.p.experiments.func.texts <- grep("lemmas_func_texts", networks.p.experiments, value = TRUE)
plot.comparisons(c(networks.p.experiments.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_exp_prob_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "sentences_", "lemmas_", "coordinates_"))
plot.comparisons(c(networks.p.experiments.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_prob_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.p.experiments.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_exp_prob_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))


### STUDIES ###

## WINNER-TAKES-ALL ##
networks.wta.studies <- sorted.sigma(grep("coordinates_studies_.*_winner-takes-all", networks, value = TRUE))
networks.wta.studies.anat <- grep("anat", networks.wta.studies, value = TRUE)
networks.wta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.studies, value = TRUE)
networks.wta.studies.func.texts <- grep("lemmas_func_texts", networks.wta.studies, value = TRUE)
plot.comparisons(c(networks.wta.studies.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_stud_wta_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "sentences_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.wta.studies.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_wta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.wta.studies.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_wta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.studies.anat <- grep("anat", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.texts <- grep("lemmas_func_texts", networks.pwta.studies, value = TRUE)
plot.comparisons(c(networks.pwta.studies.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_stud_pwta_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.pwta.studies.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_pwta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.pwta.studies.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_pwta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC ##
networks.p.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic_", networks, value = TRUE))
networks.p.studies.anat <- grep("anat", networks.p.studies, value = TRUE)
networks.p.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.p.studies, value = TRUE)
networks.p.studies.func.texts <- grep("lemmas_func_texts", networks.p.studies, value = TRUE)
plot.comparisons(c(networks.p.studies.anat, networks.words.sentences.lemmas.anat), "coords&words_sentences_stud_prob_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "sentences_", "lemmas_", "coordinates_"))
plot.comparisons(c(networks.p.studies.func.abstracts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_prob_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "sentences_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.p.studies.func.texts, networks.words.sentences.lemmas.func), "coords&words_sentences_stud_prob_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "sentences_", "lemmas_", "_func_texts", "coordinates_"))


### UNWINDOWED WORDS & COORDS ###

# Filter words by bags and lemmas
networks.words.bags.lemmas.anat <- grep("lemmas", networks.words.bags.anat, value = TRUE)
networks.words.bags.lemmas.func <- grep("lemmas", networks.words.bags.func, value = TRUE)

### EXPERIMENTS ###

## WINNER-TAKES-ALL ##
networks.wta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_winner-takes-all", networks, value = TRUE))
networks.wta.experiments.anat <- grep("anat", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.experiments, value = TRUE)
networks.wta.experiments.func.texts <- grep("lemmas_func_texts", networks.wta.experiments, value = TRUE)
plot.comparisons(c(networks.wta.experiments.anat, networks.words.bags.lemmas.anat), "coords&words_bags_exp_wta_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "bags_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.wta.experiments.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_exp_wta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.wta.experiments.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_exp_wta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "winner-takes-all_", "bags_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.experiments.anat <- grep("anat", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.experiments, value = TRUE)
networks.pwta.experiments.func.texts <- grep("lemmas_func_texts", networks.pwta.experiments, value = TRUE)
plot.comparisons(c(networks.pwta.experiments.anat, networks.words.bags.lemmas.anat), "coords&words_bags_exp_pwta_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.pwta.experiments.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_exp_pwta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.pwta.experiments.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_exp_pwta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC ##
networks.p.experiments <- sorted.sigma(grep("coordinates_experiments_.*_probabilistic_", networks, value = TRUE))
networks.p.experiments.anat <- grep("anat", networks.p.experiments, value = TRUE)
networks.p.experiments.func.abstracts <- grep("lemmas_func_abstracts", networks.p.experiments, value = TRUE)
networks.p.experiments.func.texts <- grep("lemmas_func_texts", networks.p.experiments, value = TRUE)
plot.comparisons(c(networks.p.experiments.anat, networks.words.bags.lemmas.anat), "coords&words_bags_exp_prob_anat", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "bags_", "lemmas_", "coordinates_"))
plot.comparisons(c(networks.p.experiments.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_exp_prob_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.p.experiments.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_exp_prob_func_texts", "nmi", 4, 4.5, remove = c("studies_", "experiments_", "probabilistic_", "bags_", "lemmas_", "_func_texts", "coordinates_"))


### STUDIES ###

## WINNER-TAKES-ALL ##
networks.wta.studies <- sorted.sigma(grep("coordinates_studies_.*_winner-takes-all", networks, value = TRUE))
networks.wta.studies.anat <- grep("anat", networks.wta.studies, value = TRUE)
networks.wta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.wta.studies, value = TRUE)
networks.wta.studies.func.texts <- grep("lemmas_func_texts", networks.wta.studies, value = TRUE)
plot.comparisons(c(networks.wta.studies.anat, networks.words.bags.lemmas.anat), "coords&words_bags_stud_wta_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "bags_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.wta.studies.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_stud_wta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.wta.studies.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_stud_wta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "winner-takes-all_", "bags_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC WINNER-TAKES-ALL ##
networks.pwta.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic-winner-takes-all", networks, value = TRUE))
networks.pwta.studies.anat <- grep("anat", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.pwta.studies, value = TRUE)
networks.pwta.studies.func.texts <- grep("lemmas_func_texts", networks.pwta.studies, value = TRUE)
plot.comparisons(c(networks.pwta.studies.anat, networks.words.bags.lemmas.anat), "coords&words_bags_stud_pwta_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "coordinates_", "anat_"))
plot.comparisons(c(networks.pwta.studies.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_stud_pwta_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.pwta.studies.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_stud_pwta_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic-winner-takes-all_", "bags_", "lemmas_", "_func_texts", "coordinates_"))

## PROBABILISTIC ##
networks.p.studies <- sorted.sigma(grep("coordinates_studies_.*_probabilistic_", networks, value = TRUE))
networks.p.studies.anat <- grep("anat", networks.p.studies, value = TRUE)
networks.p.studies.func.abstracts <- grep("lemmas_func_abstracts", networks.p.studies, value = TRUE)
networks.p.studies.func.texts <- grep("lemmas_func_texts", networks.p.studies, value = TRUE)
plot.comparisons(c(networks.p.studies.anat, networks.words.bags.lemmas.anat), "coords&words_bags_stud_prob_anat", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "bags_", "lemmas_", "coordinates_"))
plot.comparisons(c(networks.p.studies.func.abstracts, networks.words.bags.lemmas.func), "coords&words_bags_stud_prob_func_abstracts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "bags_", "lemmas_", "_func_abstracts", "coordinates_"))
plot.comparisons(c(networks.p.studies.func.texts, networks.words.bags.lemmas.func), "coords&words_bags_stud_prob_func_texts", "nmi", 4, 4.5, remove = c("studies_", "studies_", "probabilistic_", "bags_", "lemmas_", "_func_texts", "coordinates_"))


