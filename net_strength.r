# Clear workspace
rm(list=ls())

# Import libraries
library(base)
library(extrafont)
library(ggplot2)
library(RColorBrewer)

# Point working directory to networks
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")

# Set path for plots
plot.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/strength/"
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

# Load network file list
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Load node classes
anat <- c("frontal pole", "insular cortex", "superior frontal gyrus", "middle frontal gyrus", "inferior frontal gyrus", "precentral gyrus", "temporal pole", "superior temporal gyrus", "middle temporal gyrus", "inferior temporal gyrus", "postcentral gyrus", "superior parietal lobule", "supramarginal gyrus", "angular gyrus", "lateral occipital cortex", "intracalcarine cortex", "frontal medial cortex", "supplementary motor cortex", "subcallosal cortex", "paracingulate gyrus", "cingulate gyrus", "precuneous cortex", "cuneal cortex", "frontal orbital cortex", "parahippocampal gyrus", "lingual gyrus", "temporal fusiform cortex", "temporal occipital fusiform cortex", "occipital fusiform gyrus", "frontal operculum cortex", "central opercular cortex", "parietal operculum cortex", "planum polare", "heschl's gyrus", "planum temporale", "supracalcarine cortex", "occipital pole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor learning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit memory", "implicit memory", "working memory", "music", "reasoning", "social cognition", "somatic", "spatial", "temporal cognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "interoception", "baroregulation", "gastrointestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Function to load networks and plot scatter of strength in lemmatized vs. unlemmatized abstracts and texts
plot.strength.words <- function(networks, file.name) {

  for (network in networks) {
    
    # Load network
    file <- read.csv(network)
    dat <- as.matrix(file)
    net <- graph.edgelist(dat[,1:2], directed = FALSE)
    E(net)$weight <- as.numeric(dat[,3])
    net <- delete_edges(net, which(E(net)$weight == 0))

    # Load node names and strength
    if (grepl("lemmas", network)) {
      strength.lemma <- strength(net)
      name <- V(net)$name
    }
    else {
      strength.raw <- strength(net)
    }
  }
  
  # Assign node class
  class = c()
  for (i in 1:length(name)) {
    if (name[i] %in% anat) {
      class <- c(class, "anatomical")
    }
    if (name[i] %in% behav) {
      class <- c(class, "behavioral")
    }
  }
  
  # Build data frame
  data <- data.frame(name, strength.lemma, strength.raw)
  colnames(data) <- c("name", "strength.lemma", "strength.raw")
  
  # Scatter plot of strength in lemmatized vs. unlemmatized networks
  plot <- ggplot(data, aes(x = strength.raw, y = strength.lemma, label = name)) +
    theme_classic() +
    labs(x = "\nstrength without lemmatization", y = paste("strength with lemmatization\n")) +
    scale_y_continuous(limits = c(0, 350)) +
    geom_text(aes(label = name, color = factor(class)), 
              check_overlap = FALSE, hjust = "inward", vjust = -1,
              show.legend = FALSE, 
              family = "Lucida Sans Unicode", cex = 6) +
    scale_color_manual(values = c("black", "gray50")) +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 14, hjust = 0.5, color = "gray40"),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 14),
          axis.title = element_text(family = "Lucida Sans Unicode", size = 18, hjust = 0.5),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
    #geom_smooth(method = lm, linetype = 0, alpha = 0.05)
  
  # Save plot
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 4.5, width = 12, units = "in")
  
}

# Filter networks to compare raw and lemmatized networks of abstracts and texts, windowed by sentences
networks.abstracts.sentences.anat <- grep("abstracts.*sentences.*anat", networks, value = TRUE)
networks.abstracts.sentences.behav <- grep("abstracts.*sentences.*behav", networks, value = TRUE)
networks.abstracts.sentences.func <- grep("abstracts.*sentences.*func", networks, value = TRUE)
networks.texts.sentences.anat <- grep("texts.*sentences.*anat", networks, value = TRUE)
networks.texts.sentences.behav <- grep("texts.*sentences.*behav", networks, value = TRUE)
networks.texts.sentences.func <- grep("texts.*sentences.*func", networks, value = TRUE)

# # Execute plotting of words
# plot.strength.words(networks.abstracts.sentences.anat, "abstracts_sentences_anat")
# plot.strength.words(networks.abstracts.sentences.behav, "abstracts_sentences_behav")
# plot.strength.words(networks.abstracts.sentences.func, "abstracts_sentences_func")
# plot.strength.words(networks.texts.sentences.anat, "texts_sentences_anat")
# plot.strength.words(networks.texts.sentences.behav, "texts_sentences_behav")
# plot.strength.words(networks.texts.sentences.func, "texts_sentences_func")

# Function to load networks and plot scatter of strength in lemmatized vs. unlemmatized abstracts and texts
plot.strength <- function(networks, file.name, text) {
  
  # Load network 1
  file.1 <- read.csv(networks[1])
  dat.1 <- as.matrix(file.1)
  net.1 <- graph.edgelist(dat.1[,1:2], directed = FALSE)
  E(net.1)$weight <- as.numeric(dat.1[,3])
  net.1 <- delete_edges(net.1, which(E(net.1)$weight == 0))
  strength.coords <- strength(net.1)
  name <- V(net.1)$name
  
  # Load network 2
  file.2 <- read.csv(networks[2])
  dat.2 <- as.matrix(file.2)
  net.2 <- graph.edgelist(dat.2[,1:2], directed = FALSE)
  E(net.2)$weight <- as.numeric(dat.2[,3])
  net.2 <- delete_edges(net.2, which(E(net.2)$weight == 0))
  strength.words <- strength(net.2)
  
  # Assign node class
  class = c()
  for (i in 1:length(name)) {
    if (name[i] %in% anat) {
      class <- c(class, "anatomical")
    }
    if (name[i] %in% behav) {
      class <- c(class, "behavioral")
    }
  }
  
  # Build data frame
  data <- data.frame(name, strength.coords, strength.words)
  colnames(data) <- c("name", "strength.coords", "strength.words")
  
  # Scatter plot of strength in coordinates vs. words
  plot <- ggplot(data, aes(x = strength.coords, y = strength.words, label = name)) +
    theme_classic() +
    labs(x = "\nnetwork of coordinates", y = paste("network of", text, "\n")) +
    #scale_y_continuous(limits = c(0, 350)) +
    geom_text(aes(label = name, color = factor(class)), 
              check_overlap = FALSE, hjust = "inward", vjust = -1,
              show.legend = FALSE, 
              family = "Lucida Sans Unicode", cex = 4) +
    scale_color_manual(values = c("black", "gray50")) +
    theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 14, hjust = 0.5, color = "gray40"),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text = element_text(family = "Lucida Sans Unicode", size = 14),
          axis.title = element_text(family = "Lucida Sans Unicode", size = 18, hjust = 0.5),
          plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in")) +
  geom_smooth(method = lm, linetype = 0, alpha = 0.05)
  
  # Save plot
  ggsave(file = paste(plot.path, file.name, ".png", sep = ""), plot, height = 6.5, width = 10, units = "in")
  
}

# Filter networks to compare coords and words
plot.strength(c("coordinates_studies_raw_probabilistic-winner-takes-all_lemmas_func_abstracts.csv", "abstracts_studies_sentences_lemmas_func.csv"), "coords_abstracts_func", "abstracts")
plot.strength(c("coordinates_studies_raw_probabilistic-winner-takes-all_lemmas_func_texts.csv", "texts_studies_sentences_lemmas_func.csv"), "coords_texts_func", "full texts")
plot.strength(c("coordinates_studies_raw_probabilistic-winner-takes-all_anat.csv", "abstracts_studies_sentences_lemmas_anat.csv"), "coords_abstracts_anat", "abstracts")
plot.strength(c("coordinates_studies_raw_probabilistic-winner-takes-all_anat.csv", "texts_studies_sentences_lemmas_anat.csv"), "coords_texts_anat", "full texts")