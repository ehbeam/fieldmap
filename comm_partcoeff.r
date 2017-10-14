# Clear workspace
rm(list=ls())

# Import libraries
library(base)
library(brainGraph)
library(extrafont)
library(ggplot2)
library(RColorBrewer)
library(tools)

# Point working directory to networks
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")

# Set path for plots
plot.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/partcoeff/"
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

# Function for converting network names to figure labels
presentable <- function(networks, remove) {
  net.names <- c()
  for (net in networks) {
    name <- file_path_sans_ext(net)
    name <- gsub("_studies","", name)
    #name <- gsub("_probabilistic-winner-takes-all", "", name)
    #name <- gsub("_winner-takes-all", "", name)
    #name <- gsub("_probabilistic", "", name)
    name <- gsub("_raw", "0 mm", name)
    #name <- gsub("mm", " mm", name)
    name <- gsub("gaussian_", "", name)
    name <- gsub("_anat", "", name)
    name <- gsub("_behav", "", name)
    name <- gsub("_func", "", name)
    name <- gsub("_", ", ", name)
    net.names <- c(net.names, name)
  }
  return(net.names)
}

# Load network file list
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)
network <- networks[1]

# Load node classes
anat <- c("frontal pole", "insular cortex", "superior frontal gyrus", "middle frontal gyrus", "inferior frontal gyrus", "precentral gyrus", "temporal pole", "superior temporal gyrus", "middle temporal gyrus", "inferior temporal gyrus", "postcentral gyrus", "superior parietal lobule", "supramarginal gyrus", "angular gyrus", "lateral occipital cortex", "intracalcarine cortex", "frontal medial cortex", "supplementary motor cortex", "subcallosal cortex", "paracingulate gyrus", "cingulate gyrus", "precuneous cortex", "cuneal cortex", "frontal orbital cortex", "parahippocampal gyrus", "lingual gyrus", "temporal fusiform cortex", "temporal occipital fusiform cortex", "occipital fusiform gyrus", "frontal operculum cortex", "central opercular cortex", "parietal operculum cortex", "planum polare", "heschl's gyrus", "planum temporale", "supracalcarine cortex", "occipital pole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor learning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit memory", "implicit memory", "working memory", "music", "reasoning", "social cognition", "somatic", "spatial", "temporal cognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "interoception", "baroregulation", "gastrointestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Function to load raw network
load.net <- function(network) {
  file <- read.csv(network)
  dat <- as.matrix(file)
  net <- graph.edgelist(dat[,1:2], directed = FALSE)
  E(net)$weight <- as.numeric(dat[,3])
  net <- delete_edges(net, which(E(net)$weight == 0))
  #net <- simplify(net, remove.loops = TRUE)
  return(net)
}

# Function to load normalized network
load.net <- function(network) {
  file <- read.csv(network)
  dat <- as.matrix(file)
  net <- graph.edgelist(dat[,1:2], directed = FALSE)
  E(net)$weight <- as.numeric(dat[,3])
  net <- delete_edges(net, which(E(net)$weight == 0))
  net <- simplify(net, remove.loops = TRUE)
  return(net)
}

# Function to load participation coefficient
load.partcoeff <- function(net) {
  clust <- cluster_fast_greedy(net, weights = E(net)$weight)
  partcoeff <- part_coeff(net, clust$membership)
  return(partcoeff)
}


# Function to plot scatter of participation coefficient in networks of coordinates vs. abstracts and full texts
plot.partcoeff <- function(networks) {
  
  for (network in networks) {
    
    # Load network of coordinates
    net <- load.net(network)
    
    # Load node names
    name <- V(net)$name
    
    # Load participation coefficients
    coords.partcoeff <- load.partcoeff(net)
    
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
    
    # Load networks of words for plotting against network of coordinates
    if (grepl("anat", network)) { networks.words <- c("abstracts_studies_lemmas_anat.csv", "abstracts_studies_sentences_lemmas_anat.csv", "texts_studies_lemmas_anat.csv", "texts_studies_sentences_lemmas_anat.csv") }
    if (grepl("behav", network)) { networks.words <- c("abstracts_studies_lemmas_behav.csv", "abstracts_studies_sentences_lemmas_behav.csv", "texts_studies_lemmas_behav.csv", "texts_studies_sentences_lemmas_behav.csv") }
    if (grepl("func_texts", network)) { networks.words <- c("texts_studies_lemmas_func.csv", "texts_studies_sentences_lemmas_func.csv") }
    if (grepl("func_abstracts", network)) { networks.words <- c("abstracts_studies_lemmas_func.csv", "abstracts_studies_sentences_lemmas_func.csv") }
    else {if (grepl("func", network)) { networks.words <- c("abstracts_studies_lemmas_func.csv", "abstracts_studies_sentences_lemmas_func.csv", "texts_studies_lemmas_func.csv", "texts_studies_sentences_lemmas_func.csv") }}

    for (network.words in networks.words) {
      
      # Load network of words and its participation coefficient
      net.words <- load.net(network.words)
      words.partcoeff <- load.partcoeff(net.words)
      
      if (length(coords.partcoeff) == length(words.partcoeff)) {
        # Build data frame
        data <- data.frame(name, coords.partcoeff, words.partcoeff)
        colnames(data) <- c("name", "pc.coords", "pc.words")
        
        # Scatter plot of strength in lemmatized vs. unlemmatized networks
        plot <- ggplot(data, aes(x = pc.coords, y = pc.words, label = name)) +
          theme_classic() +
          labs(x = paste("\n", presentable(network)), y = paste(presentable(network.words), "\n")) +
          geom_point(aes(color = factor(class)), size = 2) +
          #scale_y_continuous(limits = c(0.6, 0.75)) +
          geom_text(aes(label = name, color = factor(class)), 
                    check_overlap = FALSE, hjust = "inward", vjust = -1,
                    show.legend = FALSE, 
                    family = "Lucida Sans Unicode", cex = 3.75) +
          scale_color_manual(values = c("gray65", "black")) +
          theme(legend.position = "none",
                plot.title = element_text(family = "Lucida Sans Unicode", size = 14, hjust = 0.5, color = "gray40"),
                axis.line.x = element_line(size = 0.5),
                axis.line.y = element_line(size = 0.5),
                axis.text = element_text(family = "Lucida Sans Unicode", size = 14),
                axis.title = element_text(family = "Lucida Sans Unicode", size = 20, hjust = 0.5),
                plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
        #geom_smooth(method = lm, linetype = 0, alpha = 0.05)
        
        # Save plot
        ggsave(file = paste(plot.path, file_path_sans_ext(network), "_VS_", file_path_sans_ext(network.words), ".png", sep = ""), plot, height = 7, width = 12, units = "in")
        
      }
    }
  }
}

  
# Filter networks
networks.coords <- c("coordinates_studies_raw_probabilistic-winner-takes-all_anat.csv", "coordinates_studies_raw_probabilistic-winner-takes-all_lemmas_func_abstracts.csv", "coordinates_studies_raw_probabilistic-winner-takes-all_lemmas_func_texts.csv")
networks.words <- grep("studies_sentences_lemmas", networks, value = TRUE)
  
# Execute plotting
plot.partcoeff(networks.coords)
#plot.partcoeff(networks.words)