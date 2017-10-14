# Clear workspace
rm(list=ls())

# Import libraries
library(extrafont)
library(igraph)
library(RColorBrewer)
library(stringr)
library(tools)

# Set threshold variable for number of strongest nodes to visualize
threshold = 102

# Point working directory to networks
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")

# Set path for visualizations
vis.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/visuals"
dir.create(vis.path)

# Create directories for visualization types
dir.create(paste(vis.path, "/circle", sep = ""))
dir.create(paste(vis.path, "/kamada-kawai", sep = ""))

# Create directories for threshold level
dir.create(paste(vis.path, "/circle/thres", threshold, sep = ""))
dir.create(paste(vis.path, "/kamada-kawai/thres", threshold, sep = ""))

# Load networks
networks <- grep("thres", list.files(pattern = ".*csv"), value = TRUE, invert = TRUE)

# Set palette
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

# Load node classes
anat <- c("frontal\npole", "insular\ncortex", "superior\nfrontal\ngyrus", "middle\nfrontal\ngyrus", "inferior\nfrontal\ngyrus", "precentral\ngyrus", "temporal\npole", "superior\ntemporal\ngyrus", "middle\ntemporal\ngyrus", "inferior\ntemporal\ngyrus", "post-\ncentral\ngyrus", "superior\nparietal\nlobule", "supra-\nmarginal\ngyrus", "angular\ngyrus", "lateral\noccipital\ncortex", "intra-\ncalcarine\ncortex", "frontal\nmedial\ncortex", "supp-\nlementary\nmotor\ncortex", "subcallosal\ncortex", "para-\ncingulate\ngyrus", "cingulate\ngyrus", "precuneous\ncortex", "cuneal\ncortex", "frontal\norbital\ncortex", "para-\nhippo-\ncampal\ngyrus", "lingual\ngyrus", "temporal\nfusiform\ncortex", "temporal\noccipital\nfusiform\ncortex", "occipital\nfusiform\ngyrus", "frontal\noperculum\ncortex", "central\nopercular\ncortex", "parietal\noperculum\ncortex", "planum\npolare", "heschl's\ngyrus", "planum\ntemporale", "supra-\ncalcarine\ncortex", "occipital\npole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor\nlearning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit\nmemory", "implicit\nmemory", "working\nmemory", "music", "reasoning", "social\ncognition", "somatic", "spatial", "temporal\ncognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "intero-\nception", "baroregulation", "gastro-\nintestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Function to clean node labels
presentable <- function(data) {
  data <- gsub("gastrointestinal", "gastro- intestinal", data)
  data <- gsub("interoception", "intero- ception", data)
  data <- gsub("intracalcarine cortex", "intra- calcarine cortex", data)
  data <- gsub("parahippocampal gyrus", "para- hippo- campal gyrus", data)
  data <- gsub("paracingulate gyrus", "para- cingulate gyrus", data)
  data <- gsub("postcentral gyrus", "post- central gyrus", data)
  data <- gsub("supplementary motor cortex", "supp- lementary motor cortex", data)
  data <- gsub("supracalcarine cortex", "supra- calcarine cortex", data)
  data <- gsub("supramarginal gyrus", "supra- marginal gyrus", data)
  data <- gsub(" ", "\n", data)
}

# Function for plotting by layout
plot.layout <- function(l, net) {
  plot(net,
       layout = l,
       vertex.frame.color = adjustcolor("white", 0),
       vertex.label.color = "black",
       vertex.label.family = "Lucida Sans Unicode",
       vertex.label.font = 2,
       vertex.label.cex = 2,
       vertex.label.dist = 0,
       vertex.color = adjustcolor(palette, 0.6)[V(net)$community],
       vertex.shape = c("circle", "square")[1 + (V(net)$class == "anat")],
       vertex.size = scale(strength(net), scale = TRUE, center = FALSE)*5 + rep(6, length(V(net))),
       edge.curved = 0.35,
       edge.color = adjustcolor("#B0B0B0", 0.55),
       edge.width = scale(E(net)$weight, scale = TRUE, center = FALSE)*6 + rep(1, length(E(net))),
       edge.arrow.size = 0)
}

# Function for exporting plots of node clusters
cluster.nodes <- function(net, dir, g) {
  
  # Scale plot size to number of nodes
  d <- length(V(net)) * 30 + 650
  
  # Export circle plot
  png(paste(dir, "/circle/thres", threshold, "/", file_path_sans_ext(g), ".png", sep = ""), d, d)
  par(mar = c(0, 0, 0, 0))
  #l.circle <- layout_in_circle(net, order = order(V(net)$community, strength(net)))
  l.circle <- layout_in_circle(net, order = order(V(net)$community))
  plot.layout(l.circle, net)
  dev.off()
  
  # # Export Kamada-Kawai plot
  # par(mar = c(0.01, 0.01, 0.01, 0.01))
  # png(paste(dir, "/kamada-kawai/thres", threshold, "/", file_path_sans_ext(g), ".png", sep = ""), d, d)
  # l.kk <- layout_with_kk(net)
  # plot.layout(l.kk, net)
  # dev.off()
  
}

# Execute clustering of nodes
lapply(networks, function(g, method) {
  
  # Load network
  file <- read.csv(g)
  dat <- as.matrix(file)
  datp <- presentable(dat)
  net.raw <- graph.edgelist(datp[,1:2], directed = FALSE)
  E(net.raw)$weight = as.numeric(datp[,3])
  net <- delete_edges(net.raw, which(E(net.raw)$weight == 0))
  net <- simplify(net, remove.loops = TRUE)
  
  # Assign node class
  class = c()
  for (i in 1:length(V(net)$name)) {
    if (V(net)$name[i] %in% anat) {
      class <- c(class, "anat")
    }
    if (V(net)$name[i] %in% behav) {
      class <- c(class, "behav")
    }
  }
  V(net)$class <- class
  
  # Perform community detection with the fast & greedy algorithm
  clust.fg <- cluster_fast_greedy(net, weights = E(net)$weight)
  V(net)$community <- clust.fg$membership
  
  # Threshold to show top nodes
  if (threshold < length(V(net))) {
    threshold_strength <- rev(sort(strength(net)))[threshold]
    net.vis <- delete_vertices(net, strength(net) < threshold_strength)
    net.vis <- delete_vertices(net.vis, strength(net.vis) == 0)
  }
  else {
    net.vis <- delete_vertices(net, strength(net) == 0)
  }
  
  # Export network plots
  cluster.nodes(net.vis, vis.path, g)
  
})
