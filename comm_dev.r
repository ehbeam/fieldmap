# Clear workspace
rm(list=ls())

# Import libraries
library(extrafont)
library(igraph)
library(linkcomm)
library(RColorBrewer)
library(stringr)
library(tools)

# Set path to clusters
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")
clust.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/clusters"
dir.create(clust.path)

# Load networks
networks.all <- list.files(pattern = ".*csv")
networks.normed <- grep("norm", networks.all, value = TRUE)
networks.thres <- grep("thres", networks.normed, value = TRUE)

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
             "#c37070") # Rose
palette <- c(palette, brewer.pal(12, "Set3"), brewer.pal(8, "Accent"))

# Set node classes
anat <- c("frontal\npole", "insular\ncortex", "superior\nfrontal\ngyrus", "middle\nfrontal\ngyrus", "inferior\nfrontal\ngyrus", "precentral\ngyrus", "temporal\npole", "superior\ntemporal\ngyrus", "middle\ntemporal\ngyrus", "inferior\ntemporal\ngyrus", "post-\ncentral\ngyrus", "superior\nparietal\nlobule", "supra-\nmarginal\ngyrus", "angular\ngyrus", "lateral\noccipital\ncortex", "intra-\ncalcarine\ncortex", "frontal\nmedial\ncortex", "supp-\nlementary\nmotor\ncortex", "subcallosal\ncortex", "para-\ncingulate\ngyrus", "cingulate\ngyrus", "precuneous\ncortex", "cuneal\ncortex", "frontal\norbital\ncortex", "para-\nhippocampal\ngyrus", "lingual\ngyrus", "temporal\nfusiform\ncortex", "temporal\noccipital\nfusiform\ncortex", "occipital\nfusiform\ngyrus", "frontal\noperculum\ncortex", "central\nopercular\ncortex", "parietal\noperculum\ncortex", "planum\npolare", "heschl's\ngyrus", "planum\ntemporale", "supra-\ncalcarine\ncortex", "occipital\npole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor\nlearning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit\nmemory", "implicit\nmemory", "working\nmemory", "music", "reasoning", "social\ncognition", "somatic", "spatial", "temporal\ncognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "intero-\nception", "baroregulation", "gastro-\nintestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Function to clean node labels
presentable <- function(data) {
  data <- gsub("gastrointestinal", "gastro- intestinal", data)
  data <- gsub("interoception", "intero- ception", data)
  data <- gsub("intracalcarine cortex", "intra- calcarine cortex", data)
  data <- gsub("parahippocampal gyrus", "para- hippocampal gyrus", data)
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
       vertex.label.family = "Lucida Console",
       vertex.label.cex = 1.35,
       vertex.label.dist = 0,
       vertex.color = adjustcolor(palette, 0.65)[V(net)$community],
       vertex.shape = c("circle", "square")[1 + (V(net)$class == "anat")],
       vertex.size = scale(degree(net), scale = TRUE, center = FALSE)*7.5 + rep(6, length(degree(net))),
       edge.curved = 0.35,
       edge.color = adjustcolor("#B0B0B0", 0.55),
       edge.width = scale(E(net)$weight, scale = TRUE, center = FALSE)*7.5 + rep(1, length(E(net)$weight)),
       edge.arrow.size = 0)

}

# Function for exporting plots of node clusters
cluster.nodes <- function(net, net.clust, dir, x) {
  
  # Load network properties
  V(net)$community <- net.clust$membership
  E(net)$community <- net.clust$membership
  
  # Scale plot size to number of nodes
  n <- length(V(net))
  d <- n * 25 + 650
  print(d)
  
  # Export circle plot
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  png(paste(dir, "circle_", file_path_sans_ext(x), ".png", sep = ""), d, d)
  l.circle <- layout_in_circle(net, order = order(V(net)$community, degree(net)))
  plot.layout(l.circle, net)
  dev.off()
  
  # Export Kamada-Kawai plot
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  png(paste(dir, "kamada-kawai_", file_path_sans_ext(x), ".png", sep = ""), d, d)
  l.kk <- layout_with_kk(net) 
  plot.layout(l.kk, net)
  dev.off()
  
}

# Create directories for node clusters
dir.create(paste(clust.path, "/nodes", sep = ""))
dir.create(paste(clust.path, "/nodes/edge_betweenness", sep = ""))
dir.create(paste(clust.path, "/nodes/fast_greedy", sep = ""))
dir.create(paste(clust.path, "/nodes/infomap", sep = ""))
dir.create(paste(clust.path, "/nodes/label_prop", sep = ""))
dir.create(paste(clust.path, "/nodes/louvain", sep = ""))
dir.create(paste(clust.path, "/nodes/walktrap", sep = ""))

# Initialize file of metrics for node clusters
write("NETWORK,INPUT,THRESHOLD,EDGE_BETWEENNESS_Communities,FAST_GREEDY_Communities,INFOMAP_Commmunities,LABEL_PROP_Commmunities,LOUVAIN_Communities,WALKTRAP_Communities,EDGE_BETWEENNESS_Modularity,FAST_GREEDY_Modularity,INFOMAP_Modularity,LABEL_PROP_Modularity,LOUVAIN_Modularity,WALKTRAP_Modularity",
      file = paste(clust.path, "/nodes/community_metrics.csv", sep = ""))

# Execute clustering of nodes
lapply(networks.normed, function(x, method) {
  
  # Load network
  data <- read.csv(x)
  data <- as.matrix(data)
  data <- presentable(data)
  net.raw <- graph.edgelist(data[,1:2], directed = FALSE)
  E(net.raw)$weight = as.numeric(data[,3])
  net <- delete.edges(net.raw, which(E(net.raw)$weight == 0))
  
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
  
  # Perform clustering
  clust.eb <- cluster_edge_betweenness(net, weights = E(net)$weight, directed = FALSE)
  clust.fg <- cluster_fast_greedy(net, weights = E(net)$weight)
  clust.im <- cluster_infomap(net, e.weights = E(net)$weight)
  clust.lp <- cluster_label_prop(net, weights = E(net)$weight)
  clust.lo <- cluster_louvain(net, weights = E(net)$weight)
  clust.wt <- cluster_walktrap(net, weights = E(net)$weight)
  
  # Compute metrics
  num.eb <- length(clust.eb)
  mod.eb <- modularity(clust.eb)
  num.fg <- length(clust.fg)
  mod.fg <- modularity(clust.fg)
  num.im <- length(clust.im)
  mod.im <- modularity(clust.im)
  num.lp <- length(clust.lp)
  mod.lp <- modularity(clust.lp)
  num.lo <- length(clust.lo)
  mod.lo <- modularity(clust.lo)
  num.wt <- length(clust.wt)
  mod.wt <- modularity(clust.wt)
  
  # Export metrics
  input <- gsub("_.*$", "", x)
  threshold <- 0
  if (grepl("thres", x)){ threshold <- sub(".*thres", "", file_path_sans_ext(x)) }
  row <- paste(x, input, threshold, 
               num.eb, num.fg, num.im, num.lp, num.lo, num.wt, mod.eb, mod.fg, mod.im, mod.lp, mod.lo, mod.wt, 
               sep = ",")
  write(row, file = paste(clust.path, "/nodes/community_metrics.csv", sep = ""), append = TRUE)
  
  # Export network plots
  cluster.nodes(net, clust.eb, paste(clust.path, "/nodes/edge_betweenness/", sep = ""), x)
  cluster.nodes(net, clust.fg, paste(clust.path, "/nodes/fast_greedy/", sep = ""), x)
  cluster.nodes(net, clust.im, paste(clust.path, "/nodes/infomap/", sep = ""), x)
  cluster.nodes(net, clust.lp, paste(clust.path, "/nodes/label_prop/", sep = ""), x)
  cluster.nodes(net, clust.lo, paste(clust.path, "/nodes/louvain/", sep = ""), x)
  cluster.nodes(net, clust.wt, paste(clust.path, "/nodes/walktrap/", sep = ""), x)
  
})

# # Function for clustering links using linkcomm
# cluster.links <- function(x, method) {
#   
#   # Load data
#   data <- read.csv(x)
#   data <- as.matrix(data)
#   data <- presentable(data)
# 
#   # Extract communities
#   method.path <- paste(clust.path, "/links/", method, sep = "")
#   dir.create(method.path)
#   title.dend.method <- paste(method.path, "/dendrogram_", file_path_sans_ext(x), ".png", sep = "")
#   png(title.dend.method, 600, 600)
#   par(mar = c(0.025, 0.025, 0.025, 0.025))
#   lc.method <- getLinkCommunities(data, hcmethod = method)
#   dev.off()
#   
#   # Plot network with communities
#   title.net.method <- paste(method.path, "/network_", file_path_sans_ext(x), ".png", sep = "")
#   png(title.net.method, 1000,  1000)
#   par(mar = c(0.025, 0.025, 0.025, 0.025))
#   plot(lc.method,
#        type = "graph",
#        layout = layout.kamada.kawai,
#        pal = palette,
#        vertex.radius = 0.05,
#        scale.vertices = 0,
#        vlabel.cex = 1.25,
#        ewidth = 5,
#        edge.curved = 0.35)
#   dev.off()
#   
#   # Plot grid with community memberships
#   title.grid.method <- paste(method.path, "/grid_", file_path_sans_ext(x), ".png", sep = "")
#   png(title.grid.method, 800,  800)
#   par(mar = c(0.025, 0.025, 0.025, 0.025))
#   plot(lc.method,
#        type = "members",
#        pal = palette)
#   dev.off()
#   
#   # Save metrics for export
#   metrics <<- paste(metrics, lc.method$numbers[3], mean(getCommunityConnectedness(lc.method, conn = "mod")), sep = ",")
#   
# }
# 
# # Create directory for link clusters
# dir.create(paste(clust.path, "/links", sep = ""))
# 
# # Initialize file of metrics for link clusters
# write("NETWORK,INPUT,SIGMA,THRESHOLD,WARD_NumCom,WARD_MeanMod,SINGLE_NumCom,SINGLE_MeanMod,COMPLETE_NumCom,COMPLETE_MeanMod,AVERAGE_NumCom,AVERAGE_MeanMod,MCQUITTY_NumCom,MCQUITTY_MeanMod,MEDIAN_NumCom,MEDIAN_MeanMod,CENTROID_NumCom,CENTROID_MeanMod", 
#       file = paste(clust.path, "/links/community_metrics.csv", sep = ""))
# 
# # Execute clustering of links
# lapply(networks.thres, function(x, method) {
# 
#   metrics <<- ""
# 
#   # Plot networks with communities
#   cluster.links(x, "ward")
#   cluster.links(x, "single")
#   cluster.links(x, "complete")
#   cluster.links(x, "average")
#   cluster.links(x, "mcquitty")
#   cluster.links(x, "median")
#   cluster.links(x, "centroid")
# 
#   # Export metrics
#   input <- gsub("_.*$", "", x)
#   sigma <- gsub("norm", " ", gsub(".*[_]([^.]+)[m].*", "\\1m", x))
#   threshold <- sub(".*thres", "", file_path_sans_ext(x))
#   row <- gsub(",,", ",", paste(x, input, sigma, threshold, metrics, sep = ","))
#   write(row, file = paste(clust.path, "/links/community_metrics.csv", sep = ""), append = TRUE)
# 
# })
# 
