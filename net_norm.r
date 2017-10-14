# Clear workspace
rm(list=ls())

# Import libraries
library(extrafont)
library(igraph)
library(latentnet)
library(RColorBrewer)
library(tm)

# Set threshold variable for number of strongest nodes to visualize
threshold = 30

# Point working directory to networks
net.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks"
setwd(net.path)

# Set path for visualizations
vis.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/visuals"
norm.vis.path <- paste(vis.path, "/norm", sep = "")
norm.vis.path.thres <- paste(norm.vis.path, "/thres", threshold, sep = "")

# Set path for normalized edge lists
norm.net.path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/norms"

# Create directories for normalized networks by threshold level
dir.create(vis.path)
dir.create(norm.vis.path)
dir.create(paste(norm.path.thres))
dir.create(norm.net.path)

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

# Load network file list
networks <- list.files(pattern = ".*csv")
networks <- grep("coordinates_studies_raw_probabilistic-winner-takes-all", networks, value = TRUE)

# Load term frequencies
text <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/texts/texts/preproc"
docs <- VCorpus(DirSource(text))
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))

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

# Function to convert node labels to exportable form
exportable <- function(data) {
  data <- gsub(" ", "_", data)
}

# Iteratively plot network normalizations
for (network in networks) {
  
  # Load adjacency matrix
  rdr <- read.csv(network)
  dat <- as.matrix(rdr)
  g <- graph.data.frame(dat)
  E(g)$weight <- as.numeric(dat[,3])
  g <- delete_edges(g, which(E(g)$weight == 0))
  g <- simplify(g, remove.loops = TRUE)
  
  # Assign term frequencies
  f <- c()
  for (i in 1:length(V(g))) {
    label <- gsub(" ", "_", V(g)$name[i])
    label <- gsub("'", "", label)
    if (!is.na(freq[label])) {f <- c(f, freq[label])}
    else {f <- c(f, 0)}
  }
  V(g)$frequency <- scale(f, center = FALSE)
  
  # Coerce to network
  a <- get.adjacency(g, sparse = FALSE, attr = "weight", names = TRUE, edges = TRUE)
  n <- network(a, directed = FALSE)
  n%v%"frequency" <- V(g)$frequency
  n%e%"weight" <- E(g)$weight
  
  # Fit exponential random graph mixed model
  #fit <- ergmm(n ~ bilinear(2) + nodecov("frequency"), response = "weight", family = "binomial.logit", fam.par = list(trials = 1763), verbose = TRUE)
  fit <- ergmm(n ~ bilinear(2), family = "binomial.logit", fam.par = list(trials = 1763), verbose = TRUE)
  
  # Coerce predicted dyad values to network
  fit.pred <- predict(fit)
  fit.net <- graph.adjacency(fit.pred, weighted = TRUE, mode = "undirected")
  V(fit.net)$name <- exportable(V(g)$name)
  
  # Export normalized network edge list
  write_graph(fit.net, paste(norm.net.path, "/", file_path_sans_ext(network), "_norm.txt", sep = ""), format = "ncol")
  
  # # Load vertex class assignments
  # anat <- c("frontal\npole", "insular\ncortex", "superior\nfrontal\ngyrus", "middle\nfrontal\ngyrus", "inferior\nfrontal\ngyrus", "precentral\ngyrus", "temporal\npole", "superior\ntemporal\ngyrus", "middle\ntemporal\ngyrus", "inferior\ntemporal\ngyrus", "post-\ncentral\ngyrus", "superior\nparietal\nlobule", "supra-\nmarginal\ngyrus", "angular\ngyrus", "lateral\noccipital\ncortex", "intra-\ncalcarine\ncortex", "frontal\nmedial\ncortex", "supp-\nlementary\nmotor\ncortex", "subcallosal\ncortex", "para-\ncingulate\ngyrus", "cingulate\ngyrus", "precuneous\ncortex", "cuneal\ncortex", "frontal\norbital\ncortex", "para-\nhippocampal\ngyrus", "lingual\ngyrus", "temporal\nfusiform\ncortex", "temporal\noccipital\nfusiform\ncortex", "occipital\nfusiform\ngyrus", "frontal\noperculum\ncortex", "central\nopercular\ncortex", "parietal\noperculum\ncortex", "planum\npolare", "heschl's\ngyrus", "planum\ntemporale", "supra-\ncalcarine\ncortex", "occipital\npole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
  # behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor\nlearning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit\nmemory", "implicit\nmemory", "working\nmemory", "music", "reasoning", "social\ncognition", "somatic", "spatial", "temporal\ncognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "intero-\nception", "baroregulation", "gastro-\nintestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")
  # class = c()
  # for (i in 1:length(V(fit.net)$name)) {
  #   if (V(fit.net)$name[i] %in% anat) {
  #     class <- c(class, "anat")
  #   }
  #   if (V(fit.net)$name[i] %in% behav) {
  #     class <- c(class, "behav")
  #   }
  # }
  # V(fit.net)$class <- class
  # 
  # # Perform community detection with the fast & greedy algorithm
  # fit.net <- delete_edges(fit.net, which(E(fit.net)$weight == 0))
  # fit.net <- simplify(fit.net, remove.loops = TRUE)
  # clust.fg <- cluster_fast_greedy(fit.net, weights = E(fit.net)$weight)
  # V(fit.net)$community <- clust.fg$membership
  # 
  # # Threshold to show top nodes
  # if (threshold < length(V(fit.net))) {
  #   threshold_strength <- rev(sort(strength(fit.net)))[threshold]
  #   fit.net.vis <- delete_vertices(fit.net, strength(fit.net) < threshold_strength)
  #   fit.net.vis <- delete_vertices(fit.net.vis, strength(fit.net.vis) == 0)
  # }
  # else {
  #   fit.net.vis <- delete_vertices(fit.net, strength(fit.net) == 0)
  # }
  # 
  # # Scale plot size to number of nodes
  # d <- length(V(fit.net.vis)) * 30 + 650
  # 
  # # Prepare for plotting
  # png(paste(norm.vis.path.thres, "/", file_path_sans_ext(network), ".png", sep = ""), d, d)
  # par(mar = c(0, 0, 0, 0))
  # 
  # # Visualize the network
  # V(fit.net)$name <- presentable(V(g)$name)
  # l.circle <- layout_in_circle(fit.net.vis, order = order(V(fit.net.vis)$community))
  # plot(fit.net.vis,
  #      layout = l.circle,
  #      vertex.frame.color = adjustcolor("white", 0),
  #      vertex.label.color = "black",
  #      vertex.label.family = "Lucida Console",
  #      vertex.label.font = 2,
  #      vertex.label.cex = 2,
  #      vertex.label.dist = 0,
  #      vertex.color = adjustcolor(palette, 0.65)[V(fit.net.vis)$community],
  #      vertex.shape = c("circle", "square")[1 + (V(fit.net.vis)$class == "anat")],
  #      vertex.size = scale(strength(fit.net.vis), scale = TRUE, center = FALSE)*5 + rep(6, length(V(fit.net.vis))),
  #      edge.curved = 0.35,
  #      edge.color = adjustcolor("#B0B0B0", 0.55),
  #      edge.width = scale(E(fit.net.vis)$weight, scale = TRUE, center = FALSE)*6 + rep(1, length(E(fit.net.vis))),
  #      edge.arrow.size = 0)
  # 
  # # Turn off plotting device
  # dev.off()
  
}
