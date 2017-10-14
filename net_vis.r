# Clear workspace
rm(list=ls())

# Import libraries
library(igraph)
library(tools)

# Set path to figures
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")
vis.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/figures"
circle.path <- paste(vis.path, "/circle", sep = "")
dir.create(circle.path)
kk.path <- paste(vis.path, "/kamada-kawai", sep = "")
dir.create(kk.path)

# Load networks
networks.all <- list.files(pattern = "norm.*csv")
networks.unscaled <- grep("freqscaled", networks.all, value = TRUE, invert = TRUE)
networks.thres <- grep("thres100", networks.unscaled, value = TRUE)
networks.raw <- grep("thres", networks.unscaled, value = TRUE, invert = TRUE)

# Set node classes
anat <- c("frontal\npole", "insular\ncortex", "superior\nfrontal\ngyrus", "middle\nfrontal\ngyrus", "inferior\nfrontal\ngyrus", "precentral\ngyrus", "temporal\npole", "superior\ntemporal\ngyrus", "middle\ntemporal\ngyrus", "inferior\ntemporal\ngyrus", "post-\ncentral\ngyrus", "superior\nparietal\nlobule", "supra-\nmarginal\ngyrus", "angular\ngyrus", "lateral\noccipital\ncortex", "intra-\ncalcarine\ncortex", "frontal\nmedial\ncortex", "supp-\nlementary\nmotor\ncortex", "subcallosal\ncortex", "para-\ncingulate\ngyrus", "cingulate\ngyrus", "precuneous\ncortex", "cuneal\ncortex", "frontal\norbital\ncortex", "para-\nhippocampal\ngyrus", "lingual\ngyrus", "temporal\nfusiform\ncortex", "temporal\noccipital\nfusiform\ncortex", "occipital\nfusiform\ngyrus", "frontal\noperculum\ncortex", "central\nopercular\ncortex", "parietal\noperculum\ncortex", "planum\npolare", "heschl's\ngyrus", "planum\ntemporale", "supra-\ncalcarine\ncortex", "occipital\npole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens")
behav <- c("action", "execution", "speech", "imagination", "inhibition", "motor\nlearning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit\nmemory", "implicit\nmemory", "working\nmemory", "music", "reasoning", "social\ncognition", "somatic", "spatial", "temporal\ncognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "intero-\nception", "baroregulation", "gastro-\nintestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape")

# Set palette
palette <- c(adjustcolor("#EAAB00", 0.65), adjustcolor("#CC3300", 0.65))

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

# Plot thresholded networks iteratively
lapply(networks.thres, function(x) {
  
  # Load data
  data <- read.csv(x)
  data <- as.matrix(data)
  data <- presentable(data)
  net.raw <- graph.edgelist(data[,1:2])
  E(net.raw)$weight = as.numeric(data[,3])*0.75
  net <- delete.edges(net.raw, which(E(net.raw)$weight == 0))
  V(net)$size <- degree(net)*0.25
  
  # Scale nodes and edges
  E(net)$weight = as.numeric(data[,3])*0.25
  V(net)$size <- degree(net)*1.25
  
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
 
  # Build circle plot
  c <- layout_in_circle(net, order = order(degree(net)))
  c.title <- paste(circle.path, "/", file_path_sans_ext(x), ".png", sep = "")
  png(c.title, 1250, 1250)
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  circle <- plot(net,
                layout = c * -1, 
                vertex.color = palette[1 + (V(net)$class == "behav")],
                vertex.frame.color = adjustcolor("white", 0),
                vertex.label.color = "black",
                vertex.label.family = "Lucida Console",
                vertex.label.cex = 0.95,
                vertex.label.dist = 0,
                edge.curved = 0.35,
                edge.color = adjustcolor("#B0B0B0", 0.55),
                edge.width = E(net)$weight,
                edge.arrow.size = 0
                )
  save(circle, file = c.title)
  dev.off()
  
  # Build Kamada Kawai plot
  kk <- layout_with_kk(net)
  kk.title <- paste(kk.path, "/", file_path_sans_ext(x), ".png", sep = "")
  png(kk.title, 1250, 1250)
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  kamada_kawai <- plot(net,
                 layout = kk, 
                 vertex.color = palette[1 + (V(net)$class == "behav")],
                 vertex.frame.color = adjustcolor("white", 0),
                 vertex.label.color = "black",
                 vertex.label.family = "Lucida Console",
                 vertex.label.cex = 1.1,
                 vertex.label.dist = 0,
                 edge.curved = 0.35,
                 edge.color = adjustcolor("#B0B0B0", 0.55),
                 edge.width = E(net)$weight,
                 edge.arrow.size = 0
                 )
  save(kamada_kawai, file = kk.title)
  dev.off()
  
})

# Plot raw networks iteratively
lapply(networks.raw, function(x) {

  # Load data
  data <- read.csv(x)
  data <- as.matrix(data)
  data <- presentable(data)
  net.raw <- graph.edgelist(data[,1:2])
  E(net.raw)$weight = as.numeric(data[,3])*0.75
  net <- delete.edges(net.raw, which(E(net.raw)$weight == 0))
  V(net)$size <- degree(net)*0.25

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

  # Build circle plot
  c <- layout_in_circle(net, order = order(degree(net)))
  c.title <- paste(circle.path, "/", file_path_sans_ext(x), ".png", sep = "")
  png(c.title, 7000, 7000)
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  circle <- plot(net,
                 layout = c * -1,
                 vertex.color = c(adjustcolor("#EAAB00", 0.65), adjustcolor("#CC3300", 0.65))[1 + (V(net)$class == "behav")],
                 vertex.frame.color = adjustcolor("white", 0),
                 vertex.label.color = "black",
                 vertex.label.family = "Lucida Console",
                 vertex.label.cex = 3,
                 vertex.label.dist = 0,
                 edge.curved = 0.35,
                 edge.color = adjustcolor("#B0B0B0", 0.65),
                 edge.width = E(net)$weight,
                 edge.arrow.size = 0
  )
  save(circle, file = c.title)
  dev.off()

  # Build Kamada Kawai plot
  kk <- layout_with_kk(net)
  kk.title <- paste(kk.path, "/", file_path_sans_ext(x), ".png", sep = "")
  png(kk.title, 7000, 7000)
  par(mar = c(0.025, 0.025, 0.025, 0.025))
  kamada_kawai <- plot(net,
                       layout = kk,
                       vertex.color = c(adjustcolor("#EAAB00", 0.65), adjustcolor("#CC3300", 0.65))[1 + (V(net)$class == "behav")],
                       vertex.frame.color = adjustcolor("white", 0),
                       vertex.label.color = "black",
                       vertex.label.family = "Lucida Console",
                       vertex.label.cex = 3.5,
                       vertex.label.dist = 0,
                       edge.curved = 0.35,
                       edge.color = adjustcolor("#B0B0B0", 0.55),
                       edge.width = E(net)$weight,
                       edge.arrow.size = 0
  )
  save(kamada_kawai, file = kk.title)
  dev.off()

})