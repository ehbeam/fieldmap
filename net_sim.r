# Function to set range from 0 to 1
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Initialize matrix of network similarity
similarities <- matrix(nrow = length(networks), ncol = length(networks))
colnames(similarities) <- net.names
rownames(similarities) <- net.names

# Load network similarity
E(net.1)$weight <- normalize(E(net.1)$weight)
E(net.2)$weight <- normalize(E(net.2)$weight)
mat.1 <- as_adjacency_matrix(net.1, attr = "weight")
mat.2 <- as_adjacency_matrix(net.2, attr = "weight")
dif <- abs(mat.1 - mat.2)
sum <- mat.1 + mat.2
mat.ones <- ones(length(V(net.1)), length(V(net.1)))
sim <- mat.ones - (dif / sum)
sim.clean <- gsub(Inf, NA, sim)
sim.final <- as.numeric(sim.clean)
similarities[i,j] <- sum(sim.final, na.rm = TRUE)

# Plot heatmap of similarities
similarities.scaled <- t(apply(similarities, 1, function(x)(x-min(x))/(max(x)-min(x))))
similarities.melt <- melt(similarities.scaled)
heatmap.similarities <- ggplot(data = similarities.melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + theme_classic() +
  scale_fill_gradient2(low = "#79b0db", high = "#cc3300", mid = "#eaab00",
                       midpoint = 0.5, limit = c(0, 1), space = "Lab",
                       name = "network\nsimilarity\n") +
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 24, hjust = 0.5),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 8),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = paste("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/similarities/", file.name, ".png", sep = ""), heatmap.similarities, height = fig.height, width = fig.width, units = "in")
