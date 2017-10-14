# Clear workspace
rm(list=ls())

# Import libraries
library(extrafont)
library(ggplot2)

# Set working directory
vec.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/vecs"
setwd(vec.path)

# Create path to lemma plots
lemma.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/lemmas"
dir.create(lemma.path)

# Load data
data <- read.csv(paste(vec.path, "/lemmas_texts_examples.csv", sep = ""), header = TRUE)

# Load groups
insular_cortex <- subset(data, data$LEMMA == "insular_cortex")

# Color palette
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

fig.insular_cortex <- ggplot(insular_cortex, aes(x = reorder(TERM, -PROBABILITY), y = PROBABILITY)) +   
  geom_bar(aes(fill = SELECTED), position = position_dodge(width = 0.8), stat = "identity") +
  theme_classic() +
  labs(title = "embedding of 'insular_cortex'") +
  guides(fill = guide_legend(title = "selected\n")) +
  scale_fill_manual(values = adjustcolor(c(palette[3], palette[2]), 1)) + 
  theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 20, hjust = 0.5),
    legend.title = element_text(family = "Lucida Console", size = 14),
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    axis.text.x = element_text(family = "Lucida Sans Unicode", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_blank(),
    legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
    plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
ggsave(file = paste(lemma.path, "/insular_cortex.png", sep = ""), fig.insular_cortex, height = 4.25, width = 8.5, units = "in")

