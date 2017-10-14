# Clear workspace
rm(list=ls())

# Import libraries
library(extrafont)
library(forcats)
library(ggplot2)

# Set working directory
vec.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/vecs"
setwd(vec.path)

# Create path to lemma plots
lemma.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/plots/lemmas"
dir.create(lemma.path)

# Load lemma dictionary
data <- read.csv(paste(vec.path, "/lemmas_frequencies.csv", sep = ""), header = TRUE)

# Load term frequencies
text <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/texts/texts/preproc"
docs <- VCorpus(DirSource(text))
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))

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

# Plot term frequencies
plot <- ggplot(data, aes(x = reorder(LEMMA, -LEMMA_ORDER), y = PERCENTAGE)) +   
  geom_bar(aes(fill = as.factor(TERM_ORDER)), position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = adjustcolor(palette, 0.65)) + 
  theme(legend.position = "none",
        plot.title = element_text(family = "Lucida Sans Unicode", size = 20, hjust = 0),
        legend.title = element_text(family = "Lucida Console", size = 14),
        axis.ticks = element_blank(),
        axis.line = element_line(size = 0, color = "gray"),
        #axis.line.y = element_line(size = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 16, hjust = 1),
        axis.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in")) +
  coord_flip() +
  scale_y_reverse()

ggsave(file = paste(lemma.path, "/lemma_freqs.png", sep = ""), plot, height = 2, width = 10, units = "in")

