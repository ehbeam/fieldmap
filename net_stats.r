# Clear workspace
rm(list=ls())

# Import libraries
library(ggplot2)
library(extrafont)
library(stringr)
library(base)
library(Hmisc)
library(MASS)

# Load data
path <- "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
setwd(paste(path, "/stats", sep = ""))
net.data <- read.csv("network_stats.csv")
node.data <- read.csv("node_stats.csv")

# Color palette 
palette.text <- c("#E9AB17", "#C24641") # Yellow, red
palette.fill <- c(adjustcolor("#FFED6F", 0.75), adjustcolor("#FB8072", 0.75), adjustcolor ("#BEBADA", 0.75)) # Yellow, red, purple

### NETWORK STATS ###

# Create directory to network plots
net.path <- paste(path, "/plots/networks", sep = "")
dir.create(net.path)

# Load rows from network data
rownames(net.data) <- net.data$network
rows.raw <- grep("thres", rownames(net.data), value = TRUE, invert = TRUE)

## Mapping strategies ##

# Create path to plots of strategies
strat.path <- paste(net.path, "/strategies", sep = "")
dir.create(strat.path)
setwd(strat.path)

# Load rows from networks of coordinates
rows.coords.prob <- grep("probabilistic_", rows.raw, value = TRUE)
rows.coords.wta <- grep("winner.takes.all_", rows.raw, value = TRUE)
rows.coords.prob.wta <- grep("probabilistic.winner.takes.all", rows.raw, value = TRUE)
rows.coords <- c(rows.coords.prob, rows.coords.wta, rows.coords.prob.wta)
coords.data <- subset(net.data, rownames(net.data) %in% rows.coords)

# Load group memberships from networks of coordinates
coords.class <- gsub("anat", "anatomical", gsub("behav", "behavioral", gsub("func", "functional", coords.data$class)))
coords.strat <- gsub(" ", "\n", gsub("winner takes all", "winner-takes-all", coords.data$strategy))

# Function for making a bar chart comparing mapping strategies
barchart.strat <- function(stat, stat.name) {
  figure <- ggplot(coords.data, aes(coords.strat, stat)) +   
    geom_bar(aes(fill = coords.class, width = 0.7), position = position_dodge(width = 0.8), stat = "identity") +
    theme_classic() +
    labs(title = paste(stat.name, "\n")) +
    scale_fill_manual(values = c(adjustcolor("#FFED6F", 0.75), adjustcolor ("#BEBADA", 0.75))) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(family = "Lucida Sans Unicode", size = 15),
        axis.text.y = element_text(family = "Lucida Sans Unicode", size = 12),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
        plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
  title = paste(strat.path, "/", gsub(" ", "_", stat.name), ".png", sep = "")
  ggsave(file = title, figure, height = 5.5, width = 8.5, units = "in")
}

# Generate bar charts to compare mapping strategies
barchart.strat(coords.data$number.of.nodes, "number of nodes")
barchart.strat(coords.data$number.of.edges, "number of edges")
barchart.strat(coords.data$density, "density")
barchart.strat(coords.data$clustering, "clustering")
barchart.strat(coords.data$number.of.cliques, "number of cliques")
barchart.strat(coords.data$largest.clique, "largest clique size")


## Abstracts and coordinates ##

# Create path to plots of abstracts vs. coordinates
input.path <- paste(net.path, "/inputs", sep = "")
dir.create(input.path)
setwd(input.path)

# Load rows from networks of coordinates
rows.abs <- grep("abstracts", rows.raw, value = TRUE)
rows.coords.prob.wta <- c(rows.coords.prob.wta, grep("coords_behav", rows.raw, value = TRUE))
rows.inputs <- c(rows.abs, rows.coords.prob.wta)
inputs.data <- subset(net.data, rownames(net.data) %in% rows.inputs)

# Load group memberships from networks of coordinates
inputs.class <- gsub("anat", "anatomical", gsub("behav", "behavioral", gsub("func", "functional", inputs.data$class)))
inputs.input <- gsub("coords", "coordinates", inputs.data$input)

# Function for making a bar chart comparing mapping strategies
barchart.input <- function(stat, stat.name) {
  figure <- ggplot(inputs.data, aes(inputs.input, stat)) +   
    geom_bar(aes(fill = inputs.class, width = 0.7), position = position_dodge(width = 0.8), stat = "identity") +
    theme_classic() +
    labs(title = paste(stat.name, "\n")) +
    scale_fill_manual(values = palette.fill) +
    theme(plot.title = element_text(family = "Lucida Console", size = 24),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(family = "Lucida Sans Unicode", size = 15),
          axis.text.y = element_text(family = "Lucida Sans Unicode", size = 12),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(family = "Lucida Sans Unicode", size = 12),
          plot.margin = grid::unit(c(0.25, 0.1, 0.25, 0.25), "in"))
  title = paste(input.path, "/", gsub(" ", "_", stat.name), ".png", sep = "")
  ggsave(file = title, figure, height = 5.5, width = 8.5, units = "in")
}

# Generate bar charts to compare abstracts and probabilistic winner-takes-all mapping of coordinates
barchart.input(inputs.data$number.of.nodes, "number of nodes")
barchart.input(inputs.data$number.of.edges, "number of edges")
barchart.input(inputs.data$density, "density")
barchart.input(inputs.data$clustering, "clustering")
barchart.input(inputs.data$number.of.cliques, "number of cliques")
barchart.input(inputs.data$largest.clique, "largest clique size")



### NODE STATS ###

# Create path to plots of node stats
nodes.path <- paste(path, "/plots/nodes", sep = "")
dir.create(nodes.path)

# Load group memberships
node <- node.data$node
node.class <- node.data$class

# Function for cleaning up axis title
titular <- function(var, stat.name) {
  lab <- gsub("\\.in", "", var)
  lab <- gsub("\\.", "", lab)
  lab <- gsub("_", "", lab)
  lab <- gsub(" ", "", lab)
  stat <- gsub("_", "", stat.name)
  stat <- gsub(" ", "", stat)
  lab <- gsub(stat, "", lab)
  lab <- gsub("coordsbehav", "brainmap behavioral domains", lab)
  lab <- gsub("coordsprobabilisticwinnertakesall", "coordinates (probabilistic winner-takes-all)", lab)
  lab <- gsub("coordsprobabilistic", "coordinates (probabilistic)", lab)
  lab <- gsub("coordswinnertakesall", "coordinates (winner-takes-all)", lab)
  lab <- gsub("anat", "", lab)
  lab <- gsub("abstractsbehav", "abstracts", lab)
  lab <- gsub("func", "", lab)
}

# Function for getting significance level
get.sig <- function(p) {
  print(p)
  if (p < 0.001) {
    p = "*** (p < 0.001)"
  }
  else if (p < 0.01) {
    p = "** (p < 0.01)"
  }
  else if (p < 0.05) {
    p = "* (p < 0.05)"
  }
  else if (p >= 0.05) {
    p = " (p > 0.05)"
  }
  print(p)
}

# Function for making a scatter plot between two variables
scatter <- function(xvar, yvar, stat.name, stat.path, input = F, xmin = F, xmax = F, ymin = F, ymax = F) {

  print(xvar)
  print(yvar)
  
  # Load data
  x.data <- scale(node.data[,xvar], center = TRUE, scale = TRUE)
  y.data <- scale(node.data[,yvar], center = TRUE, scale = TRUE)
  
  # Plot and file labels
  if (xvar != yvar) {
    
    # Instantiate names
    xlab <- titular(xvar, stat.name)
    ylab <- titular(yvar, stat.name)
    title.par <- paste(stat.path, "/parametric/", gsub(" ", "_", paste(ylab, "VS", xlab)), ".png", sep = "")
    title.nonpar <- paste(stat.path, "/nonparametric/", gsub(" ", "_", paste(ylab, "VS", xlab)), ".png", sep = "")
    dir.create(paste(stat.path, "/parametric", sep = ""))
    dir.create(paste(stat.path, "/nonparametric", sep = ""))
    if (input != F) {
      title.par <- paste(stat.path, "/parametric/", input, "_", gsub(" ", "_", paste(ylab, "VS", xlab)), ".png", sep = "")
      title.nonpar <- paste(stat.path, "/nonparametric/", input, "_", gsub(" ", "_", paste(ylab, "VS", xlab)), ".png", sep = "")
    }
    
    # # PARAMETRIC STATS
    # 
    # # Paired samples t-test
    # ttest <- t.test(node.data[,xvar], node.data[,yvar], paired = TRUE)
    # t <- sprintf(ttest$statistic, fmt = "%0.2f")
    # t.p <- ttest$p.value * 12.0000
    # print(ttest)
    # 
    # # Pearson correlation
    # rtest <- cor.test(node.data[,xvar], node.data[,yvar], method = "pearson")
    # r <- sprintf(rtest$estimate, fmt = "%0.2f")
    # r.p <- rtest$p.value * 12.0000
    # print(rtest)
    # 
    # # Build the plot
    # figure.par <- ggplot(node.data, aes(x = node.data[,xvar], y = node.data[,yvar], label = node)) +
    #   theme_classic() +
    #   labs(x = paste("\n", xlab), y = paste(ylab, "\n"), title = paste("t = ", t, get.sig(t.p), "\n", "r = ", r, get.sig(r.p), sep = "")) +
    #   scale_x_continuous(limits = c(xmin, xmax)) +
    #   scale_y_continuous(limits = c(ymin, ymax)) +
    #   geom_point(aes(color = factor(node.class)), size = 0.4, show.legend = FALSE) +
    #   geom_text(aes(label = node, color = factor(node.class)), 
    #             check_overlap = FALSE, hjust = "inward", vjust = -1,
    #             show.legend = FALSE, 
    #             family = "Lucida Console", cex = 2.75, fontface = "bold") +
    #   scale_color_manual(values = palette.text) +
    #   theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 14, hjust = 0.5, color = "gray40"),
    #         axis.line.x = element_line(size = 0.5),
    #         axis.line.y = element_line(size = 0.5),
    #         axis.text = element_text(family = "Lucida Sans Unicode", size = 14),
    #         axis.title = element_text(family = "Lucida Sans Unicode", size = 18, hjust = 0.5),
    #         plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in")) +
    #   geom_smooth(method = lm, linetype = 0, alpha = 0.1)
    #   #geom_abline(intercept = 0, slope = 1, alpha = 0.75, lty = 2)
    # 
    # # Save the plot
    # ggsave(file = title.par, figure.par, height = 6.5, width = 7, units = "in") 
    
    # NONPARAMETRIC STATS
    
    # Wilcoxon paired samples t-test
    ttest.nonpar <- wilcox.test(x.data, y.data, paired = TRUE)
    t.nonpar <- sprintf(ttest.nonpar$statistic, fmt = "%0.2f")
    t.p.nonpar <- ttest.nonpar$p.value * 12.0000
    print(ttest.nonpar)
    
    # Spearman correlation
    rtest.nonpar <- cor.test(x.data, y.data, method = "spearman")
    r.nonpar <- sprintf(rtest.nonpar$estimate, fmt = "%0.2f")
    r.p.nonpar <- rtest.nonpar$p.value * 12.0000
    print(rtest.nonpar)
    
    # Build the plot
    figure.nonpar <- ggplot(node.data, aes(x = x.data, y = y.data, label = node)) +
      theme_classic() +
      labs(x = paste("\n", xlab), y = paste(ylab, "\n"), title = paste("V = ", t.nonpar, get.sig(t.p.nonpar), "\n", "ρ = ", r.nonpar, get.sig(r.p.nonpar), sep = "")) +
      scale_x_continuous(limits = c(xmin, xmax)) +
      scale_y_continuous(limits = c(ymin, ymax)) +
      geom_point(aes(color = factor(node.class)), size = 0.4, show.legend = FALSE) +
      geom_text(aes(label = node, color = factor(node.class)), 
                check_overlap = FALSE, hjust = "inward", vjust = -1,
                show.legend = FALSE, 
                family = "Lucida Console", cex = 2.75, fontface = "bold") +
      scale_color_manual(values = palette.text) +
      theme(plot.title = element_text(family = "Lucida Sans Unicode", size = 14, hjust = 0.5, color = "gray40"),
            axis.line.x = element_line(size = 0.5),
            axis.line.y = element_line(size = 0.5),
            axis.text = element_text(family = "Lucida Sans Unicode", size = 14),
            axis.title = element_text(family = "Lucida Sans Unicode", size = 18, hjust = 0.5),
            plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in")) +
      geom_smooth(method = lm, linetype = 0, alpha = 0.1) +
      geom_abline(intercept = 0, slope = 1, alpha = 0.75, lty = 2)
    
    # Save the plot
    ggsave(file = title.nonpar, figure.nonpar, height = 6.5, width = 7, units = "in") 
    
  }
}

# Function for iterative scatter plotting over pairs of variables in a list
iter.scatter <- function(stat, stat.name, xmin, xmax, ymin, ymax) {
  stat.anat <- grep("anat", stat, value = TRUE)
  stat.behav <- grep("behav", stat, value = TRUE)
  stat.func <- grep("func", stat, value = TRUE)
  stat.path <- paste(nodes.path, "/", gsub(" ", "_", stat.name), sep = "")
  dir.create(stat.path)
  setwd(stat.path)
  lapply(stat.anat[1:(length(stat.anat)-1)], function(xvar) {
    lapply(stat.anat[2:length(stat.anat)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "anatomical", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
  lapply(stat.anat[2:length(stat.anat)], function(xvar) {
    lapply(stat.anat[1:(length(stat.anat)-1)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "anatomical", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
  lapply(stat.behav[1:(length(stat.behav)-1)], function(xvar) {
    lapply(stat.behav[2:length(stat.behav)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "behavioral", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
  lapply(stat.behav[2:length(stat.behav)], function(xvar) {
    lapply(stat.behav[1:(length(stat.behav)-1)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "behavioral", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
  lapply(stat.func[1:(length(stat.func)-1)], function(xvar) {
    lapply(stat.func[2:length(stat.func)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "functional", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
  lapply(stat.func[2:length(stat.func)], function(xvar) {
    lapply(stat.func[1:(length(stat.func)-1)], function(yvar) {
      scatter(xvar, yvar, stat.name, stat.path, input = "functional", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    })
  })
}

# Load unthresholded variables
unthresholded <- grep("thres", colnames(node.data), invert = TRUE, value = TRUE)

# Generate scatter plots of frequency
freq.path <- paste(path, "/plots/nodes/frequency", sep = "")
dir.create(freq.path)
scatter("frequency.in.abstracts", "frequency.in.results", "frequency", freq.path, xmin = 0, xmax = 725, ymin = 0, ymax = 3000)

# Generate scatter plots of number of edges
edges <- grep("edges", unthresholded, value = TRUE)
iter.scatter(edges, "number of edges", xmin = 0, xmax = 12500, ymin = 0, ymax = 12500)

# Generate scatter plots of degree
degree <- grep("degree.in", unthresholded, value = TRUE)
iter.scatter(degree, "degree", xmin = 0, xmax = 1, ymin = 0, ymax = 1)

# Generate scatter plots of betweenness
betweenness <- grep("betweenness.in", unthresholded, value = TRUE)
iter.scatter(betweenness, "betweenness", xmin = 0, xmax = 0.75, ymin = 0, ymax = 0.75)

# Generate scatter plots of betweenness current flow
# Range 0 - 0.12
betweenness.cf <- grep("betweenness.current.flow", unthresholded, value = TRUE)
iter.scatter(betweenness.cf, "betweenness current flow", xmin = -2.25, xmax = 2.25, ymin = -2.25, ymax = 2.25)

# Generate scatter plots of closeness
closeness <- grep("closeness.in", unthresholded, value = TRUE)
iter.scatter(closeness, "closeness", xmin = 0, xmax = 1, ymin = 0, ymax = 1)

# Generate scatter plots of closeness current flow
# Range 0-400 for coords anat, 0-3.5 for coords func (x and y)
# Range 0-0.8 for abstracts anat, 0-0.15 for abstracts func (x)
closeness.cf <- grep("closeness.current.flow", unthresholded, value = TRUE)
iter.scatter(closeness.cf, "closeness current flow", xmin = -2.25, xmax = 2.25, ymin = -2.25, ymax = 2.25)

# Generate scatter plots of closeness vitality
closeness.vit <- grep("closeness.vitality", unthresholded, value = TRUE)
iter.scatter(closeness.vit, "closeness vitality", xmin = 0, xmax = 250, ymin = 0, ymax = 250)

# Generate scatter plots of load
load <- grep("load", unthresholded, value = TRUE)
iter.scatter(load, "load", xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)

# Generate scatter plots of clustering
clustering <- grep("clustering", unthresholded, value = TRUE)
iter.scatter(clustering, "clustering", xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)


# Function for histograms
histo <- function(class, stat, stat.name, color, xmin = 0, xmax = 1, ymin = 0, ymax = 1) {
  qplot(node.data[,stat], geom = "histogram",
                  fill = I(color), xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
            labs(title = paste(gsub(" ", "\n", gsub("\\)", "", gsub("coordinates \\(", "", titular(stat, stat.name)))), "\n")) +
            theme_classic() +
            theme(plot.title = element_text(family = "Lucida Console", size = 18, hjust = 0.5),
                        axis.line.x = element_line(size = 0.5),
                        axis.line.y = element_line(size = 0.5),
                        axis.text = element_text(family = "Lucida Sans Unicode", size = 12),
                        axis.title = element_blank(),
                        legend.position = "none",
                        plot.margin = grid::unit(c(0.25, 0.5, 0.25, 0.25), "in"))
}

# Multiple plot function from https://stackoverflow.com/questions/24387376/r-weird-error-could-not-find-function-multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
require(grid)

# Make a list from the ... arguments and plotlist
plots <- c(list(...), plotlist)

numPlots = length(plots)

# If layout is NULL, then use 'cols' to determine layout
if (is.null(layout)) {
# Make the panel
# ncol: Number of columns of plots
# nrow: Number of rows needed, calculated from # of cols
layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
ncol = cols, nrow = ceiling(numPlots/cols))
}

if (numPlots==1) {
print(plots[[1]])

} else {
# Set up the page
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

# Make each plot, in the correct location
for (i in 1:numPlots) {
# Get the i,j matrix positions of the regions that contain this subplot
matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
layout.pos.col = matchidx$col))
}
}
}

# Generate histograms of betweenness
p1 <- histo("anat", "betweenness.in.coords_probabilistic_anat", "betweenness", adjustcolor("#E9AB17", 0.75), xmin = -0.1, xmax = 1, ymax = 45)
p2 <- histo("anat", "betweenness.in.coords_winner.takes.all_anat", "betweenness", adjustcolor("#E9AB17", 0.75), xmin = -0.1, xmax = 1, ymax = 45)
p3 <- histo("anat", "betweenness.in.coords_probabilistic.winner.takes.all_anat", "betweenness", adjustcolor("#E9AB17", 0.75), xmin = -0.1, xmax = 1, ymax = 45)
p4 <- histo("func", "betweenness.in.coords_probabilistic_func", "betweenness", adjustcolor ("#735787", 0.75), xmin = -0.1, xmax = 1, ymax = 60)
p5 <- histo("func", "betweenness.in.coords_winner.takes.all_func", "betweenness", adjustcolor ("#735787", 0.75), xmin = -0.1, xmax = 1, ymax = 60)
p6 <- histo("func", "betweenness.in.coords_probabilistic.winner.takes.all_func", "betweenness", adjustcolor ("#735787", 0.75), xmin = -0.1, xmax = 1, ymax = 60)
title = paste(nodes.path, "/betweenness/histograms_coords.png", sep = "")
png(filename = title, width = 7, height = 8.5, units = "in", res = 300)
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)
dev.off()

# Generate histograms of betweenness current flow
p1 <- histo("anat", "betweenness.current.flow.in.coords_probabilistic_anat", "betweenness current flow", adjustcolor("#E9AB17", 0.75), xmax = 0.15, ymax = 45)
p2 <- histo("anat", "betweenness.current.flow.in.coords_winner.takes.all_anat", "betweenness current flow", adjustcolor("#E9AB17", 0.75), xmax = 0.15, ymax = 45)
p3 <- histo("anat", "betweenness.current.flow.in.coords_probabilistic.winner.takes.all_anat", "betweenness current flow", adjustcolor("#E9AB17", 0.75), xmax = 0.15, ymax = 45)
p4 <- histo("func", "betweenness.current.flow.in.coords_probabilistic_func", "betweenness current flow", adjustcolor ("#735787", 0.75), xmax = 0.15, ymax = 60)
p5 <- histo("func", "betweenness.current.flow.in.coords_winner.takes.all_func", "betweenness current flow", adjustcolor ("#735787", 0.75), xmax = 0.15, ymax = 60)
p6 <- histo("func", "betweenness.current.flow.in.coords_probabilistic.winner.takes.all_func", "betweenness current flow", adjustcolor ("#735787", 0.75), xmax = 0.15, ymax = 60)
title = paste(nodes.path, "/betweenness_current_flow/histograms_coords.png", sep = "")
png(filename = title, width = 7, height = 8.5, units = "in", res = 300)
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)
dev.off()

# Generate histograms of closeness
p1 <- histo("anat", "closeness.in.coords_probabilistic_anat", "closeness", adjustcolor("#E9AB17", 0.75), xmax = 1.05, ymax = 45)
p2 <- histo("anat", "closeness.in.coords_winner.takes.all_anat", "closeness", adjustcolor("#E9AB17", 0.75), xmax = 1.05, ymax = 45)
p3 <- histo("anat", "closeness.in.coords_probabilistic.winner.takes.all_anat", "closeness", adjustcolor("#E9AB17", 0.75), xmax = 1.05, ymax = 45)
p4 <- histo("func", "closeness.in.coords_probabilistic_func", "closeness", adjustcolor ("#735787", 0.75), xmax = 1, ymax = 60)
p5 <- histo("func", "closeness.in.coords_winner.takes.all_func", "closeness", adjustcolor ("#735787", 0.75), xmax = 1, ymax = 60)
p6 <- histo("func", "closeness.in.coords_probabilistic.winner.takes.all_func", "closeness", adjustcolor ("#735787", 0.75), xmax = 1, ymax = 60)
title = paste(nodes.path, "/closeness/histograms_coords.png", sep = "")
png(filename = title, width = 7, height = 8.5, units = "in", res = 300)
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)
dev.off()

# Generate histograms of closeness current flow
p1 <- histo("anat", "closeness.current.flow.in.coords_probabilistic_anat", "closeness current flow", adjustcolor("#E9AB17", 0.75), xmax = 400, ymax = 45)
p2 <- histo("anat", "closeness.current.flow.in.coords_winner.takes.all_anat", "closeness current flow", adjustcolor("#E9AB17", 0.75), xmax = 400, ymax = 45)
p3 <- histo("anat", "closeness.current.flow.in.coords_probabilistic.winner.takes.all_anat", "closeness current flow", adjustcolor("#E9AB17", 0.75), xmax = 400, ymax = 45)
p4 <- histo("func", "closeness.current.flow.in.coords_probabilistic_func", "closeness current flow", adjustcolor ("#735787", 0.75), xmax = 3, ymax = 60)
p5 <- histo("func", "closeness.current.flow.in.coords_winner.takes.all_func", "closeness current flow", adjustcolor ("#735787", 0.75), xmax = 3, ymax = 60)
p6 <- histo("func", "closeness.current.flow.in.coords_probabilistic.winner.takes.all_func", "closeness current flow", adjustcolor ("#735787", 0.75), xmax = 3, ymax = 60)
title = paste(nodes.path, "/closeness_current_flow/histograms_coords.png", sep = "")
png(filename = title, width = 7, height = 8.5, units = "in", res = 300)
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)
dev.off()

## Generate histograms for abstracts
# Betweenness
p1 <- histo("anat", "betweenness.in.abstracts_anat", "betweenness", adjustcolor("#E9AB17", 0.75), xmin = -0.1, xmax = 1, ymax = 45)
p2 <- histo("func", "betweenness.in.abstracts_func", "betweenness", adjustcolor ("#735787", 0.75), xmin = -0.1, xmax = 1, ymax = 60)
title = paste(nodes.path, "/betweenness/histograms_abstracts.png", sep = "")
png(filename = title, width = 6.75, height = 3, units = "in", res = 300)
multiplot(p1, p2, cols = 2)
dev.off()
# Betweenness current flow
p1 <- histo("anat", "betweenness.current.flow.in.abstracts_anat", "betweenness current flow", adjustcolor("#E9AB17", 0.75), xmax = 0.15, ymax = 45)
p2 <- histo("func", "betweenness.current.flow.in.abstracts_func", "betweenness current flow", adjustcolor ("#735787", 0.75), xmax = 0.15, ymax = 60)
title = paste(nodes.path, "/betweenness_current_flow/histograms_abstracts.png", sep = "")
png(filename = title, width = 6.75, height = 3, units = "in", res = 300)
multiplot(p1, p2, cols = 2)
dev.off()
# Closeness
p1 <- histo("anat", "closeness.in.abstracts_anat", "closeness", adjustcolor("#E9AB17", 0.75), xmax = 1.05, ymax = 45)
p2 <- histo("func", "closeness.in.abstracts_func", "closeness", adjustcolor ("#735787", 0.75), xmax = 1, ymax = 60)
title = paste(nodes.path, "/closeness/histograms_abstracts.png", sep = "")
png(filename = title, width = 6.75, height = 3, units = "in", res = 300)
multiplot(p1, p2, cols = 2)
dev.off()
# Closeness current flow
p1 <- histo("anat", "closeness.current.flow.in.abstracts_anat", "closeness current flow", adjustcolor("#E9AB17", 0.75), xmax = 1, ymax = 45)
p2 <- histo("func", "closeness.current.flow.in.abstracts_func", "closeness current flow", adjustcolor ("#735787", 0.75), xmax = 0.15, ymax = 60)
title = paste(nodes.path, "/closeness_current_flow/histograms_abstracts.png", sep = "")
png(filename = title, width = 6.75, height = 3, units = "in", res = 300)
multiplot(p1, p2, cols = 2)
dev.off()

