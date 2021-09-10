library(data.table) # for the 'transpose' function
library(ggplot2)

# Load the Wahl-O-Mat data:
data <- read.csv('wahl-o-mat-bundestagswahl-2021.csv',
                 header=TRUE, sep="\t", dec=".", comment.char = "#")

# Decide whether to show all parties or just the 6 major ones:
all_parties <- FALSE
if (all_parties) {
  # Remove the first column (the one with the numbers of the positions):
  data <- data[,-1]
  # German Party Colors:
  parties <- c("CDU", "SPD", "AFD", "FDP", "LIN", "GRU", "FRW", "PAR", "TSP", "NPD", "PIR", "ODP")
  party_colors <- c("black", "red", "blue", "yellow", "darkviolet", "green", "darkorange", "darkred", "darkblue", "saddlebrown", "orange", "orange")
  # Hint: See http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf for all colors in R.
  party_text_colors <- c("white", "white", "white", "black", "white", "black", "black", "white", "white", "white", "black", "black")
  parties_with_colors <- data.frame(parties, colors)
} else {
  # Only keep the columns 2 to 7:
  data <- data[,2:7]
  # German Party Colors:
  parties <- c("CDU", "SPD", "AFD", "FDP", "LIN", "GRU")
  party_colors <- c("black", "red", "blue", "yellow", "darkviolet", "green")
  party_text_colors <- c("white", "white", "white", "black", "white", "black")
  parties_with_colors <- data.frame(parties, colors)
}

# Standardize the data:
data <-as.data.frame(scale(data))

# Transpose:
t_data <- transpose(data)
colnames(t_data) <- rownames(data)
rownames(t_data) <- colnames(data)
data <- t_data

# Output the prepared data set:
print(data)

# Principal Component Analysis (PCA):
pca <- prcomp(data,scale=FALSE) # columns are already scaled to unit variance

# Print the summary of the PCA:
summary(pca)

# Plot the PCA:

# 2D plot:
proj <- as.data.frame(pca$x) 
ggplot(proj, aes(PC1, PC2, colour = PC3)) +
  geom_point() +
  geom_text(aes(label=rownames(proj)),hjust="middle", vjust="bottom") +
  labs(caption = "LIN=Linke, GRU=Gruene, FRW=Freie Waehler, PAR=Die Partei, TSP=Tierschutzpartei, PIR=Piratenpartei")

# 1D plot (cf. https://stackoverflow.com/questions/3798676/how-can-i-plot-a-1-d-plot-in-r):
proj <- data.frame(pca$x[,"PC1"])
colnames(proj) <- c("PC1")
print(proj)
min_PC1 <- min(proj$PC1)*1.1
max_PC1 <- max(proj$PC1)*1.1
ggplot(proj, aes(x=PC1, y=0)) +
  annotate("segment",x=min_PC1,xend=max_PC1, y=0, yend=0, size=2) +
  annotate("segment",x=min_PC1,xend=min_PC1, y=-0.1,yend=0.1, size=2) +
  annotate("segment",x=max_PC1,xend=max_PC1, y=-0.1,yend=0.1, size=2) +
  annotate("segment",x=0,xend=0, y=-0.1,yend=0.1, size=2) +
  geom_point(size = 10, colour=party_colors) +
  geom_text(aes(label = rownames(proj)), col=party_text_colors) +
  scale_x_continuous(limits = c(min_PC1,max_PC1)) +
  scale_y_continuous(limits = c(-1,1)) +
  scale_color_manual(values = unname(colours)) + 
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  labs(title="PC1 Wahl-O-Mat Major Parties",
       caption = sprintf("The line ranges from %f to %f.", min_PC1, max_PC1))
# Note: pca$rotation[,"PC1"] will show the weights used to calculate PC1
