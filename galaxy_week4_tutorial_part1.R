# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})


# Import data  ------------------------------------------------------------

results <- read.csv("limma-voom_basalpregnant-basallactate.csv")


# Format data  ------------------------------------------------------------

# Create columns from the column numbers specified and use the existing category_symbol column for shapes
results <- results %>% mutate(
  fdr = .[[8]],
  pvalue = .[[7]],
  logfc = .[[4]],
  labels = .[[2]],
)

# Check if shape_col is provided 

# Get names for legend
down <- unlist(strsplit('Down,Not Sig,Up', split = ","))[1]
notsig <- unlist(strsplit('Down,Not Sig,Up', split = ","))[2]
up <- unlist(strsplit('Down,Not Sig,Up', split = ","))[3]

# Set colours
# colours <- setNames(c("cornflowerblue", "grey", "firebrick"), c(down, notsig, up))
colours <- setNames(c("purple", "grey", "orange"), c(down, notsig, up))

# Create significant (sig) column
results <- mutate(results, sig = case_when(
  fdr < 0.01 & logfc > 0.58 ~ up,
  fdr < 0.01 & logfc < -0.58 ~ down,
  TRUE ~ notsig))


# Specify genes to label --------------------------------------------------

# Get top genes by P value
top <- slice_min(results, order_by = pvalue, n = 10)

# Extract into vector
toplabels <- pull(top, labels)

# Label just the top genes in results table
results <- mutate(results, labels = ifelse(labels %in% toplabels, labels, ""))


# Create plot -------------------------------------------------------------

# Open file to save plot as PDF
# pdf("volcano_plot.pdf") # PART 1 OF 2: UNCOMMENT THIS TO GET THE PDF FILE

# Set up base plot with faceting by category_symbol instead of shapes
p <- ggplot(data = results, aes(x = logfc, y = -log10(pvalue))) +
  scale_color_manual(values = colours) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank()) 

# Conditional logic to use either shape or facet based on user selection
# p <- p + geom_point(aes(colour = sig)) #only add color
p <- p + geom_point(aes(colour = sig), size = 0.5) #only add color

# Add gene labels
# p <- p + geom_text_repel(data = filter(results, labels != ""), aes(label = labels),
#                          min.segment.length = 0,
#                          max.overlaps = Inf,
#                          show.legend = FALSE)
p <- p + geom_text_repel(data = filter(results, labels != ""), 
                         aes(label = labels),
                         size = 3)






# Set legend title
# p <- p + theme(legend.title = element_blank()) # this says not title for legend


# Adding title
p <- p + labs(title = 'Informative Title')

# Print plot
print(p)

# Close PDF graphics device
# dev.off() # PART 2 OF 2: UNCOMMENT THIS TO GET THE PDF FILE


# R and Package versions -------------------------------------------------
sessionInfo()

