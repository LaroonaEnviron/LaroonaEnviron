require(ggcorrplot)

# structure of dataframe soils
str(soils[, 1:11])

# derive correlation coeeficient matrix and display
corr <- round(cor(soils[, 1:11]), 2)
head(corr[, 1:11], 11)

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)

# method = "circle"
ggcorrplot(corr, method = "circle")

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "black")

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = TRUE, type = "upper", p.mat = cor_pmat(soils[, 1:11]), lab=TRUE, insig = "blank", ggtheme=theme_bw, legend.title="Correlation\nCoefficients", lab_size=3, show.diag=TRUE, title="Correlation Coeeficients of Mallee and Barossa Soils")
