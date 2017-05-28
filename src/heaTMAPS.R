heatmap(as.matrix(mall_sc), scale="none", cexCol=2, cexRow=0.5)

require("RColorBrewer")
col <- colorRampPalette(brewer.pal(12, "Set3"))(301)
heatmap(as.matrix(mall_sc), scale = "row", col = col, cexRow=1, 
            RowSideColors = c(rep("blue", 100), rep("green", 101), rep("cyan", 100)),
            ColSideColors = c(rep("yellow", 3), rep("orange", 3), rep("blue", 3)))
            