##' @importFrom ggplot2 fortify
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 geom_point
##' @importFrom ggplot2 scale_color_gradient
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 ggtitle
##' @author Guangchuang Yu, Vitalii Kleshchevnikov modified to order and color by any column and give custom message
##' on the plot terms are ordered high to low, top to bottom
dotplot_internal <- function(object, x="geneRatio", colorBy="p.adjust", orderBy = "GeneRatio", showCategory=10, split=NULL, font.size=12, title="", xlabel = "") {
  colorBy <- match.arg(colorBy, c("pvalue", "p.adjust", "qvalue", "enrichmentScore"))
  if (x == "geneRatio" || x == "GeneRatio") {
    x <- "GeneRatio"
    size <- "Count"
  } else if (x == "count" || x == "Count") {
    x <- "Count"
    size <- "GeneRatio"
  } else {
    stop("x should be geneRatio or count...")
  }
  df <- fortify(object, showCategory = showCategory, split=split)
  ## already parsed in fortify
  ## df$GeneRatio <- parse_ratio(df$GeneRatio)
  if(colorBy == "enrichmentScore") {lows = "blue"; highs = "red"}
  if(colorBy != "enrichmentScore") {lows = "red"; highs = "blue"}
  
  idx <- order(df[,orderBy], decreasing = FALSE)
  df$Description <- factor(df$Description, levels=df$Description[idx])
  ggplot(df, aes_string(x=x, y="Description", size=size, color=colorBy)) +
    geom_point() + scale_color_gradient(low=lows, high=highs) +
    ggtitle(title) + theme_dose(font.size) + 
    ylab(ifelse(orderBy == "p.adjust",">> adjusted p-value increasing >>", ""))+
    xlab(ifelse(x == "GeneRatio" & class(object) == "gseaResult" & xlabel != "",
                xlabel, # the fraction of proteins from a gene set which are over- or underrepresented
                ifelse(x == "GeneRatio" & class(object) == "enrichResult" & xlabel != "",
                       xlabel, # the fraction of proteins from a gene set in the analysed set
                       x)))
}