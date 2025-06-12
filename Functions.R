# this function creates subdirectories to start a project:
# filePath - main directory of where you want your folders to be created

# testPath <- "/mnt/data/megan/test"
startProjDir <- function(filePath){
  mainDir <- filePath
  subDir <- c("data", "rds", "figures")
  for (i in subDir) {
    ifelse(!dir.exists(file.path(mainDir, i)),
           dir.create(file.path(mainDir, i)),
           print(paste(i, "folder already exists")))
  }
  
}

# filters top genes from a DESeq analysis using baseMean and log2FC thresholds
# dfList = list; a list of DESEq2 results converted into a dataframe
# pvalue, L2FC = integer; numerical thresholds for pvalue and log2FoldChange, respectively

diff_expressed <- function(dfList, padjust, L2FC){
  
  results <- list()
  
  for (i in 1:length(dfList)) {
    # categorise to upregulated and downregulated genes
    y <- dfList[[i]] %>%
      mutate(diffexpressed = case_when(
        log2FoldChange > L2FC & padj < padjust ~ 'UP',
        log2FoldChange < (L2FC*-1) & padj < padjust ~ 'DOWN',
        padj > padjust ~ 'NO',
        .default = 'NO')) # none of the cases match
    
    results[[paste0(names(dfList[i]))]] <- y
  }
  
  return(results)
}

diff_expr2 <- function(df, padjust, L2FC){
  
  # categorise to upregulated and downregulated genes
  y <- df %>%
    mutate(diffexpressed = case_when(
      log2FoldChange > L2FC & padj < padjust ~ 'UP',
      log2FoldChange < (L2FC*-1) & padj < padjust ~ 'DOWN',
      padj > padjust ~ 'NO',
      .default = 'NO')) # none of the cases match
  
  return(y)
}


# Function to reformat DESeq results table for enrichment analyses
cProfilr_input <- function(df){
  input <- list()
  
  # we want the log2 fold change
  original_gene_list <- df$log2FoldChange
  
  # name the vector
  names(original_gene_list) <- rownames(df)
  
  # omit any NA values
  gene_list<-na.omit(original_gene_list)
  
  # sort the list in decreasing order (required for clusterProfiler)
  gene_list = sort(gene_list, decreasing = TRUE)
  
  # Exctract significant results (padj < 0.05)
  sig_genes_df = subset(df, diffexpressed != "NO")
  
  # From significant results, we want to filter on log2fold change
  genes <- sig_genes_df$log2FoldChange
  
  # Name the vector
  names(genes) <- rownames(sig_genes_df)
  
  # omit NA values
  genes <- na.omit(genes)
  
  genes <- sort(genes, decreasing = TRUE)
  
  input[["signif_genes"]] <- genes
  input[["all_gene_list"]] <- gene_list
  
  return(input)
}

# set ScoreType for fgsea ("std" if neg and pos values present; "pos" if only pos; "neg" if only neg)
score_test <- function(gsea_data_vec){
  if(min(gsea_data_vec) < 0 & max(gsea_data_vec) > 0){
    type <- "std"
  } else if(min(gsea_data_vec) < 0 & max(gsea_data_vec) < 0){
    type <- "neg"
  } else{
    type <- "pos"
  }
  return(type)
}

# GSEA function
# source: https://bioinformaticsbreakdown.com/how-to-gsea/
GSEA = function(gene_list, pathway, pval, condition_name) {
  set.seed(54321)
  library(dplyr)
  library(fgsea)
  
  if ( any( duplicated(names(gene_list)) )  ) {
    warning("Duplicates in gene names")
    gene_list = gene_list[!duplicated(names(gene_list))]
  }
  if  ( !all( order(gene_list, decreasing = TRUE) == 1:length(gene_list)) ){
    warning("Gene list not sorted")
    gene_list = sort(gene_list, decreasing = TRUE)
  }
  
  fg <- fgsea::fgsea(pathways = pathway,
                     stats = gene_list,
                     minSize=10, ## minimum gene set size
                     nPermSimple = 10000) %>%
    as.data.frame()
  
  
  fgRes <- fg %>%
    dplyr::filter(padj < !!pval) %>%
    arrange(desc(NES))
  
  message(paste("Number of signficant gene sets =", nrow(fgRes)))
  
  # message("Collapsing Pathways -----")
  # concise_pathways = collapsePathways(data.table::as.data.table(fgRes),
  #                                     pathways = pathway,
  #                                     stats = gene_list)
  #
  # fgRes = fgRes[fgRes$pathway %in% concise_pathways$mainPathways, ]
  # message(paste("Number of gene sets after collapsing =", nrow(fgRes)))
  
  fgRes$Enrichment = ifelse(fgRes$NES > 0, "Up-regulated", "Down-regulated")
  filtRes = rbind(head(fgRes, n = 15),
                  tail(fgRes, n = 15))
  
  total_up = sum(fgRes$Enrichment == "Up-regulated")
  total_down = sum(fgRes$Enrichment == "Down-regulated")
  path_info = paste0("Top 30 (Total pathways: Up=", total_up,", Down=",    total_down, ")")
  
  fig_name = paste0(condition_name,":")
  
  colors = setNames(c("red3", "darkblue"),
                    c("Up-regulated", "Down-regulated"))
  
  theme_set(theme_classic())
  #My_Theme = theme(title=element_text(size=15, face='bold'))
  
  g1 = ggplot(filtRes, aes(reorder(pathway, NES), NES)) +
    geom_point( aes(fill = Enrichment, size = size), shape=21) +
    scale_fill_manual(values = colors ) +
    scale_size_continuous(range = c(2,10)) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    labs(x="Pathway", y=paste0("Normalized Enrichment Score (NES): ", path_info),
         title = fig_name,
         subtitle = "Reactome Pathways NES from GSEA") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5))
  
  g2 = ggplot(filtRes, aes(reorder(pathway, NES), NES)) +
    geom_col(aes(fill=Enrichment)) +
    scale_fill_manual(values = colors ) +
    coord_flip() +
    labs(x="Pathway", y=paste0("Normalized Enrichment Score (NES): ", path_info),
         title = fig_name,
         subtitle = "Reactome Pathways NES from GSEA") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5))
  
  output = list("Results" = fg, "Plot1" = g1, "Plot2" = g2)
  return(output)
}
