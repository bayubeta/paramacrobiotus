# load the required libraries
suppressPackageStartupMessages(library(ComplexHeatmap))
library(gridExtra)

# ------------------------------- load the data --------------------------------
# preprocess species data
species <- read.table("species.xls", header = TRUE)
species$Tax_detail <- c() # remove taxa information
species[,-1] <- data.matrix(species[,-1]) # convert the numbers into numeric
rownames(species) <- species$Taxonomy # set rownames from the taxonomy column
species <- species[,-1] # remove the first column
species <- species[-dim(species)[1],] # remove the last row (total)

# preprocess genus data
genus <- as.data.frame(readxl::read_xlsx("genus.xlsx"))
genus$Tax_detail <- c() # remove taxa information
genus[,-1] <- data.matrix(genus[,-1]) # convert the numbers into numeric
rownames(genus) <- genus$Taxonomy # set rownames from the taxonomy column
genus <- genus[,-1] # remove the first column
genus <- genus[-dim(genus)[1],] # remove the last row (total)

# name of the stages
stages_name <- c("egg", "active", "tuns7", "active7", 
                 "tuns120", "active120", "dead120")
Pe_names <- sapply(stages_name, function(x){paste0("Pe_", x)})
Pf_names <- sapply(stages_name, function(x){paste0("Pf_", x)})

# ------------------------------ helper functions ------------------------------
# function to translate to discrete levels
val2lvl <- function(val){
  sum(val >= c(0,1,100, 1000, 10000))
}

# function to set the color of the numbers shown on the stages average heatmap
font_col <- function(lvl){
  if (lvl %in% c(3,5)){
    return("white")
  }else{
    return("black")
  }
}


# ---------------------- main function to plot the heatmap ---------------------
plotTunStage <- function(df){
  
  mainString <- ifelse(rownames(df)[1] == "Paracoccus_yeei", 
                       "Species level", "Genus level")
  
  M <- as.matrix(df)
  
  # ================== find the average of tun stages 
  # ======= for param. exp.
  tuns <- apply(M, 1, function(x){mean(x[c(3,5, 10, 12)])})
  non_tuns <- apply(M, 1, function(x){mean(x[-c(3,5, 10, 12)])})
  
  # find an ordering from the highest mean tun stage to lowest
  ord <- names(sort(tuns[tuns > non_tuns], decreasing = TRUE))
  
  # create a separate matrix for Pe and Pf
  M_pe <- M[tuns > non_tuns, 1:7]
  M_pe <- M_pe[ord,]
  colnames(M_pe) <- Pe_names
  
  M_pf <- M[tuns > non_tuns, (1:7)+7]
  M_pf <- M_pf[ord,]
  colnames(M_pf) <- Pf_names
  
  # stages average matrix
  M_avg <- cbind(tuns[ord], non_tuns[ord])
  colnames(M_avg) <- c("Tun stages", "Other stages")
  
  # adjust the fonts for texts
  column_names_gp <- gpar(fontsize = 10, fontface = "italic", 
                          fontfamily = "serif")
  column_title_gp <- gpar(fontface = "italic", fontfamily = "serif")
  row_names_gp <- gpar(fontsize = 8, fontface = "italic", fontfamily = "serif")
  
  
  # convert M_pe, M_pf, M_avg to discrete matrices of 5 levels
  M_pe_disc <- matrix(nrow = nrow(M_pe), ncol = ncol(M_pe))
  M_pe_disc[] <- vapply(X = M_pe, FUN = val2lvl, FUN.VALUE = numeric(1))
  colnames(M_pe_disc) <- Pe_names
  rownames(M_pe_disc) <- ord
  
  M_pf_disc <- matrix(nrow = nrow(M_pf), ncol = ncol(M_pf))
  M_pf_disc[] <- vapply(X = M_pf, FUN = val2lvl, FUN.VALUE = numeric(1))
  colnames(M_pf_disc) <- Pf_names
  rownames(M_pf_disc) <- ord
  
  M_avg_disc <- matrix(nrow = nrow(M_avg), ncol = ncol(M_avg))
  M_avg_disc[] <- vapply(X = M_avg, FUN = val2lvl, FUN.VALUE = numeric(1))
  colnames(M_avg_disc) <- c("Tun stages", "Other stages")
  rownames(M_avg_disc) <- ord
  
  # list of colors
  col_list <- structure(c("white", "lightskyblue1", 
                          "royalblue3", "orange", "red2"), 
                        names = c("1", "2", "3", "4", "5"))
  # heatmap for Pe
  pushViewport(viewport(gp = gpar(fontfamily = "serif")))
  hm_Pe <- Heatmap(M_pe_disc, cluster_rows = FALSE, cluster_columns = FALSE,
                   col = col_list, 
                   column_title_gp = column_title_gp,
                   show_heatmap_legend = FALSE,
                   rect_gp = gpar(col = "grey90", lwd = 0.1),
                   row_names_gp = row_names_gp, 
                   column_names_gp = column_names_gp,
                   column_names_rot = 90, column_title = "Pe.")
  
  # heatmap for Pf
  hm_Pf <- Heatmap(M_pf_disc, cluster_rows = FALSE, cluster_columns = FALSE,
                   col = col_list, 
                   column_title_gp = column_title_gp,
                   show_heatmap_legend = FALSE,
                   rect_gp = gpar(col = "grey90", lwd = 0.1),
                   row_names_gp = row_names_gp, 
                   column_names_gp = column_names_gp,
                   column_names_rot = 90, column_title = "Pf.")
  
  # heatmap for avg
  hm_avg <- Heatmap(M_avg_disc, cluster_rows = FALSE, cluster_columns = FALSE,
                    column_title_gp = gpar(fontsize = 10, fontfamily = "serif"), 
                    column_title = "Stages average",
                    col = col_list, column_names_rot = 90, 
                    column_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
                    show_heatmap_legend = FALSE, 
                    rect_gp = gpar(col = "grey90", lwd = 0.1),
                    row_names_gp = row_names_gp,
                    cell_fun = function(j,i,x,y,w,h,col){
                      grid.text(M_avg[i,j], x, y, 
                                gp = gpar(fontsize = 8, 
                                          col = font_col(M_avg_disc[i,j])))},
                    width = unit(4, "cm"))
  
  h1 <- grid.grabExpr(draw(hm_Pe + hm_Pf + hm_avg, 
                           padding = unit(c(0.2,0.2,0.5,6), "cm"), 
                           ht_gap = unit(0.5, "cm"), newpage = FALSE))
  
  grid.arrange(h1, 
               top = textGrob(mainString, 
                              gp = gpar(fontsize=24, fontface = "bold",
                                        fontfamily = "serif")))
  lgd <- Legend(labels = c("0", 
                          expression("">=1 ~", "~ ""<100), 
                          expression("">=100 ~", "~ ""<1000), 
                          expression("">=1000 ~", "~ ""<10000), 
                          expression("">=10000)),
               legend_gp = gpar(fill = col_list),
               title_gp = gpar(fontfamily = "serif"),
               border = "black",
               title = "num. of sequences", grid_height = unit(1, "cm"))
  draw(lgd, x = unit(0.95, "npc"), just = "right")
}


# plot the heatmap, optionally save to pdf
# pdf("tun_species_level_v2.pdf", height = 15, width = 15)
plotTunStage(species)
# dev.off()

# pdf("tun_genus_level_v2.pdf", height = 15, width = 15)
plotTunStage(genus)
# dev.off()
