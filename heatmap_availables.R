# load the required libraries
suppressPackageStartupMessages(library(ComplexHeatmap))

# ------------------------------- load the data --------------------------------
species <- read.table("species.xls", header = TRUE)
species$Tax_detail <- c()
species[,-1] <- data.matrix(species[,-1])
rownames(species) <- species$Taxonomy
species <- species[,-1]
species <- species[-dim(species)[1],]


genus <- data.frame(readxl::read_xlsx("genus.xlsx"))
genus$Tax_detail <- c()
genus <- genus[-dim(genus)[1],]
rownames(genus) <- genus$Taxonomy
genus <- genus[,-1]
genus <- genus[-dim(genus)[1],]


# ------------------------------- stages names -------------------------------
stages_name <- c("egg", "active", "tuns7", "active7", 
                 "tuns120", "active120", "dead120")
Pe_names <- sapply(stages_name, function(x){paste0("Pe_", x)})
Pf_names <- sapply(stages_name, function(x){paste0("Pf_", x)})



# ==============================================================================
# -------------------------- begin plotting routines --------------------------- 
# ------------- species level data: find species that appear everywhere
# nonzeros indices, species
nzs <- apply(species, 1, function(x){all(x > 0)})

# sort based on euclidean distance to 0
dist_s <- apply(species[nzs,], 1, function(x){sqrt(sum(x^2))})
ord_s <- order(dist_s, decreasing = TRUE)

# new data frame based on the ordering
sp_new <- species[nzs,][ord_s,]

# ------------- begin heatmap
# color function for scale
col_fun <- circlize::colorRamp2(c(0, 10000, 50000), 
                                c("blue4", "white", "darkorange2"))

# matrix for heatmap
M <- as.matrix(sp_new)
colnames(M) <- c(Pe_names, Pf_names)

pushViewport(viewport(gp = gpar(fontfamily = "serif")))
hm <- Heatmap(M, cluster_rows = FALSE, cluster_columns = FALSE,
              column_split = rep(c("Pe", "Pm"), each = 7),
              col = col_fun(seq(0, 50000, length.out = 100)), 
              column_title_gp = gpar(fontface = "italic"),
              rect_gp = gpar(col = "black", lwd = 0.1),
              row_names_gp = gpar(fontsize = 8, fontface = "italic"), 
              column_names_gp = gpar(fontsize = 10, fontface = "italic"),
              column_names_rot = 90,
              heatmap_legend_param = list(title = "sequences", 
                                          legend_height = unit(6, "cm")))

# optional: save as pdf
# pdf("species_level.pdf")
draw(hm, padding = unit(c(0.2,0.2,0.5,0.2), "cm"), 
     column_title = gt_render("Species level", 
                              padding = unit(c(0,0,0.5,0), "cm")), 
     column_title_gp = gpar(fontsize = 20, fontface = "bold"), newpage = FALSE)
# dev.off()


# ------------- genus level data: find species that appear everywhere
# genus data: find genus that appear everywhere
nzg <- apply(genus, 1, function(x){all(x > 0)})

# sort based on euclidean distance to 0
dist_g <- apply(genus[nzg,], 1, function(x){sqrt(sum(x^2))})
ord_g <- order(dist_g, decreasing = TRUE)

# new dataframe based on the ordering
gn_new <- genus[nzg,][ord_g,]

# ------------- begin heatmap
# color function for scale
col_fun2 = circlize::colorRamp2(c(0, 50000, 250000), 
                                c("blue4", "white", "darkorange2"))

# matrix for heatmap
M2 <- as.matrix(gn_new)
colnames(M2) <- c(Pe_names, Pf_names)

pushViewport(viewport(gp = gpar(fontfamily = "serif")))
hm2 <- Heatmap(M2, cluster_rows = FALSE, cluster_columns = FALSE,
               column_split = rep(c("Pe", "Pm"), each = 7),
               col = col_fun(seq(0, 50000, length.out = 100)), 
               column_title_gp = gpar(fontface = "italic"),
               rect_gp = gpar(col = "black", lwd = 0.1),
               row_names_gp = gpar(fontsize = 8, fontface = "italic"), 
               column_names_gp = gpar(fontsize = 10, fontface = "italic"),
               column_names_rot = 90,
               heatmap_legend_param = list(title = "sequences", 
                                          legend_height = unit(6, "cm")))

# optional: save as pdf
# pdf("genus_level.pdf")
draw(hm2, padding = unit(c(0.2,0.2,0.5,0.2), "cm"), 
     column_title = gt_render("Genus level", padding = unit(c(0,0,0.5,0), "cm")), 
     column_title_gp = gpar(fontsize = 20, fontface = "bold"), newpage = FALSE)
# dev.off()

