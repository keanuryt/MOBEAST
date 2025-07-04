library(ComplexHeatmap)
library(data.table)

#setwd("C:\\Users\\Devin\\Documents\\thesis\\KM2315dna\\stats\\")


# Color pallete

cast_colors = c(
  #Cast
  'A1' = "#1A1A1A",
  'B1' = "#4CBB17",
  'C1' = "#0047AB",
  'A2' = "#4A4A4A",
  'B2' = "#48872B",
  'C2' = "#0066FF"
)


  #Depth
depth_colors =c(
  '5' = "#e3342f",
  '25' = "#f6993f", 
  '45' = "#ffed4a",
  '75' = "#38c172",
  'DCM' = "#4dc0b5",
  '100' = "#3490dc",
  '125' = "#6574cd",
  '150' = "#9561e2",
  '175' = "#873260",
  '250' = "#290245"
  
)

station_colors = c(
  #Station
  'A' = "black",
  'B' = "#009915",
  'C' = "#0030FF"
)



############
# Functions ---------------
############

# function to sum by family
prep_ht_mat = function(otus,
                       tax_table,
                       tax_level = "family",
                       cutoff = 40,
                       min_sam = 2,
                       scale_vals = F){
  if(tax_level == "OTU"){
    otu_sum = otus
    
  }else{
    otu_key = merge(otus,
                    tax_table[,
                              .SD,
                              .SDcols = c(tax_level, "OTU")],
                    by = "OTU",
                    all.x = T)
    
    otu_key[, OTU := NULL]
    
    # sum by tax level
    otu_sum = otu_key[, lapply(.SD, sum), by = tax_level]
  }
  
  # order by most abundant taxa and subset to top N
  otu_sum[ , tax_sums := rowSums(otu_sum[ , .SD, .SDcols = !tax_level])]
  otu_sum = otu_sum[tax_sums > 0]
  otu_sum = otu_sum[order(tax_sums, decreasing = T)]
  otu_sum[ , tax_sums := NULL]
  otu_sum = otu_sum[1:cutoff,]
  
  # transform data.table to matrix
  otu_mat = as.matrix(otu_sum[, .SD,
                              .SDcols = !tax_level],
                      rownames.value = otu_sum[[tax_level]])
  
  # drop taxa that don't meet min sample count
  keep_tax = apply(otu_mat, 1, function(x)
    length(x[x > 0]) > min_sam)
  otu_mat = otu_mat[keep_tax, ]
  
  # optionally z-score a.k.a. scale values
  if (isTRUE(scale_vals)) {
    otu_mat = t(scale(t(otu_mat)))
  }
  
  return(otu_mat)
}

###############
# Code to run ------------------------------------
##############


# read in our data
sample_sheet = fread("hogate_thesis_mapping_file2.csv")
otu = fread("output/km2315_round1_otu_table.csv")
tax = fread("output/km2315_round1_taxonomy.csv")


# Set the order of the variables
sample_sheet$Cast <- factor(sample_sheet$Cast, levels = c("C1", "C2", "B1", "B2", "A1", "A2"))
sample_sheet$Station <- factor(sample_sheet$Station, levels = rev(c("C", "B", "A")))
sample_sheet$Depth <- factor(sample_sheet$Depth, levels = rev(c("250", "175", "150", "125", "100", "DCM", "75", "45", "25", "5")))



# filtering and organization
cruise_samples = sample_sheet[Depth != "-999" & Cast != "Deep", collection]
cruise_samples = cruise_samples[cruise_samples %in% colnames(otu)]
cruise_otu = otu[, .SD, ,.SDcols = c("OTU",cruise_samples)]



# get family summed matrix for top 40 most relatively abundant families
cruise_mat_nd = prep_ht_mat(otu = cruise_otu,
                                  tax_table = tax,
                                  tax_level = "family",
                                  cutoff = 40,
                                  scale_vals = T)

collections_by_depth_and_station = sample_sheet[collection %in% colnames(cruise_mat_nd)][order(Depth,Station),collection]

cruise_mat_nd = cruise_mat_nd[,collections_by_depth_and_station]

# identify samples in the summed family matrix by depth and station
cruise_depth = sample_sheet[match(colnames(cruise_mat_nd),
                                  sample_sheet$collection),
                            Depth]
cruise_station = sample_sheet[match(colnames(cruise_mat_nd),
                                    sample_sheet$collection),
                              Station]


###########
# Heatmap 1: Grouped by depth group (ML, DCM, Mesopelagic) --------------
############

# make heatmap annotation


depth_and_station_anno = HeatmapAnnotation(
  Depth = cruise_depth,
  Station = cruise_station,
  col = list(Depth = depth_colors,
             Station = station_colors),
  show_annotation_name = F,
  annotation_legend_param = list(
    labels_gp = gpar(fontsize = 7),
    title_gp = gpar(fontsize = 7)
  )
)


# Look up the station for each collection
# Note: collection column in the sample sheet matches the column names of the matrix [Group, Group2]
groups = sample_sheet[ match(colnames(cruise_mat_nd), sample_sheet$collection), "Group2"]

#organize the features and labeling of the heatmap
ht_depth_groups = Heatmap(
  matrix = cruise_mat_nd,
  bottom_annotation = depth_and_station_anno,
  row_names_gp = gpar(fontsize = 8),
  #row_order = order(as.numeric(rownames(cruise_depth)))))
  show_column_names = F,
  column_title = "KM2315 Heatmap",
  column_split = groups,
  heatmap_legend_param = list(
    labels_gp = gpar(fontsize = 7),
    title_gp = gpar(fontsize = 7),
    title = "Scaled RA"
  )
)

ht_depth_groups

pdf("heatmap_plots\\heatmap_depth_groups.pdf",
    width = 10,
    height = 8)

print(ht_depth_groups)

dev.off()

###########
# Heatmap 2: Grouped by depth group (ML, DCM, Mesopelagic) --------------
############


# Look up the station for each collection
# Note: collection column in the sample sheet matches the column names of the matrix [Group, Group2]
groups = sample_sheet[ match(colnames(cruise_mat_nd), sample_sheet$collection), "Depth"]

#organize the features and labeling of the heatmap
ht_depths = Heatmap(
  matrix = cruise_mat_nd,
  bottom_annotation = depth_and_station_anno,
  row_names_gp = gpar(fontsize = 8),
  #row_order = order(as.numeric(rownames(cruise_depth)))))
  show_column_names = F,
  column_title = "KM2315 Heatmap",
  column_split = groups,
  cluster_columns = F,
  heatmap_legend_param = list(
    labels_gp = gpar(fontsize = 7),
    title_gp = gpar(fontsize = 7),
    title = "Scaled RA"
  )
)

ht_depths

pdf("heatmap_plots/heatmap_depths.pdf",
    width = 10,
    height = 8)

print(ht_depths)

dev.off()
