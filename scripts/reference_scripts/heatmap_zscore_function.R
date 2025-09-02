###
###
###


### Heatmap Sean's function
# Heat map funciton
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
                              .SDcols = c(tax_level, "OTU")], ## Make sure the seq are "ASV"
                    by = "OTU",
                    all.x = T)
    
    otu_key[, OTU := NULL]
    
    # sum by tax level
    otu_sum = otu_key[, lapply(.SD, sum), by = tax_level] #### maybe code crashes here
    ## Aggregates data by family
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



### Rayna's z-score function 

zscore.calculation = function(x) {
  #Inputs: x = dummy variable that function will be run on. Must be a vector of numbers.
  
  zscoredat=x #duplicate vector x, save as a new df zscoredat.
  
  for (i in 1:length(x)) { #for each value in vector x
    zscoredat[i]=(x[i]-mean(x))/sd(x) #subset the input data to the corresponding position, and make it equal to the zscored value of the original cell value at that position.
    #zscoring is done by subtracting the mean value for a vector from the value of the vector at position i, and then dividing by the SD for the vector.
  }
  zscoredat1 <<- zscoredat #save output as new df, zscoredat1
}





