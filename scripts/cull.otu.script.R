cull.abundance.prevalence=function(relabund.df, min.num, min.abund, min.single.abund) {
  #Inputs:  relabund.df = dataframe containing only relative abundance data, no metadata or other info. Samples in rows and features (OTUs, metabolites, etc) in columns.
  
  #min.num = the minimum number of samples a feature needs to be present in to not be culled.
  #Wes often does 3
  
  #min.abund = the minimum relative abundance a feature needs to have in (the min.num) samples to not be culled. i.e ra of atleast .1% in atleast three samples
  #Wes often does 0.001
  
  #min.single.abund = the minimum relative abundance a feature needs to have in a single sample to not be culled.
  #Wes often does 0.01
  
  #The idea is that if it only shows up once but at a high percent (i.e.1%) it stays. 
  
  sub=c() #create a empty vector
  
  cull=function(x) { #make a function that says for any input, generate a logical vector of TRUEs and FALSEs that will be used for subsetting, selecting features that...
    sub=ifelse(length(x[x>=min.abund])>=min.num #have a relabund>"min.abund" in "min.num" samples 
               | length(x[x>=min.single.abund])>0,TRUE,FALSE) #or have a relabund>"min.single.abund" in at least one sample
    return(sub)
  }
  
  cull.vec=apply(relabund.df,2,FUN=cull) #apply cull function to relabund.df, save output as a vector.
  cull.vec <<- cull.vec[]
  relabund.df.cull=relabund.df[,cull.vec] #Use cull.vec to subset the columns of relabund.df for OTUs that passed the cull threshold.
  
  relabund.df.cull<<-relabund.df.cull
} 