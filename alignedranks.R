#####################################################################################
#
#     ALIGNED RANKS
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: November 3, 2017
#     Versión: 1.0
#
#     INSTRUCTIONS
#
#     Function calculates the average ranks of aligned observations of k>=1 groups
#
#     x: is a numerical vector
#     groups: is a factor that indicates the group of each observation in x
#     alignment: is a vector of characters. Choose one type:
#         "unadjusted": nothing is sustracte from x
#         "overallmean": overall mean is sustracted from x
#         "overallmedian": overall median is sustracted from x
#         "samplemean": group mean from corresponding group is sustracted from x
#         "samplemedian": group median from corresponding group is sustracted from x
#     absolute: TRUE for the rank of absolute aligned values.


alignedranks <- function(x, groups, alignement, absolute){

  #Error verification
  feasibles.alignements = c("unadjusted", "overallmean", "overallmedian", "samplemean", "samplemedian")
  if(all(alignement != feasibles.alignements)){
    print("Error: alignement type not recognized")
    return()
  }
  
  #Preliminaries
  n = tapply(x,groups,length) #used in samplemean and samplemedian
  N = sum(n) #combined samples total
  a = rep(0,N) #vector a is initialized
  #Alignment
  if(alignement == "unadjusted"){
    a = x
  }
  if(alignement == "overallmean"){
    a = x-mean(x)
  }
  if(alignement == "overallmedian"){
    a = x-median(x)
  }
  if(alignement == "samplemean"){
    xmean = tapply(x,groups,mean)
    vmean = rep(xmean,n)
    a = x - vmean
  }
  if(alignement == "samplemedian"){
    xmedian = tapply(x,groups,median)
    vmedian = rep(xmedian,n)
    a = x - vmedian
  }
  
  #Ranking
  if(absolute == T){
    r = rank(abs(a))
  }
  if(absolute == F){
    r = rank(a)
  }
  return(r)
}
