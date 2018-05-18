#####################################################################################
#
#     MULTIPLE SAMPLE CHI-SQUARED/F TEST
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: November 4, 2017
#     Versión: 1.0
#
#     DESCRIPTION
#
#     The function gets different linear rank scores from a vector of ranks.
#     If Chi-squared method is used, a list with p-value, Chi-squared statistic and df is obtained
#     If F method is used, a list with p-value, F statistic and degrees of freedom is obtained
#
#     s: is a numerical vector of scores
#     groups: is a factor that indicates the group of each observation in s
#     test.method: is a vector of characters. Choose one type:
#         "Chisq": chi-squared stastistic sum(ni*(Ai-a)^2/V) is used with k-1 df.
#         "Fstat": F statistic is used F = Chisq / (k-1) / ( (N-1-Chisq)/(N-k)  )


multiple.ChisqF <- function(s, groups, test.method){
  
  #Error verification
  feasibles.methods = c("Chisq","Fstat")
  if(all(test.method != feasibles.methods)){
    print("Error: alignement type not recognized")
    return()
  }
  
  #Preliminaries
  ni = tapply(s,groups,length) #subgroup sample size
  N = sum(ni)
  Ai = tapply(s,groups,mean) #subgroup mean
  a = mean(s) #overall mean of N scores
  V = var(s) #overall variance of N scores
  k = length(ni)
  df = k - 1
  ChiS = sum(ni*(Ai-a)^2/V)
  
  #Chi-squared approximation method
  #ncp is omited in pchisq, if ncp = 0 is added, a less precise algorith is used
  if(test.method == "Chisq"){
    pvalue = 1 - pchisq(q = ChiS, df = df, lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue, Chisq = ChiS,df = df)
    return(results) 
  }
  #F approximation method
  #ncp is omited in pf, if ncp = 0 is added, a less precise algorith is used
  if(test.method == "Fstat"){
    Fstat = ( ChiS / (k-1) ) / ( (N-1-ChiS)/(N-k)  )
    pvalue = 1 - pf(Fstat, df1 = (k-1), df2 = (N-1), lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue, Fstat = Fstat, df1 = (N-k), df2 = (N-1))
    return(results) 
  }
}
