## figureFiveA.R

## Adapted from Erhan Bilal's code
## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

figureFiveA <- function(... = NULL){
  ## REQUIRE
  require(synapseClient)
  require(survcomp)
  require(reshape)
  require(ggplot2)
  
  ## LOAD NECESSARY DATA OBJECTS
  cat('Loading necessary data objects from Synapse\n')
  clinGroupCciEnt <- loadEntity('syn1738278')
  clinGroupCciList <- clinGroupCciEnt$objects$object
  
  cat('Performing one way ANOVA on clinical variables -> model performance\n')
  ## AGE ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$preMeno), as.vector(clinGroupCciList$postMeno))
  x <- as.factor(c(rep(0, 83), rep(1, 83)))
  a <- anova(lm(y ~ x))
  age_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance AGE: ", age_pvar)
  message("p-value: ", a[1,5])
  
  ## LYMPH NODE STATUS ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$lnNeg),
         as.vector(clinGroupCciList$ln1to3),
         as.vector(clinGroupCciList$ln4to9),
         as.vector(clinGroupCciList$ln10plus))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83),
                   rep(2, 83),
                   rep(3, 83)))
  
  a <- anova(lm(y ~ x))
  lymphNode_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance LYMPH NODE STATUS: ", lymphNode_pvar)
  message("p-value: ", a[1,5])

  ## HISTOLOGIC GRADE ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$grade1),
         as.vector(clinGroupCciList$grade2),
         as.vector(clinGroupCciList$grade3))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83),
                   rep(2, 83)))
  
  a <- anova(lm(y ~ x))
  grade_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance HISTOLOGIC GRADE: ", grade_pvar)
  message("p-value: ", a[1,5])
  
  ## TUMOR SIZE ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$size0to2),
         as.vector(clinGroupCciList$size2plus))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83)))
  
  a <- anova(lm(y ~ x))
  size_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance TUMOR SIZE: ", size_pvar)
  message("p-value: ", a[1,5])
  
  ## FOLLOWUP TIME ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$time0to5),
         as.vector(clinGroupCciList$time5to10),
         as.vector(clinGroupCciList$time10plus))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83),
                   rep(2, 83)))
  
  a <- anova(lm(y ~ x))
  time_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance FOLLOWUP TIME: ", time_pvar)
  message("p-value: ", a[1,5])
  
  ## HER2 STATUS ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$her2Pos),
         as.vector(clinGroupCciList$her2Neg))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83)))
  
  a <- anova(lm(y ~ x))
  her2_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance HER2 STATUS: ", her2_pvar)
  message("p-value: ", a[1,5])
  
  ## ER STATUS ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$erPos),
         as.vector(clinGroupCciList$erNeg))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83)))
  
  a <- anova(lm(y ~ x))
  er_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance ER STATUS: ", er_pvar)
  message("p-value: ", a[1,5])
  
  ## PR STATUS ONE WAY ANOVA
  y <- c(as.vector(clinGroupCciList$prPos),
         as.vector(clinGroupCciList$prNeg))
  
  x <- as.factor(c(rep(0, 83),
                   rep(1, 83)))
  
  a <- anova(lm(y ~ x))
  pr_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance PR STATUS: ", pr_pvar)
  message("p-value: ", a[1,5])
  
  ## PLOT THE PERCENT VARIANCE EXPLAINED
  cat('Plotting the Variance Explained barplot\n')
  anovaDF <- data.frame('Grade' = grade_pvar, 'LN' = lymphNode_pvar, 'FollowupTime' = time_pvar, 'Age' = age_pvar, 'Size' = size_pvar,
                        'PR' = pr_pvar, 'ER' = er_pvar, 'HER2' = her2_pvar)
  mAnovaDF <- melt(anovaDF)
  colnames(mAnovaDF) <- c('Clinical', 'percentVariance')
  
  anovaBarPlot <- ggplot(mAnovaDF, aes(Clinical, percentVariance)) + geom_bar(aes(fill = Clinical), stat = 'identity') +
    ggtitle('ANOVA Analysis of Clinical Characteristics\n') +
    xlab('\nClinical Characteristic') + ylab('Percent Variance Explained\n')
  
  show(anovaBarPlot)
  
  returnList <- list('anovaDF' = anovaDF,
             'anovaBarPlot' = anovaBarPlot)
  
  return(returnList)
  
}