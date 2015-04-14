## clinicalTable.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## REQUIRE
clinicalTable <- function(x){
  require(synapseClient)
  #require()
  
  ## LOAD METABRIC DATA
  mbricEnt <- synGet('syn1710260', load=TRUE)
  metaClin <- mbricEnt@objects$metabricClinicalTable
  
  ## LOAD OSLOVAL DATA
  osloClinEnt <- synGet('syn1710251', load=TRUE)
  osloSurvEnt <- synGet('syn1710257', load=TRUE)
  osloClin <- osloClinEnt@objects$oslovalClinicalTable
  
  ## CONSTRUCT DATA FRAME
  clinTable <- data.frame('METABRIC' = rep(NA, 28), 
                          'OSLOVAL' = rep(NA, 28), 
                          'pVal' = rep(NA, 28),
                          'metabricNA' = rep(NA, 28),
                          'oslovalNA' = rep(NA, 28))
  rownames(clinTable)[1] <- 'cohortSize'
  rownames(clinTable)[2] <- 'age'
  rownames(clinTable)[3] <- 'ageLessEqual50'
  rownames(clinTable)[4] <- 'age51to59'
  rownames(clinTable)[5] <- 'age60Plus'
  rownames(clinTable)[6] <- 'tumorSize'
  rownames(clinTable)[7] <- 'tumorLessEqual2'
  rownames(clinTable)[8] <- 'tumor2to5'
  rownames(clinTable)[9] <- 'tumor5Plus'
  rownames(clinTable)[10] <- 'nodeStatus'
  rownames(clinTable)[11] <- 'nodeNegative'
  rownames(clinTable)[12] <- 'nodes1to3'
  rownames(clinTable)[13] <- 'nodes4to9'
  rownames(clinTable)[14] <- 'nodes10plus'
  rownames(clinTable)[15] <- 'erStatus'
  rownames(clinTable)[16] <- 'erExpressPositive'
  rownames(clinTable)[17] <- 'erExpressNegative'
  rownames(clinTable)[18] <- 'prStatus'
  rownames(clinTable)[19] <- 'prExpressPositive'
  rownames(clinTable)[20] <- 'prExpressNegative'
  rownames(clinTable)[21] <- 'her2CopyStatus'
  rownames(clinTable)[22] <- 'her2CopyAmp'
  rownames(clinTable)[23] <- 'her2CopyNeutral'
  rownames(clinTable)[24] <- 'her2CopyLoss'
  rownames(clinTable)[25] <- 'tumorGrade'
  rownames(clinTable)[26] <- 'gradeOne'
  rownames(clinTable)[27] <- 'gradeTwo'
  rownames(clinTable)[28] <- 'gradeThree'
  
  ## POPULATE THE METABRIC CLINICAL
  # Cohort Size
  clinTable['cohortSize', 'METABRIC'] <- nrow(metaClin)
  clinTable['cohortSize', 'OSLOVAL'] <- nrow(osloClin)
  
  ## NA function
  countNA <- function(x){
    length(which(is.na(x)))
  }
  
  # Age at Diagnosis
  mbricAge <- metaClin$age_at_diagnosis
  mbricAgeNA <- countNA(mbricAge)
  clinTable['ageLessEqual50', 'METABRIC'] <-
    length(mbricAge[which(mbricAge <= 50)])
  clinTable['age51to59', 'METABRIC'] <-
    length(mbricAge[which(mbricAge >= 51 & mbricAge < 60)])
  clinTable['age60Plus', 'METABRIC'] <- 
    length(mbricAge[which(mbricAge >= 60)])
  
  osloAge <- osloClin$age_at_diagnosis
  osloAgeNA <- countNA(osloAge)
  clinTable['ageLessEqual50', 'OSLOVAL'] <-
    length(osloAge[which(osloAge <= 50)])
  clinTable['age51to59', 'OSLOVAL'] <-
    length(osloAge[which(osloAge >= 51 & osloAge < 60)])
  clinTable['age60Plus', 'OSLOVAL'] <- 
    length(osloAge[which(osloAge >= 60)])
  
  ageFisher <- fisher.test(clinTable[c('ageLessEqual50', 'age51to59', 'age60Plus'), 1:2])
  clinTable['age', 'pVal'] <- ageFisher$p.value
  clinTable['age', 'metabricNA'] <- mbricAgeNA
  clinTable['age', 'oslovalNA'] <- osloAgeNA
  
  # Tumor Size
  mbricSize <- metaClin$size
  mbricSizeNA <- countNA(mbricSize)
  clinTable['tumorLessEqual2', 'METABRIC'] <- length(mbricSize[which(mbricSize < 20)])
  clinTable['tumor2to5', 'METABRIC'] <- length(mbricSize[which(mbricSize >= 20 & mbricSize <= 50)])
  clinTable['tumor5Plus', 'METABRIC'] <- length(mbricSize[which(mbricSize > 50)])
  
  osloSize <- osloClin$size
  osloSizeNA <- countNA(osloSize)
  clinTable['tumorLessEqual2', 'OSLOVAL'] <- length(osloSize[which(osloSize < 2)])
  clinTable['tumor2to5', 'OSLOVAL'] <- length(osloSize[which(osloSize >= 2 & osloSize <= 5)])
  clinTable['tumor5Plus', 'OSLOVAL'] <- length(osloSize[which(osloSize > 5)])
  
  sizeFisher <- fisher.test(clinTable[c('tumorLessEqual2', 'tumor2to5', 'tumor5Plus'), 1:2])
  clinTable['tumorSize', 'pVal'] <- sizeFisher$p.value
  clinTable['tumorSize', 'metabricNA'] <- mbricSizeNA
  clinTable['tumorSize', 'metabricNA'] <- osloSizeNA
  
  # Node Status
  metaNodes <- metaClin$lymph_nodes_positive
  metaNodesNA <- countNA(metaNodes)
  clinTable['nodeNegative', 'METABRIC'] <- length(metaNodes[which(metaNodes == 0)])
  clinTable['nodes1to3', 'METABRIC'] <- length(metaNodes[which(metaNodes > 0 & metaNodes < 4)])
  clinTable['nodes4to9', 'METABRIC'] <- length(metaNodes[which(metaNodes > 3 & metaNodes < 10)])
  clinTable['nodes10plus', 'METABRIC'] <- length(metaNodes[which(metaNodes > 9)])
  
  osloNodes <- osloClin$lymph_nodes_positive
  osloNodesNA <- countNA(osloNodes)
  clinTable['nodeNegative', 'OSLOVAL'] <- 
    length(osloNodes[which(osloNodes == 0 & osloNodes != 'NA')])
  clinTable['nodes1to3', 'OSLOVAL'] <- 
    length(osloNodes[which(osloNodes > 0 & osloNodes < 4 & osloNodes != 'NA')])
  clinTable['nodes4to9', 'OSLOVAL'] <- 
    length(osloNodes[which(osloNodes > 3 & osloNodes < 10 & osloNodes != 'NA')])
  clinTable['nodes10plus', 'OSLOVAL'] <- length(osloNodes[which(osloNodes > 9 & osloNodes != 'NA')])
  
  nodeFisher <- fisher.test(clinTable[c('nodeNegative', 'nodes1to3', 'nodes4to9', 'nodes10plus'), 1:2])
  clinTable['nodeStatus', 'pVal'] <- nodeFisher$p.value
  clinTable['nodeStatus', 'metabricNA'] <- metaNodesNA
  clinTable['nodeStatus', 'oslovalNA'] <- osloNodesNA
  
  # ER Status
  metaER <- metaClin$ER.Expr
  metaERNA <- countNA(metaER)
  clinTable['erExpressNegative', 'METABRIC'] <- length(metaER[which(metaER == '-')])
  clinTable['erExpressPositive', 'METABRIC'] <- length(metaER[which(metaER == '+')])
  
  osloER <- osloClin$ER.Expr
  clinTable['erExpressNegative', 'OSLOVAL'] <- length(osloER[which(osloER == '-')])
  clinTable['erExpressPositive', 'OSLOVAL'] <- length(osloER[which(osloER == '+')])
  
  erFisher <- fisher.test(clinTable[c('erExpressNegative', 'erExpressPositive'), 1:2])
  clinTable['erStatus', 'pVal'] <- erFisher$p.value
  
  # PR Status
  metaPR <- metaClin$PR.Expr
  clinTable['prExpressNegative', 'METABRIC'] <- length(metaPR[which(metaPR == '-')])
  clinTable['prExpressPositive', 'METABRIC'] <- length(metaPR[which(metaPR == '+')])
  
  osloPR <- osloClin$PR.Expr
  clinTable['prExpressNegative', 'OSLOVAL'] <- length(osloPR[which(osloPR == '-')])
  clinTable['prExpressPositive', 'OSLOVAL'] <- length(osloPR[which(osloPR == '+')])
  
  prFisher <- fisher.test(clinTable[c('prExpressNegative', 'prExpressPositive'), 1:2])
  clinTable['prStatus', 'pVal'] <- prFisher$p.value
  
  # HER2 Status
  metaHer2 <- metaClin$HER2_SNP6_state
  clinTable['her2CopyAmp', 'METABRIC'] <- length(metaHer2[which(metaHer2 == 'GAIN')])
  clinTable['her2CopyNeutral', 'METABRIC'] <- length(metaHer2[which(metaHer2 == 'NEUT')])
  clinTable['her2CopyLoss', 'METABRIC'] <- length(metaHer2[which(metaHer2 == 'LOSS')])
  
  osloHer2 <- osloClin$HER2_SNP6_state
  clinTable['her2CopyAmp', 'OSLOVAL'] <- length(osloHer2[which(osloHer2 == 'GAIN')])
  clinTable['her2CopyNeutral', 'OSLOVAL'] <- length(osloHer2[which(osloHer2 == 'NEUT')])
  clinTable['her2CopyLoss', 'OSLOVAL'] <- length(osloHer2[which(osloHer2 == 'LOSS')])
  
  her2Fisher <- fisher.test(clinTable[c('her2CopyAmp', 'her2CopyNeutral', 'her2CopyLoss'), 1:2])
  clinTable['her2CopyStatus', 'pVal'] <- her2Fisher$p.value
  
  # Grade
  metaGrade <- metaClin$grade
  metaGradeNA <- countNA(metaGrade)
  clinTable['gradeOne', 'METABRIC'] <- length(metaGrade[which(metaGrade == 1)])
  clinTable['gradeTwo', 'METABRIC'] <- length(metaGrade[which(metaGrade == 2)])
  clinTable['gradeThree', 'METABRIC'] <- length(metaGrade[which(metaGrade == 3)])
  
  osloGrade <- osloClin$grade
  osloGradeNA <- countNA(osloGrade)
  clinTable['gradeOne', 'OSLOVAL'] <- length(osloGrade[which(osloGrade == 1 & osloGrade != 'NA')])
  clinTable['gradeTwo', 'OSLOVAL'] <- length(osloGrade[which(osloGrade == 2 & osloNodes != 'NA')])
  clinTable['gradeThree', 'OSLOVAL'] <- length(osloGrade[which(osloGrade == 3 & osloNodes != 'NA')])
  
  gradeFisher <- fisher.test(clinTable[c('gradeOne', 'gradeTwo', 'gradeThree'), 1:2])
  clinTable['tumorGrade', 'pVal'] <- gradeFisher$p.value
  clinTable['tumorGrade', 'metabricNA'] <- metaGradeNA
  clinTable['tumorGrade', 'oslovalNA'] <- osloGradeNA
  
  ## MAKE REAL TABLE (DIVIDE BY SAMPLE SIZE)
  innerTab <- clinTable[ , 1:2]
  innerTab$METABRIC <- 100 * round(innerTab$METABRIC/1981, 3)
  innerTab$OSLOVAL <- 100 * round(innerTab$OSLOVAL/184, 3)
  
  ## FINAL TABLE
  clinTable[2:28, 1:2] <- innerTab[2:28, 1:2]
  clinTable <- data.frame('Categories' = rownames(clinTable), clinTable)
}
