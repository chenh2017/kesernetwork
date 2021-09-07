icdmap <- readRDS("~/Hierarchy-ICD/test_dt/test_rintrojs/data/icdmap.rds")
phecode <- icdmap[, 4, drop = FALSE]
phecode$row <- 1:nrow(phecode)
phecode <- phecode[!duplicated(phecode$Phecode),]
phecode$Phecode <- paste0("PheCode:", phecode$Phecode)
