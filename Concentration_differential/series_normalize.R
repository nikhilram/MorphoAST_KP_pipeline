files <- list.files(pattern = "*.csv") ## setwd to directory with the files
tbl <- lapply(files, data.table::fread, sep=",", fill=TRUE)
names(tbl) <- tools::file_path_sans_ext(files)  
  for (n in names(tbl)) {
    tbl[[n]]$name <- n
   }
x <- data.table::rbindlist(tbl, fill=TRUE)
x$strain <- sapply(strsplit(x$name, "_"), function(x) paste(x[2],x[3], sep = "_"))
  
x[x$name %like% "_50_", "conc"] <- "conc_50c"
x[x$name %like% "_5_", "conc"] <- "conc_5c"
x[x$name %like% "_05_", "conc"] <- "conc_05c"
x[x$name %like% "_005_", "conc"] <- "conc_005c"
x[x$name %like% "_1_", "conc"] <- "conc_1c"
x[x$name %like% "_2_", "conc"] <- "conc_2c"
x[x$name %like% "_4_", "conc"] <- "conc_4c"
x[x$name %like% "_8_", "conc"] <- "conc_8c"
x[x$name %like% "_16_", "conc"] <- "conc_16c"
x[x$name %like% "_025_", "conc"] <- "conc_025c"
x[x$name %like% "_125_", "conc"] <- "conc_125c"
x[is.na(x$conc), "conc"] <- "Control"
  
### Determine the population mean for the untreated control for each strain at each time point  
control.means <- list()
for (f in unique(x$strain)) {
 control.means[[f]] <- list()
  for (n in 1:16) {
    a <- as.data.frame(t(colMeans(x[x$strain==f & x$conc == "Control" & x$Time_Point==n,][,c(14:32,34)], na.rm = T)))
    control.means[[f]][[n]] <- a      
    }
  }

### Normalize the parameters for each strain against the control means for each time point/Strain 
normalized.raw <- list()
for (f in unique(x$strain)) {
 normalized.raw[[f]] <- list()
 for (i in unique(x$conc[x$conc != "Control"])) {
   normalized.raw[[f]][[i]] <- list()
   for (n in 1:16) {
     b <- as.data.frame(x[x$strain==f & x$conc==i & x$Time_Point==n,][,c(1,14:32,34)])
     if (dim(b)[1]==0) {
       next
      }
     celln <- b$Cell_number
     b <- b[,-1]
     c <- control.means[[f]][[n]]
     c <- as.data.frame(lapply(c, rep, nrow(b)))
     a <- b-c
     a$strain <- f
     a$Time_Point <- n
     a$conc <- i
     a$cell_number <- celln
     normalized.raw[[f]][[i]][[n]] <- a
    }      
  }
}
normalized.compressed.a <- list()
normalized.compressed.b <- list()
for (n in names(normalized.raw)) {
  normalized.compressed.a[[n]] <- list()
  normalized.compressed.b[[n]] <- list()
  for (i in names(normalized.raw[[n]])) {
    normalized.compressed.a[[n]][[i]] <- rbindlist(normalized.raw[[n]][[i]])
   }
  normalized.compressed.b[[n]] <- rbindlist(normalized.compressed.a[[n]])
 }
  
normalized.compressed <- rbindlist(normalized.compressed.b)
normalized.compressed <- normalized.compressed[with(normalized.compressed, order(strain, conc,cell_number, Time_Point))]
  
resistance <- read.csv("../../../../resistance_profile_physiological.csv", header = T) ## CSV file with the ground truth for the different strains
normalized.compressed$resistance <- resistance$resistance[match(normalized.compressed$strain, resistance$strain)]
normalized.compressed$MIC <- resistance$eMIC[match(normalized.compressed$strain, resistance$strain)]
write.csv(normalized.compressed, file = "normalized_KP_700603.csv", quote = FALSE, row.names = FALSE) ## change filename
  

