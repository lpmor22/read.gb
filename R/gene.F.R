gene.F <-
function(Feat, SQuali, SQualiN){
  if (length(Feat) == 1) {
    return(NULL)
  }
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/operon=", "/product=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "operon", "product", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  gene <- data.frame("Location" = "gene", "Qualifier" = gsub(".*gene +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        gene <- rbind(gene, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $|\\d$", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $|\\d$", Feat[t])) == 0 && t <= length(Feat)){
            gene[dim(gene)[1],2] <- paste(gene[dim(gene)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $|\\d$", Feat[t])) == 1){
            gene[dim(gene)[1],2] <- paste(gene[dim(gene)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        gene <- rbind(gene, c(SQuali[k], SQualiN[k]))
      }
    }

    ## If a PCR primers part is included in the gene part :
    if(length(grep("PCR_primers=", Feat[i])) == 1){
      s <- gsub(".*=\\\\\\\"([^.]+)\"*", "\\1", Feat[i])
      k <-i+1
      while(length(grep("\\\\\"$", Feat[k])) != 1 && is.na(Feat[k]) == F){
        s <- paste(s, Feat[k], collapse = "")
        k <- k+1
      }
      if(is.na(Feat[k]) == F){
        s <- paste(s, Feat[k], collapse = "")
      }
      s <- gsub("PCR_primers=", "", s)
      gene <- rbind(gene, c("PCR_primers",  s))
      gene[length(gene[,1]),2] <- gsub("\\\"", "", gene[length(gene[,1]),2], perl = TRUE)

    }
  }
  gene <- apply(gene, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  gene <- apply(gene, 2, function(x){gsub("\"", "", x, fixed = T)})
  gene <- apply(gene, 2, function(x){gsub("\\", "", x, fixed = T)})
  gene <- apply(gene, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]|, $", "", x, perl = TRUE)})
  return(gene)
}
