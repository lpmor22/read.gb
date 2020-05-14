#' @title Extracts and returns a specific item from .gb records
#' 
#' @description This function regroup every items found in a list if they correspond to the character string specified. Works for general parts (ACCESSION, SOURCE...) and parts of FEATURES (source, gene, CDS...)
#'  
#'
#' @param x The name of the list containing the records.
#' @param Item character. The part to extract. It have to match case.
#'
#' @return Returns a list containing the data
#'
#' @examples
#' extract.gb(Example, "gene")
#'
#'
#' @export
extract.gb <- function(x, Item){
  Common <- c("LOCUS", "DEFINITION", "ACCESSION", "VERSION", "DBLINK", "KEYWORDS", "SOURCE", "ORGANISM", "REFERENCE", "COMMENT", "FEATURES", "ORIGIN")
  DNA <- c("assembly_gap", "C_region", "CDS", "centromere", "D-loop", "D_segment", "exon", "gap", "gene", "iDNA", "intron", "J_segment", "mat_peptide", "misc_binding", "misc_difference", "misc_feature", "misc_recomb", "misc_RNA", "misc_structure", "mobile_element", "modified_base", "mRNA", "ncRNA", "N_region", "old_sequence", "operon", "oriT", "polyA_site", "precursor_RNA", "prim_transcript", "primer_bind", "propeptide", "protein_bind", "regulatory", "repeat_region", "rep_origin", "rRNA", "S_region", "sig_peptide", "source", "stem_loop", "STS", "telomere", "tmRNA", "transit_peptide", "tRNA", "unsure", "V_region", "V_segment", "variation", "variation", "variation")
  
  TempE <- list()
  
  ## Common informations :
  if(length(grep(Item, Common)) == 1){
    for(i in 1:length(x)){
      if(!is.null(x[[i]][Item][[1]]) == T){
        TempE[[length(TempE) + 1]] <- x[[i]][Item]
        names(TempE)[[length(TempE)]] <- strsplit(as.character(x[[i]]["LOCUS"]), "\\s")[[1]][1]
      }
    }
  }
  
  ## DNA part :
  if(length(grep(Item, DNA)) == 1){
    for(i in 1:length(x)){
      if(!is.null(x[[i]][["FEATURES"]][Item][[1]]) == T && length(grep(Item, names(x[[i]][["FEATURES"]])))  == 1){
        TempE[[length(TempE) + 1]] <- x[[i]][["FEATURES"]][Item]
        names(TempE)[[length(TempE)]] <- strsplit(as.character(x[[i]]["LOCUS"]), "\\s")[[1]][1]
      }
      
      if(!is.null(x[[i]][["FEATURES"]][Item][[1]]) == T && length(grep(Item, names(x[[i]][["FEATURES"]]))) > 1){
        Count <- grep(Item, names(x[[i]][["FEATURES"]]))
        TempDNA <- list()
        
        for(j in 1:length(Count)){
          TempDNA[[length(TempDNA) + 1]] <- x[[i]][["FEATURES"]][Count[j]]
        }
        TempE[[length(TempE) + 1]] <- TempDNA
        names(TempE)[[length(TempE)]] <- strsplit(as.character(x[[i]]["LOCUS"]), "\\s")[[1]][1]
      }
    }
  }
  
  
  return(TempE)
}