Feature.sep <-
function(y){
  TempF <- y$FEATURES
  Feature <- list()
  Tag <- c(" +assembly_gap {2,}", " +C_region {2,}", " +CDS {2,}", " +centromere {2,}", " +D-loop {2,}", " +D_segment {2,}", " +exon {2,}", " +gap {2,}",
           " +gene {2,}", " +iDNA {2,}", " +intron {2,}", " +J_segment {2,}", " +mat_peptide {2,}", " +misc_binding {2,}", " +misc_difference {2,}",
           " +misc_feature {2,}", " +misc_recomb {2,}", " +misc_RNA {2,}", " +misc_structure {2,}", " +mobile_element {2,}", " +modified_base {2,}",
           " +mRNA {2,}", " +ncRNA {2,}", " +N_region {2,}", " +old_sequence {2,}", " +operon {2,}", " +oriT {2,}", " +polyA_site {2,}", " +precursor_RNA {2,}",
           " +prim_transcript {2,}", " +primer_bind {2,}", " +propeptide {2,}", " +protein_bind {2,}", " +regulatory {2,}", " +repeat_region {2,}",
           " +rep_origin {2,}", " +rRNA {2,}", " +S_region {2,}", " +sig_peptide {2,}", " +source {2,}", " +stem_loop {2,}", " +STS {2,}", " +telomere {2,}",
           " +tmRNA {2,}", " +transit_peptide {2,}", " +tRNA {2,}", " +unsure {2,}", " +V_region {2,}", " +V_segment {2,}", " +variation {2,}", " +3[[:punct:]]UTR +"," +5[[:punct:]]UTR +")
  TagN <- c("assembly_gap", "C_region", "CDS", "centromere", "D_loop", "D_segment", "exon", "gap",
            "gene", "iDNA", "intron", "J_segment", "mat_peptide", "misc_binding", "misc_difference",
            "misc_feature", "misc_recomb", "misc_RNA", "misc_structure", "mobile_element", "modified_base",
            "mRNA", "ncRNA", "N_region", "old_sequence", "operon", "oriT", "polyA_site", "precursor_RNA",
            "prim_transcript", "primer_bind", "propeptide", "protein_bind", "regulatory", "repeat_region",
            "rep_origin", "rRNA", "S_region", "sig_peptide", "source", "stem_loop", "STS", "telomere",
            "tmRNA", "transit_peptide", "tRNA", "unsure", "V_region", "V_segment", "variation", "T_UTR","F_UTR")
  TagChr <- c()
  SQuali <- c("/environmental_sample", "/focus", "/germline", "/macronuclear", "/partial", "/proviral", "/pseudo(?!g)",
              "/rearranged", "/ribosomal_slippage", "/transgenic", "/trans_splicing")
  SQualiN <- c("environmental_sample", "focus", "germline", "macronuclear", "partial", "proviral", "pseudo", "rearranged",
               "ribosomal_slippage", "transgenic", "trans_splicing")
  
  ## Separation of Feature items :
  for(i in 1:length(Tag)){
    if(grepl(Tag[i], TempF, fixed = F)[[1]] == TRUE){
      P <- as.numeric(gregexpr(Tag[i], TempF, fixed = F)[[1]])
      for(j in 1:length(P)){
        if(P[j] != -1){
          TagChr <- c(TagChr, P[j])
        }
      }
    }
  }
  TagChr <- c(TagChr, nchar(TempF))
  TagChr <- sort(TagChr)
  
  ## Loop for Feature treatment :i
  for(k in 2:length(TagChr)){
    Feat <- substr(TempF, TagChr[k-1], TagChr[k]-3)
    Feat <- unlist(strsplit(Feat, "\\\", \\n?\\\"", fixed = F))
    for(l in 1:length(Tag)){
      if(length(grep(Tag[l], Feat[1])) == 1){
        assign(paste(TagN[l]), do.call(paste(TagN[l],".F", sep = ""), args = list(Feat, SQuali, SQualiN)))
        Feature[[length(Feature)+1]] <- data.frame(get(paste(TagN[l])), stringsAsFactors = F)
        names(Feature)[[length(Feature)]] <- paste(TagN[l])
      }
    }
  }
  return(Feature)
}
