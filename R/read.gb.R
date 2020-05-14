#' @title Opens files with .gb extensions
#' 
#' @description This function opens complete record(s) with .gb extension from the NCBI/GenBank Nucleotide database and returns a list containing shaped record(s). These kind of files contains detailed records of DNA samples (locus, organism, type of sequence, source of the sequence...). An example of record can be found at <https://www.ncbi.nlm.nih.gov/nuccore/HE799070>. Records with > 200'000 bp may experience long processing times, especially if they have numerous FEATURES items. Also works for reports obtained with rentrez package.
#'  
#'
#' @param x  character. The name of the file which the data are to be read from, or the character string containing the data. It can contains several records
#' @param DNA   logical. If TRUE, the DNA sequence in the ORIGIN part will be merged in one character string. If FALSE, the default layout will be kept. Default if TRUE
#' @param Type  character. Should the output contain FEATURES and REFERENCE parts ? Possible values are "full" for a full record, "nofeat" to ignore FEATURES part, "noref" to ignore REFERENCE part and "nfnr" to ignore both parts. Default is "full"
#' @param Source  character. If x is a character string, use "Char". If x is a filename use "File". Default is "File"
#'
#' @include DNA.treatment.R
#' @include Feature.sep.R
#' @include Reference.sep.R
#' @include Reorganize.report.R
#'
#' @return Returns a table containing the data
#'
#' @examples
#' \dontrun{
#' read.gb(File = "sequence.gb", DNA = TRUE, Type = "full", Source = "File")
#' }
#' 
#' require(rentrez)
#' x <- rentrez::entrez_fetch(db = "Nucleotide", id = "508082122", rettype = "gb")
#' read.gb(File = x, DNA = TRUE, Type = "full", Source = "Char")
#' 
#' @export
read.gb <- function(x, DNA = TRUE, Type = "full", Source = "File"){
  ## Source of data :
  if(Source == "File"){
    Base <- readChar(x, file.info(x)$size, nchars = 99999999)
  }
  if(Source == "Char"){
    Base <- x
  }

  Sample <- list()
  SampleS <- gregexpr("LOCUS", Base)[[1]]
  SampleE <- gregexpr("(?<!:)//", Base, perl = T)[[1]]
  for(k in 1:length(SampleS)){
    Temp <- substr(Base, SampleS[k], SampleE[k])
    y <- Reorganize.report(Temp)

    if(Type == "full" | Type == "nofeat"){
      y$REFERENCE <- Reference.sep(y)
    }
    if(Type == "full" | Type == "noref"){
      y$FEATURES <- Feature.sep(y)
    }
    if(Type == "noref" | Type == "nfnr"){
      y[["REFERENCE"]] <- NULL
    }
    if(Type == "nofeat" | Type == "nfnr"){
      y[["FEATURES"]] <- NULL
    }

    if(DNA == TRUE){
      y$ORIGIN <- DNA.treatment(y)
    }
    Sample[[length(Sample) + 1]] <- y
    names(Sample)[[length(Sample)]] <- strsplit(y$LOCUS, "\\s")[[1]][1]
    message(paste("Sample", k, "in", length(SampleS), "done", sep = " "))
  }
  return(Sample)
}
