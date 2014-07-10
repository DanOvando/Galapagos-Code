################################################################################
# Functions to write .dat file, .pin file, and delete misc files
#     for new LBSPR_AssessFun
# Updated Oct 24 for newest version.
################################################################################

WriteDat <- function(MK, Linf, LinfCV, PercLeft, NumAgClass, LengthMids, LengthClasses, LenFreq, LenProp, MatL50, MatL95, ADMBDir) {
  RelAge <- seq(from = 1/NumAgClass, to=1, length=NumAgClass)
  numLenBins <- length(LengthMids)
  con=file(paste(ADMBDir, "/lbspr_assessfun.dat", sep=""), open="wt")
  write(as.character("#M/K"),con)
  write(MK,con)
  write(as.character("#Linf"),con)
  write(Linf,con)
  write(as.character("#LinfCV"),con)
  write(LinfCV,con)
  write(as.character("#PercLeft"),con)
  write(PercLeft,con)
  write(as.character("#NumAgClass"),con)
  write(NumAgClass,con)
  write(as.character("#RelativeAge"),con)
  write(RelAge,con)
  write(as.character("#NumLenBins"),con)
  write(numLenBins,con)
  write(as.character("#LengthMids"),con)
  write(LengthMids,con)
  write(as.character("#LengthClasses"),con)
  write(LengthClasses,con)
  write(as.character("#LenFreq"),con)
  write(LenFreq,con)
  write(as.character("#LenProp"),con)
  write(LenProp,con)
  write(as.character("#MatL50"), con)
  write(MatL50, con)
  write(as.character("#MatL95"), con)
  write(MatL95, con)
  close(con)
}

WritePin <- function(ADMBDir, Vals) {
  con=file(paste(ADMBDir, "/LBSPR_AssessFun",".pin",sep=""),open="wt")
  # write("# Alpha ",con)
  # write(Vals[4], con)
  write("# SelL50",con)
  write(Vals[1],con)
  write("# Delta",con)
  write(Vals[2],con)
  write("# logFM",con)
  write(Vals[3],con)
  close(con)
}

DeleteFiles <- function(ADMBDir) {
  files <- list.files(ADMBDir)
  delfiles <- grep("admodel", files)
  delfiles <- append(delfiles, grep("eigv", files))
  delfiles <- append(delfiles, grep("fmin", files))
  delfiles <- append(delfiles, grep(".bar", files))
  delfiles <- append(delfiles, grep(".cor", files))
  delfiles <- append(delfiles, grep(".eva", files))
  delfiles <- append(delfiles, grep(".out", files))
  delfiles <- append(delfiles, grep("variance", files))
  delfiles <- append(delfiles, grep(".par", files))
  delfiles <- append(delfiles, grep(".rep", files))
  delfiles <- append(delfiles, grep(".std", files))
  delfiles <- append(delfiles, grep(".b01", files))
  delfiles <- append(delfiles, grep(".p01", files))
  delfiles <- append(delfiles, grep(".r01", files))
  delfiles <- append(delfiles, grep("lbspr_assessfun.log", files))
  Names <- paste(ADMBDir, files[delfiles], sep="/")
  if (length(delfiles) >= 1) file.remove(Names)
} 
