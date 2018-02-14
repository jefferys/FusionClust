
#' Read an abra.sv file
#'
#' Read in a sample's abra.sv.txt file, or a merged (concatenated)
#' abra.sv.txt file with an extra (initial) column giving the sample name.
#'
#' @param file The abra.sv fusion data file to load. Can be a single abra sv
#' file, or a merged file with results for multuple samples.
#'
#' @param stranded Boolean indicating if the abra file has strand information.
#' Currently, setting this TRUE will prevent processing as stradedness is not
#' implemented. Default is FALSE.
#'
#' @param orderedEnds Boolean indicating if the output should order the two
#' fusions ends canonically (ref, then pos), so the earlier one is first.
#' Default is TRUE.
#'
#' @param tumorOnly Boolean indicating if the abra file has one column, e.g.
#' tumor only or two columns of data, e.g. tumor and normal. Default is FALSE.
#'
#' @return Returns a data frame describing the fusions, with columns:
#' \itemize{
#'   \item{ \bold{sample} [optional] The sample this fusion came from.}
#'   \item{ \bold{group} The group name (within sample) of equivalent fusions.}
#'   \item{ \bold{chr1} End1's chromosome, parsed from <chr>:<pos>.}
#'   \item{ \bold{pos1} End1's position, parsed from <chr>:<pos>.}
#'   \item{ \bold{chr1} End2's chromosome, parsed from <chr>:<pos>.}
#'   \item{ \bold{pos1} End2's position, parsed from <chr>:<pos>.}
#'   \item{ \bold{orientation} The relative directionality of end1 and end2, as
#' blank " " if in the same direction (FF or RR from input), or as "I" if
#' relatively inverted (FR or RF fromn input).}
#'   \item{ \bold{normal_count} The number of split reads containing the fusion
#' point in the normal sample.}
#'   \item{ \bold{tumor_count} The number of split reads containing the fusion
#' point in the tumor sample.}
#'  }
#'
#' @examples
#' svFile <- "tests/testthat/data/perSampleAbraSv.tsv"
#' svFile <- system.file( svFile, package= "fusionClust" )
#' fusions <- loadAbraSv( svFile )
#' @export
readAbraSv <- function (file, stranded= FALSE, orderedEnds= TRUE, tumorOnly= FALSE) {

  # Can't handle stranded data
  if (stranded) {
    stop(
      "Can't handle abra.sv data with absolute strand information.",
      call.= FALSE
    )
  }

  # Load data
  # Have to specify column classes as all "F" in orientation column will show
  # up as FALSE.
  df <- read.csv( file, header= FALSE, sep= "\t", stringsAsFactors= FALSE, colClasses = "character" )

  # Set row names equal to the line from the file
  rownames(df) <- 1:nrow(df)

  # Set input column names, may be single sample or merged cohort
  countColumnNames = c("normal_count", "tumor_count")
  if (tumorOnly) {
    countColumnNames = c("tumor_count")
  }

  names <- c( "group", "end1", "end2", "strand1", "strand2", countColumnNames )
  keepColumns <- c( "id", "group", "chr1", "pos1", "chr2", "pos2",
                    "orientation", countColumnNames )

  # Use column count to guess if have sample names.
  withSample <- FALSE
  if ((tumorOnly & ncol(df) == 7) | ( ! tumorOnly & ncol(df) == 8)) {
    withSample <- TRUE
  }

  if (withSample) {
    names       <- c("sample", names)
    keepColumns <- c("sample", keepColumns)
  }
  else if (( tumorOnly & ncol(df) != 6 ) | ( ! tumorOnly & ncol(df) != 7 )) {
    stop(
      "Doesn't look like an abra.sv file, has the wrong column count.",
      call.= FALSE
    )
  }

  colnames(df) <- names
  df$id <- 1:nrow(df)
  # Convert chr:pos entries to separate columns, for each end
  df$chr1 <- sub( ":.*", "", df$end1 )
  df$chr2 <- sub( ":.*", "", df$end2 )
  df$pos1 <- as.integer(sub( ".*:", "", df$end1 ))
  df$pos2 <- as.integer(sub( ".*:", "", df$end2 ))

  # Fix false stranded info to "relative orientation"
  df$orientation <- paste0(df$strand1, df$strand2)
  df$orientation <- sub( "FR|RF", "I", df$orientation)
  df$orientation <- sub( "FF|RR", "", df$orientation)

  # Convert data columns back to integers
  df$tumor_count <- as.integer( df$tumor_count)
  if (! tumorOnly ) {
    df$normal_count <- as.integer( df$normal_count)
  }

  # Set output column names, may be single sample or merged cohort
  df <- df[, keepColumns]
  if (orderedEnds) {
    df <- orderFusionEnds( df )
  }
}

#' Write an abra.sv file
#'
#' Write a sample's abra.sv.txt file, or a merged (concatenated)
#' abra.sv.txt file with an extra (initial) column giving the sample name.
#'
#' @param file The abra.sv fusion data file to write.
#' @param fusions The file Can be a single abra sv
#' file, or a merged file with results for multuple samples.
#'
#' @return A data frame object describing the fusions, with columns:
#' \itemize{
#'   \item{ \bold{sample} [optional] The sample this fusion came from.}
#'   \item{ \bold{group} The group name (within sample) of equivalent fusions.}
#'   \item{ \bold{chr1} End1's chromosome, parsed from <chr>:<pos>.}
#'   \item{ \bold{pos1} End1's position, parsed from <chr>:<pos>.}
#'   \item{ \bold{chr1} End2's chromosome, parsed from <chr>:<pos>.}
#'   \item{ \bold{pos1} End2's position, parsed from <chr>:<pos>.}
#'   \item{ \bold{orientation} The relative directionality of end1 and end2, as
#' blank " " if in the same direction (FF or RR from input), or as "I" if
#' relatively inverted (FR or RF fromn input).}
#'   \item{ \bold{normal_count} The number of split reads containing the fusion
#' point in the normal sample.}
#'   \item{ \bold{tumor_count} The number of split reads containing the fusion
#' point in the tumor sample.}
#'  }
#'
#' @examples
#' svFile <- "tests/testthat/data/perSampleAbraSv.tsv"
#' svFile <- system.file( svFile, package= "fusionClust" )
#' fusions <- loadAbraSv( svFile )
#' @export
writeAbraSv <- function (fusions, file, stranded= FALSE) {
  # Can't handle stranded data
  if (stranded) {
    stop(
      "Can't handle abra.sv data with absolute strand information.",
      call.= FALSE
    )
  }
  if (is.null(fusions$sample)) {

  }
  outDF <- data.frame(

    stringsAsFactors = FALSE
  )
}

#' Order fusion ends in the cohort.
#'
#' Cannonically ordered fusions are ordered with lower end first. This switches
#' them if the high end was first. Low means alphabetic ordering by chr, and
#' then numeric ordering by position. No fusion can have the same left and right
#' side as positions are the last included base on each end.
#'
#' @param fusions A fusion cohort as a data frame with at least the columns:
#' chr1, pos1, chr2, and pos2.
#'
#' @return The input fusion cohort, but with all fusions listed low end first.
#'
#' @export
orderFusionEnds <- function( fusions ) {
  # Order ignores canonical chromosome order; only using alphabetic

  # Get a logical vector indicating which rows need fusion ends swapping.
  swapChr <- fusions$chr1 > fusions$chr2
  swapPos <-  (fusions$chr1 == fusions$chr2) & (fusions$pos1 > fusions$pos2)
  swap <- swapChr | swapPos

  # Do swap based on logical swap vector
  chrOld <- fusions$chr1
  fusions$chr1[swap] <- fusions$chr2[swap]
  fusions$chr2[swap] <- chrOld[swap]
  rm(chrOld)
  posOld <- fusions$pos1
  fusions$pos1[swap] <- fusions$pos2[swap]
  fusions$pos2[swap] <- posOld[swap]
  rm(posOld)
  fusions
}

#' Sort fusions
#'
#' @param fusions The fusions to sort
#'
#' @return Data frame of fusion ends, sorted from low to high, with fusion
#' id end columns added to allow mapping back to its source.
#'
#' @export
sortFusions <- function ( fusions ) {

  if ( ! all( c("chr1", "chr2", "pos1", "pos2") %in% names(fusions) )) {
    stop( "This doesn't appear to be a fusion frame", call.= FALSE )
  }

  # Build data frame of merged left and right fusion ends.
  fusions <- data.frame( fusionId= rownames(fusions),
                     chr= c(fusions$chr1, fusions$chr2),
                     pos= c(fusions$pos1, fusions$pos2),
                     fusionEnd= c(rep(1,nrow(fusions)), rep(2, nrow(fusions))),
                     stringsAsFactors= FALSE )

  # Order by chr and pos, low to high (alphabetical chr only)
  end <- fusions[with(fusions, order(chr, pos, fusionEnd)),]

}

#' Fusion endpoint spacings
#'
#' Merges the two enpoints into a single list of positions and spacings, then
#' sorts, splits by chromosome, and calculates the \code{diff()} of successive
#' endpints.
#'
#' @param fusions Data frame of fusions to cluster
#'
#' @return A list by chromosome, each a vector of fusion endpoint spacings on
#' that chromosome.
#'
#' @export
fusionGaps <- function ( fusions ) {

  end <- sortFusions(fusions)
  ## Get gaps between successive ends, per chr

  # Get chr names
  chrNames <- unique( end$chr )

  # Pre-create list with placeholders for gap vectors
  gaps <- as.list( rep( NA_integer_, length(chrNames)))
  names(gaps) <- chrNames

  # Get gaps
  for (chr in chrNames) {
    posSet <- end$pos[end$chr == chr]
    if (length(posSet > 1))
    gaps[[chr]] <- diff(posSet)
  }

  gaps
}

#' Cluster fusions
#'
#' @param fusions The fusions to cluster
#'
#' @param window The window size to use for clustering
#'
#' @return Data frame with fusions annotated by the cluster each end belongs to
#'  and by paired fusion.
#'
#' @export
clusterFusions <- function ( fusions, window= 10 ) {

  ends <- sortFusions(fusions)
  clusteredEnds <- clusterFusionEnds( ends, window )
  fusionCount <- nrow(fusions)

  clusteredEnds1 <- clusteredEnds[clusteredEnds$fusionEnd == 1,]
  fusions[clusteredEnds1$fusionId, "end1Cluster"] <- clusteredEnds1$clusterId
  fusions[clusteredEnds1$fusionId, "end1ClusterLow"] <- clusteredEnds1$clusterLow
  fusions[clusteredEnds1$fusionId, "end1ClusterHigh"] <- clusteredEnds1$clusterHigh

  fusions$end2Cluster <- rep(NA_integer_, fusionCount)
  clusteredEnds2 <- clusteredEnds[clusteredEnds$fusionEnd == 2,]
  fusions[clusteredEnds2$fusionId, "end2Cluster"] <- clusteredEnds2$clusterId
  fusions[clusteredEnds2$fusionId, "end2ClusterLow"] <- clusteredEnds2$clusterLow
  fusions[clusteredEnds2$fusionId, "end2ClusterHigh"] <- clusteredEnds2$clusterHigh

  clustClustNames <- unique(paste0(fusions$end1Cluster, "~", fusions$end2Cluster))
  map <- 1:length(clustClustNames)
  names(map) <- sort(clustClustNames)
  fusions$fusionClusterId <- map[paste0(fusions$end1Cluster, "~", fusions$end2Cluster)]

  fusions
}


#' Cluster and annotate a fusion end list
#'
#' @param ends A dataframe of fusion ends, sorted.
#' @param window The size of the clustering window, in base pairs. Two fusion
#'  ends where the difference in the last sequenced base position is less than
#'  this will be merged, e.g. if the positions are 10 and 19, a window of 10
#'  will merge these, a window of 9 will not.
#'
#' @return The data frame of fusion ends, annotated with the cluster they
#' belong to (adding columns clusterId, clusterLow, and clusterHigh).
#'
#' @export
clusterFusionEnds <- function ( ends, window= 10 ) {

  endsCount <- nrow(ends)
  rownames(ends) <- NULL

  # Pre-allocating extra space; creates cluster list the same size as the fusion
  # end list, will only use the rows that are not NA when done. Wastes space to
  # avoid growing a data frame.

  ends$clusterId <- rep(NA_integer_, endsCount)
  ends$clusterLow <- rep(NA_integer_, endsCount)
  ends$clusterHigh <- rep(NA_integer_, endsCount)
  ends$chained <- rep(NA, endsCount)

  # Init first fusion
  previousChr <- ends[1, "chr"]
  previousPos <- ends[1, "pos"]

  # Init first cluster
  clusterId <- 1
  lowPos    <- previousPos
  highPos   <- previousPos
  fusionIdStart <- 1

  for (fusionIdCurrent in 2:endsCount) {
    # Init new fusion
    currentChr <- ends[fusionIdCurrent, "chr"]
    currentPos <- ends[fusionIdCurrent, "pos"]

    if ( previousChr == currentChr && currentPos - previousPos < window ) {
      ### Grow cluster
      # fusionIdStart <- unchanged as same cluster
      # clusterId <- unchanged as same cluster
      # lowPos    <- unchanged as expect ends to be ordered
      highPos   <- currentPos

      ### Set old fusion
      # previousChr <- unchanged as only way to be in this block
      previousPos <- currentPos
    }
    else {
      ### Report cluster.
      # Now know previous fusion ended cluster (if any), so report that value.
      # Actually ending at position of last fusion in cluster, which was the
      # previous one, [fusionIdCurrent - 1]. Counting from 2 so never OOB
      for (fusionId in fusionIdStart:(fusionIdCurrent - 1)) {
        ends[fusionId, c("clusterId", "clusterLow", "clusterHigh")] <-
          list( clusterId, lowPos, highPos )
      }

      ### Init new cluster
      clusterId <- clusterId + 1
      lowPos  <- currentPos
      highPos <- currentPos
      fusionIdStart <- fusionIdCurrent

      ### Set old fusion
      previousChr <- currentChr
      previousPos <- currentPos
    }
  }
  ### Only now know previous cluster was last, so end and report it
  for (fusionId in fusionIdStart:(fusionIdCurrent)) {
    ends[fusionId, c("clusterId", "clusterLow", "clusterHigh")] <-
      list( clusterId, lowPos, highPos )
  }
  ends
}

#' Load a bed per-base coverage file
#'
#' @param file The file to load
#' @param merged TRUE if merged file from multiple samples with the first
#' column the "sample"
#'
#' @return A data frame with the coverage info, including a sample column
#' if a merged file.
#'
#' @export
loadBedCoverage <- function(file, merged=FALSE) {
  df <- read.csv( file, header= FALSE, sep= "\t", stringsAsFactors= FALSE )
  names <- c( "chr", "start", "end", "pos", "depth")
  if (merged) {
    names <- c("sample", names)
  }
  colnames(df) <- names
  df
}

# plotCoverage( coverageDF, feature ) {
#  feature <- coverage[coverage$chr=="gi|333031|lcl|HPV16REF.1|",]
# }

#' Merge fusion site counts by cluster
#'
#' @param clusteredFusions The clustered fusion sites
#'
#' @return A data frame with the same columns as clusteredFusions, but with
#' different entries for the same cluster in the same sample collapsed.
#'
#' @export
mergeCountsByCluster <- function ( clusteredFusions ) {
  sumColumns <- c("normal_count", "tumor_count")
  byColumns <- c( "sample", "fusionClusterId", "chr1", "chr2",
                  "end1Cluster", "end1ClusterLow", "end1ClusterHigh",
                  "end2Cluster", "end2ClusterLow", "end2ClusterHigh" )
  summed <- aggregate(
    cbind(normal_count, tumor_count) ~
      sample + fusionClusterId + chr1 + chr2 +
      end1Cluster + end1ClusterLow + end1ClusterHigh +
      end2Cluster + end2ClusterLow + end2ClusterHigh,
    clusteredFusions[c(byColumns, sumColumns)],
    FUN= sum
  )

  summed[with(summed, order(sample, fusionClusterId)),]

}

#' Load virus count file
#'
#' @param file Single sample or cohort virus count file. Cohort has extra (first)
#' column giving sample name.
#'
#' @return A matrix of samples (as rownames) x virus (as columnNames), giving
#' the counts of viruses found in the samples.
#'
#' @examples
#'
#' @export
readVirusCounts <- function(file) {

  # Load data
  df <- read.csv( file, header= FALSE, sep= "\t", stringsAsFactors= FALSE )

  # Set input column names, may be single sample or merged cohort
  names <- c( "virus", "start", "end", "count" )
  if ( ncol(df) == 5 ) {
    names <- c("sample", names)
  }
  else if ( ncol(df) != 4 ) {
    stop(
      "Doesn't look like an virus counts file, has the wrong column count.",
      call.= FALSE
    )
  }
  colnames(df) <- names

  # Extract to matrix, one row per sample, one column per virus
  sampleNames <- unique(df[,"sample"])
  virusNames <- unique(df[ df["sample"] == sampleNames[1], "virus" ])

  virusCounts <- as.data.frame(
    t(sapply( sampleNames, function (sampleName) {
      df[ df["sample"] == sampleName, "count" ]
    }))
  )
  colnames(virusCounts) <- virusNames
  virusCounts
}


