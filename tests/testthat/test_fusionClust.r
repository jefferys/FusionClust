context( "Testing functions in file fusionClust.R" )

svSampleFile <- "data/abraSv.tsv"
svSampleData <- readLines( svSampleFile )
svMultiTumorOnlyFile <- "data/abraSvMultiTumorOnly.tsv"
svMultiTumorOnlyData <- readLines( svMultiTumorOnlyFile )

describe( "Test dependencies infrastructure", {
  it( "includes a file of abra-derived structural variants.", {
    expect_true( file.exists( svSampleFile ))
    expect_true( length(svSampleData) > 0)
  })
  it( "includes a file of multi-sample, tumor only abra-derived structural variants.", {
    expect_true( file.exists( svMultiTumorOnlyFile ))
    expect_true( length(svMultiTumorOnlyData) > 0)
  })
})

describe( "Testing readAbraSv() ...", {
	describe( "with default parameters ..." , {
		describe( "with file = single file", {
			it ( "Reads the correct number of rows, into a data frame, without error", {
				expect_silent( fusions <- readAbraSv(svSampleFile) )
				expect_is( fusions, "data.frame" )
				expect_equal( nrow(fusions), length(svSampleData) )
			})
		})
	})
	describe( "tumorOnly= parameter effects", {
		describe( "with multiple samples ...", {
			it ( "Reads the correct number of rows, into a data frame, without error", {
				expect_silent( fusions <- readAbraSv(svMultiTumorOnlyFile, tumorOnly= TRUE) )
				expect_is( fusions, "data.frame" )
				expect_equal( nrow(fusions), length(svMultiTumorOnlyData) )
			})
		})
	})
})
