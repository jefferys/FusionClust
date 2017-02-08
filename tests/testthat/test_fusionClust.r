context( "Testing functions in file fusionClust.R" )

svSampleFile <- "data/perSampleAbraSv.tsv"
svSampleData <- readLines( svSampleFile )

describe( "Test dependencies infrastructure", {
  it( "includes a file of abra-derived structural variants.", {
    expect_true( file.exists( svSampleFile ))
    expect_true( length(svSampleData) > 0)
  })
})

describe( "Testing loadAbraSv() ...", {
  describe( "with default parameters ..." , {
    describe( "with file = single file", {
      expect_silent( fusions <- loadAbraSv(svSampleFile) )
      expect_is( fusions, "data.frame" )
      expect_equal( nrow(fusions), length(svSampleData) )
    })
  })
})

describe( "Testing orderBy() ...", {
  describe( "with default parameters ...", {
    describe( "map= is character vector map.", {
      it( "Orders correctly", {
        got <- orderBy( c("A", "B", "C"), c("C", "B", "A") )
        want <- c(3,2,1)
        expect_equal(got, want)
      })
    })
  })
})
