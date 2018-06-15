
expect_that(make_filename(2018), is_equivalent_to("accident_2018.csv.bz2"))
expect_that(fars_read("~/pkg/inst/extdata/accident_2015.csv.bz2"), is_a("data.frame"))
