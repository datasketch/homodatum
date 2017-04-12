
devtools::document()
devtools::install()


library(homodatum)


sampleCtypes(3)
sampleCtypes(3, as_df = TRUE)

n <- "NumP-Cat1-Dat"
sampleCtypes(n)
sampleCtypes(n, as_df = TRUE)

sampleWdy(6, lang = "cn")
sampleData("Wdy2",lang = "fr")
