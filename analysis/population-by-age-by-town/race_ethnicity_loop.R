library(data.table)

remove(list=ls())

total.time <- proc.time()

races <- fread(file.path(getOption("common_path"), "race_and_ethnicity_tables.csv"))

# necessary since the directory structure is incomplete when pulled from repository
if (!dir.exists(file.path(getwd(), "data"))) {
    dir.create(file.path(getwd(), "data"))
}

for(r in 1L:nrow(races)) {
    race <- races[r, race]
    table <- paste("B01001", races[r, table], sep="")
    print(paste("Executing for table ", table, ":", race, sep=""))

    source(file.path("scripts", "population-by-age-acs-api.R"))

    write.table(
            dataset,
            file.path("data", "population-by-age-by-town-2021.csv"),
            sep = ",",
            row.names=F,
            append = (r != 1),
            col.names = ifelse(r == 1, T, F),
            na = "-9999"
        )
}

proc.time()-total.time
