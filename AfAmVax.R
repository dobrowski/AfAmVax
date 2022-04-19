
# In response to Community Foundation Request

### Libraries ------

library(here)
library(tidyverse)
library(glue)
library(MCOE)
library(ggthemes)
library(googlesheets4)



options(scipen=999)


con <- MCOE::mcoe_sql_con()


### Import data -------


entit <- tbl(con, "SCHOOLS") %>%
    collect() 

entit.mry <- entit %>%
    filter(County == "Monterey") %>%
    mutate(Zip5 = str_sub(Zip,1,5)) %>%
    select(CDS_CODE = CDSCode,
           School,
           Zip5) 


enrollment <- tbl(con, "ENROLLMENT")  %>%
    #   head() %>%
    filter( County == "Monterey",
    ) %>%
    collect()


enrollment.sum <- enrollment %>%
    filter(YEAR == max(YEAR)) %>%
    group_by(DISTRICT, SCHOOL, CDS_CODE, ETHNIC) %>%
    summarise(TOTAL = sum(ENR_TOTAL)) %>%
    mutate(ETHNIC = as.character(ETHNIC),
           Percent = TOTAL/sum(TOTAL)) %>%
    left_join_codebook("ENROLLMENT", "ETHNIC" )


afam <- enrollment.sum %>%
    filter(str_detect(definition,"Afric"))

my.sheet <- "https://docs.google.com/spreadsheets/d/1e_X2DbDU0wiWs8OROiA-GHA0bhAa1TcchLVfSvkPKjM/edit#gid=0"

vax.perc <- read_sheet(my.sheet) %>%
    mutate(Zip5 = as.character(Zip5))


### Merge and output -----


afam.mry.vax <- afam %>%
    left_join(entit.mry) %>%
    left_join(vax.perc) %>%
    mutate(perc.not.full = 100 - perc.fully.vax,
        combo = Percent*perc.not.full) %>%
    arrange(desc(combo))


write_sheet(afam.mry.vax,
            ss = my.sheet,
            sheet = "School Results")
### End -----
