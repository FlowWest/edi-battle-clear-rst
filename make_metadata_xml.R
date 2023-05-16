library(tidyverse)
library(EMLaide)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/battle_recapture.csv",
                             "data/battle_release.csv",
                             "data/clear_recapture.csv",
                             "data/clear_release.csv",
                             "data/catch_final.csv",
                             "data/trap_final.csv"),
                attribute_info = c("data-raw/metadata/battle-recapture-metadata.xlsx",
                                   "data-raw/metadata/battle-release-metadata.xlsx",
                                   "data-raw/metadata/clear-recapture-metadata.xlsx",
                                   "data-raw/metadata/clear-release-metadata.xlsx",
                                   "data-raw/metadata/catch-metadata.xlsx",
                                   "data-raw/metadata/trap-metadata.xlsx"),
                datatable_description = c("Recapture table for Battle",
                                          "Release table for Battle",
                                          "Recapture table for Clear Creek",
                                          "Release table for Clear Creek",
                                          "Catch table",
                                          "Trap visit table"))

excel_path <- "data-raw/metadata/project-metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.docx"

# edi_number <- reserve_edi_id(user_id = "user name", password = "password")

# TODO update to correct version
edi_number = "edi.1030.2"

dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

custom_units <- data.frame(id = c("Day", "count of fish", "NTU"),
                           unitType = c(NA, NA, NA),
                           parentSI = c(NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA),
                           description = c("Number of days",
                                           "Count of fish",
                                           "Turbidity measured in Nephelometric Turbidity Units"))

unitList <- EML::set_unitList(custom_units)

eml <- list(packageId = "edi.1030.2",
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList)))

EML::write_eml(eml, "edi.1030.2.xml")
EML::eml_validate("edi.1030.2.xml")
