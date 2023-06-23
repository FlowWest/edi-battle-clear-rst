library(tidyverse)
library(EMLaide)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/catch.csv",
                             "data/trap.csv",
                             "data/recapture.csv",
                             "data/release.csv",
                             "data/passage_summary.csv"),
                attribute_info = c("data-raw/metadata/catch-metadata.xlsx",
                                   "data-raw/metadata/trap-metadata.xlsx",
                                   "data-raw/metadata/recapture-metadata.xlsx",
                                   "data-raw/metadata/release-metadata.xlsx",
                                   "data-raw/metadata/passage-summary-metadata.xlsx"),
                datatable_description = c("Catch table",
                                          "Trap visit table",
                                          "Recaptures",
                                          "Release summary",
                                          "Daily passage summary with fork length"),
                datatable_url = paste0("https://raw.githubusercontent.com/FlowWest/edi-battle-clear-rst/main/data/",
                                       c("catch.csv",
                                         "trap.csv",
                                         "recapture.csv",
                                         "release.csv",
                                         "passage_summary.csv")))

other_entity_metadata <- tibble(file_name = "data-raw/battle_clear_data_processing_scripts.zip",
                                file_description = "R scripts that produce daily passage and cumulative catch tables from the raw catch table 
                                                     and biweekly and brood-year passage via bootstrapping",
                                file_type = "ZIP file containing three R scripts")


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
  add_datatable(datatable_metadata) |> 
  add_other_entity(other_entity_metadata)
  
custom_units <- data.frame(id = c("Day", "count of fish", "NTU", "count", "revolutionsPerMinute"),
                           unitType = c(NA, NA, NA, NA, NA),
                           parentSI = c(NA, NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA, NA),
                           description = c("Number of days",
                                           "Count of fish",
                                           "Turbidity measured in Nephelometric Turbidity Units",
                                           "Count of 10 gallon debris tubs",
                                           "Number of revolutions per minute"))

unitList <- EML::set_unitList(custom_units)

eml <- list(packageId = "edi.1030.2",
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList)))

EML::write_eml(eml, "edi.1030.2.xml")
EML::eml_validate("edi.1030.2.xml")
