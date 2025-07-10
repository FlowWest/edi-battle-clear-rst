library(tidyverse)
library(EMLaide)
library(EML)


datatable_metadata <-
  dplyr::tibble(filepath = c("data/catch.csv",
                             "data/trap.csv",
                             "data/recapture.csv",
                             "data/release.csv"),
                attribute_info = c("data-raw/metadata/catch-metadata.xlsx",
                                   "data-raw/metadata/trap-metadata.xlsx",
                                   "data-raw/metadata/recapture-metadata.xlsx",
                                   "data-raw/metadata/release-metadata.xlsx"),
                datatable_description = c("Catch table",
                                          "Trap visit table",
                                          "Recaptures",
                                          "Release summary"))

other_entity_metadata <- list("file_name" = "Battle_Clear_Methods.zip",
                              "file_description" = "Additional methods for the Battle and Clear Creek EDI package containing equations",
                              "file_type" = "zip",
                              "physical" = create_physical("data-raw/metadata/Battle_Clear_Methods.zip",
                                                           data_url = "https://raw.githubusercontent.com/FlowWest/edi-battle-clear-rst/main/data-raw/metadata/Battle_Clear_Methods.zip"))
other_entity_metadata$physical$dataFormat <- list("externallyDefinedFormat" = list("formatName" = "zip"))


# 
# other_entity_metadata <- tibble(file_name = "data-raw/battle_clear_data_processing_scripts.zip",
#                                 file_description = "R scripts that produce daily passage and cumulative catch tables from the raw catch table 
#                                                      and biweekly and brood-year passage via bootstrapping",
#                                 file_type = "ZIP file containing three R scripts")


excel_path <- "data-raw/metadata/project-metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.docx"

# edi_number <- reserve_edi_id(user_id = Sys.getenv("edi_user_id"), password = Sys.getenv("edi_password"))

# reserved under JPE account 10-3-2023
edi_number = "edi.1509.4"

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
  
custom_units <- data.frame(id = c("Day", "count of fish", "NTU", "count", 
                                  "revolutionsPerMinute", "percentage"),
                           unitType = c(NA, NA, NA, NA, NA, NA),
                           parentSI = c(NA, NA, NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA, NA, NA),
                           description = c("Number of days",
                                           "Count of fish",
                                           "Turbidity measured in Nephelometric Turbidity Units",
                                           "Count of 10 gallon debris tubs",
                                           "Number of revolutions per minute",
                                           "Baileys efficiency in percentage"))

unitList <- EML::set_unitList(custom_units)

eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
            )

EML::write_eml(eml, paste0(edi_number, ".xml"))
EML::eml_validate(paste0(edi_number, ".xml"))

EMLaide::evaluate_edi_package(Sys.getenv("EDI_USER_ID"), Sys.getenv("EDI_PASSWORD"), paste0(edi_number, ".xml"))
report_df |> filter(Status == "error")
# #EMLaide::upload_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), paste0(edi_number, ".xml"))
# 
EMLaide::update_edi_package(Sys.getenv("EDI_USER_ID"),
                            Sys.getenv("EDI_PASSWORD"),
                            "edi.1509.2",
                            "edi.1509.3.xml",
                            environment = "staging")
