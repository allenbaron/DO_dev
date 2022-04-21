# Complete Audit of DO URLs
# J. Allen Baron
# 2022-04-22

library(tidyverse)
library(DO.utils) # >= v0.3
library(googlesheets4)

# extract all URLs (w/DOID)
repo_path <- "~/Documents/Ontologies/HumanDiseaseOntology"
input_df <- DO.utils::extract_doid_url(repo_path)

# identify unique URLs & standardize for testing
url_df <- input_df %>%
    dplyr::select(-.data$doid) %>%
    unique() %>%
    dplyr::mutate(
        std_url = DO.utils::standardize_www_duplicate(.data$url),
        domain = DO.utils::parse_url(.data$std_url)$domain
    )

# identify unique domains & initiate domain repository list to capture HTTP
#   info regulating each domain, also:
#   1. check robots ONLY for domains with > 10 unique URLs
#   2. set default delay for intra-domain requests.
dom_df <- url_df %>%
    dplyr::count(domain) %>%
    dplyr::mutate(check_robots = n > 10)

dom_list <- init_domain_repo(dom_df$domain, dom_df$check_robots, delay = 2)

# execute URL audit
audit_res <- audit_url(url_df$url, dom_list)

# tidy for review
review <- dplyr::full_join(
    input_df,
    audit_res,
    by = "url"
)

# write to google sheet
googlesheets4::write_sheet(
    gs = "1K5s2_-aoVocKIFXHe2jKFmEfLgrlwm45BAlZzArxwGQ",
    sheet = paste0("full_", DO.utils::today_datestamp())
)

