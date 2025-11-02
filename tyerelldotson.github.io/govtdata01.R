## GovtInfo bulk download (JSON listing)
gc(reset=TRUE)
library(purrr); library(magrittr); library(jsonlite); library(data.table); library(readr)

setwd("~/Documents/Methods of Data Collection Projects/tyerelldotson.github.io")
save_dir <- "Data/govinfo_foreign_relations/pdfs/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive=TRUE, showWarnings=FALSE)

listing_json <- tail(Sys.glob("~/Downloads/govinfo-search-results*.json"), 1)
if (length(listing_json)==0) stop("No govinfo-search-results*.json in ~/Downloads")

gf <- jsonlite::read_json(listing_json, simplifyVector=TRUE)
govfiles <- as.data.frame(gf$resultSet, stringsAsFactors=FALSE)

nm <- names(govfiles)
pdf_col <- intersect(nm, c("pdfLink","pdf","pdf_url","pdfURL","link","downloadUrl","downloadURL"))[1]
pid_col <- intersect(nm, c("packageId","packageID","package_id"))[1]

govfiles$pdf_url <- if (!is.na(pdf_col)) as.character(govfiles[[pdf_col]]) else NA_character_
if (!is.na(pid_col)) {
  fallback <- sprintf("https://www.govinfo.gov/content/pkg/%s/pdf/%s.pdf",
                      govfiles[[pid_col]], govfiles[[pid_col]])
  govfiles$pdf_url <- ifelse(is.na(govfiles$pdf_url) | govfiles$pdf_url=="", fallback, govfiles$pdf_url)
}

# ---- sort newest first (do this BEFORE choosing candidates) ----
date_col <- intersect(nm, c("dateIssued","publicationDate","publishDate","date"))[1]
if (!is.na(date_col)) {
  suppressWarnings(govfiles$.__date <- as.Date(govfiles[[date_col]]))
  govfiles <- govfiles[order(govfiles$.__date, decreasing=TRUE), ]
}

# ---- oversample, then keep first 20 that actually download ----
candidates <- govfiles[!is.na(govfiles$pdf_url) & nzchar(govfiles$pdf_url), ]
candidates <- head(candidates, 80)   # look at more rows so we can find 20 good PDFs

id_col <- intersect(names(candidates), c("packageId","packageID","package_id"))[1]
id_vec <- if (!is.na(id_col)) as.character(candidates[[id_col]]) else seq_len(nrow(candidates))

download_one <- function(url, id){
  dest <- file.path(save_dir, paste0("govfiles_", id, ".pdf"))
  tryCatch({
    if (!file.exists(dest)) download.file(url, destfile=dest, mode="wb", quiet=TRUE)
    file.exists(dest)
  }, error=function(e) FALSE)
}

kept <- character(0)
for (k in seq_len(nrow(candidates))) {
  if (download_one(candidates$pdf_url[k], id_vec[k])) {
    kept <- c(kept, id_vec[k])
    if (length(kept) == 20) break
  }
  Sys.sleep(runif(1, 1, 3))  # polite delay
}

cat("Saved ", length(kept), " PDFs to: ", normalizePath(save_dir), "\n", sep = "")
if (length(kept) < 20) cat("Note: listing may lack 20 working PDFs; export more results.\n")


