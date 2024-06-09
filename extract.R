
# Packages ----------------------------------------------------------------

library(curl)
library(purrr)


# Functions ---------------------------------------------------------------

# Generate base URL
base_url <- function(encode = TRUE) {
  url <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/"
  if (encode) {return(URLencode(url))}
  else {url}
}

# Generate URL to source files
create_url <- function(year, month) {
  base_url <- base_url()
  if (nchar(year) != 4) {stop("`year` must have four digits.", call. = FALSE)}
  if (nchar(month) != 2) {stop("`month` must have two digits.", call. = FALSE)}
  full_url <- paste0(base_url, year, paste0("/", year, month, "/"))
  return(full_url)
}

# Get directory/file names in FTP server
list_url_content <- function(url = base_url()) {
  con <- curl(url = url, open = "r", handle = new_handle(dirlistonly = TRUE))
  caged_ftp <- readLines(con)
  close(con)
  return(caged_ftp)
}

# Map files to download in FTP server
map_content <- function() {
  ftp_year <- list_url_content()
  caged_years <- ftp_year[grepl("\\d{4}", ftp_year, perl = TRUE, useBytes = TRUE)]

  ftp_yearmonth <- purrr::map(
    .x = caged_years,
    .f = ~list_url_content(url = paste0(base_url(), .x, "/"))
    ) |>
    purrr::set_names(caged_years)

  ftp_yearmonth <- purrr::map2(
    .x = ftp_yearmonth,
    .y = names(ftp_yearmonth),
    .f = ~paste0(base_url(), .y, "/", .x, "/")
    ) |>
    purrr::list_c()

  ftp_files <- purrr::map(
    .x = ftp_yearmonth,
    .f = ~paste0(.x, list_url_content(.x))
    ) |>
    purrr::list_c()

  return(ftp_files)

}

# Download files from FTP server
download_ftp <- function(url, dest_dir) {

  if (!dir.exists(dest_dir)) {dir.create(dest_dir)}

  curl_pool <- curl::new_pool()

  curl_success <- function(res) {
    cat("Request completed: ", res$url, " Status:", res$status, "\n")
    file_path <- paste(dest_dir, basename(res$url), sep = "/")
    writer <- file_writer(file_path)
    writer(res$content, close = TRUE)
  }

  curl_fail <- function(res) {
    cat("Request failed: ", res$url, " Status:", res$status, "\n")
  }

  purrr::walk(
    .x = url,
    .f = ~curl::curl_fetch_multi(
      url = .x,
      done = curl_success,
      fail = curl_fail,
      pool = curl_pool
    )
  )

  curl::multi_run(pool = curl_pool)

}
