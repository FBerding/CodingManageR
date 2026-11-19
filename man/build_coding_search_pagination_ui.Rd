% Generated documentation for build_coding_search_pagination_ui
\name{build_coding_search_pagination_ui}
\alias{build_coding_search_pagination_ui}
\title{Build Pagination Controls for Coding Search Results}
\usage{
build_coding_search_pagination_ui(ns, total_rows, cur_page, tot_pages,
  page_size = 25, lang = "en")
}
\arguments{
\item{ns}{Shiny namespace function created via shiny::NS().}
\item{total_rows}{Integer scalar total number of matching rows.}
\item{cur_page}{Integer scalar current 1-based page index.}
\item{tot_pages}{Integer scalar total number of pages.}
\item{page_size}{Integer scalar number of rows displayed per page.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A Shiny UI tag object.
}
\description{
Constructs Bootstrap pagination controls and result summary badges for the admin coding search and correction view.
}
\examples{
\dontrun{
build_coding_search_pagination_ui(shiny::NS("mod_admin"), 100, 1, 4)
}
}
