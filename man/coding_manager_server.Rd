% Generated documentation for coding_manager_server
\name{coding_manager_server}
\alias{coding_manager_server}
\title{Root Shiny Server Function for CodingManageR}
\usage{
coding_manager_server(input, output, session, db_con = NULL, sqlite_path = NULL,
  config = NULL, seed_empty = NULL)
}
\arguments{
\item{input}{Standard Shiny server input object.}
\item{output}{Standard Shiny server output object.}
\item{session}{Standard Shiny server session object.}
\item{db_con}{Active DBI database connection or pool::dbPool object.}
\item{sqlite_path}{Optional character path to a local SQLite database file.}
\item{config}{Optional named list of database connection parameters.}
\item{seed_empty}{Optional logical controlling whether fresh databases receive only minimal default accounts.}
}
\value{
Invisibly returns NULL.
}
\description{
Manages database connectivity, user authentication and account lockout, bilingual localization, reactive scale catalog updates, and child module orchestration.
}
\examples{
\dontrun{
shiny::shinyApp(ui = coding_manager_ui(), server = coding_manager_server)
}
}
