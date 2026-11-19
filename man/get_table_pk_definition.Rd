% Generated documentation for get_table_pk_definition
\name{get_table_pk_definition}
\alias{get_table_pk_definition}
\title{Get Primary Key Column Name for a Database Table}
\usage{
get_table_pk_definition(table_name)
}
\arguments{
\item{table_name}{Character scalar database table name.}
}
\value{
A character scalar naming the primary key column, or NULL if undefined.
}
\description{
Looks up the primary key column name associated with a known CodingManageR database table.
}
\examples{
get_table_pk_definition("sources")
}
