
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CodingManageR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/FBerding/CodingManageR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FBerding/CodingManageR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FBerding/CodingManageR/graph/badge.svg)](https://app.codecov.io/gh/FBerding/CodingManageR)
[![CRAN status](https://www.r-pkg.org/badges/version/CodingManageR)](https://CRAN.R-project.org/package=CodingManageR)
[![CRAN downloads per month](https://cranlogs.r-pkg.org/badges/CodingManageR)](https://CRAN.R-project.org/package=CodingManageR)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**CodingManageR** is a multi-user R Shiny web application and training portal for qualitative text coding across ordinal scales. It supports zero-configuration local SQLite databases for single-user desktop sessions as well as pooled MariaDB/MySQL backends for concurrent Open Source Shiny Server deployments.

## Core Features

- **Stratified Coder Training & 16 Reliability Metrics**: Draws balanced reference training pools covering every discrete scale level (`0..max_value`) and computes 16 inter-rater reliability and agreement metrics via `aifeducation` (including Cohen's Kappa, Krippendorff's Alpha, Gwet's AC1/AC2, Iota2, Kendall's W, and Macro F1).
- **Role-Based Live Coding Workspace**: Enforces granular per-user permissions across coding scales, text sources, and scientific coding sources, while automatically excluding already-coded and gold-standard cases.
- **Personal Progress Dashboard**: Tracks live codings, training ratings, completed sessions, reliability trajectories across all 16 metrics, and a searchable coding scale handbook.
- **8-Tab Administration Console**: Manages users (access tokens, active/inactive status, permissions), case texts, bibliographic sources, ordinal scales, team reliability comparisons, individual rating corrections, schema-validated CSV imports/exports, and a 9-table database integrity audit.
- **Bilingual Interface**: Built-in German (`de`) and English (`en`) interface with automatic browser language detection and live switching in the header.

## Installation

You can install **CodingManageR** either from CRAN or directly from GitHub.

### From CRAN

Install the stable release from CRAN:

``` r
install.packages("CodingManageR")
```

### From GitHub

Install the latest development version from GitHub using `pak` or `remotes`:

``` r
# Using pak (recommended)
install.packages("pak")
pak::pak("FBerding/CodingManageR")

# Or using remotes
install.packages("remotes")
remotes::install_github("FBerding/CodingManageR")
```

## Quick Start

### Local Single-User Mode (SQLite)

Launch the application with a fresh local SQLite database for your own project (creates only the initial administrator account):

``` r
library(CodingManageR)

# Start a clean project with a local SQLite database
run_app(sqlite_path = "coding_portal.sqlite", empty = TRUE)
```

If you want to explore the interface first with built-in sample scales, example texts, gold-standard reference cases, and demo accounts, start with `empty = FALSE`:

``` r
run_app(sqlite_path = "coding_demo.sqlite", empty = FALSE)
```

Default accounts created in demo mode (`empty = FALSE`):

| Role | Username | Access Token | Scope |
|:---|:---|:---|:---|
| Administrator | `admin` | `geheim123` | Full workspace + 8-tab Admin Console |
| Coder | `user1` | `token2026` | Dashboard, Training & Live Coding (assigned scales/sources) |
| Locked Demo | `inaktiv_demo` | `gesperrt123` | Deactivated account (`is_active = 0`) |

> In clean mode (`empty = TRUE`), sign in as `admin` with token `geheim123` and change the password immediately in **Admin Console -> User Management**.

### Multi-User Deployment (Shiny Server + MariaDB/MySQL)

For concurrent web sessions on Open Source Shiny Server, configure a pooled MariaDB/MySQL connection via environment variables (for example in `.Renviron`) before calling `run_app()`:

``` bash
MYSQL_HOST=127.0.0.1
MYSQL_PORT=3306
MYSQL_USER=coding_portal_user
MYSQL_PASSWORD=your_secure_password
MYSQL_DBNAME=kodierer_portal
```

## Documentation & Guides

Step-by-step guides written for beginners and administrators are bundled with the package:

- **Installation & Server Setup**: `vignette("CodingManageR", package = "CodingManageR")`
- **Step-by-Step Workflow Guide (Setup, Coding & Administration)**: `vignette("introduction", package = "CodingManageR")`

## Citation & License

**CodingManageR** is licensed under the **GNU General Public License v3.0 (GPL-3)**.

``` r
citation("CodingManageR")
```

If you encounter a bug or have a feature request, please open an issue on [GitHub Issues](https://github.com/FBerding/CodingManageR/issues).
