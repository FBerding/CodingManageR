# Installing and Launching CodingManageR

## Welcome

**CodingManageR** is a web app for qualitative text coding and coder
training. You do not need to write code to use it. After you start the
app, you work with simple menus and buttons in your web browser.

This guide shows you three things:

1.  How to install **CodingManageR** from **CRAN** or **GitHub**.
2.  How to start the app on your own computer in under one minute.
3.  *(Optional, for IT administrators)* How to run the app on a Linux
    server for a team.

### Which Setup Do You Need?

| Your situation | Recommended setup | Section to follow |
|:---|:---|:---|
| You work alone or test the app on your laptop or PC | **Local Mode (SQLite)** — no database server needed | Sections 1 and 2 |
| Several coders sign in at the same time via a web link | **Multi-User Server Mode (Shiny Server + MariaDB)** | Sections 1 and 3 |

### Before You Start

Install **R (version 4.1.0 or newer)** on your computer first. If you
are new to R, also install **RStudio Desktop** (free). RStudio gives you
a simple console window. You can paste the commands below into that
window and press **Enter**.

------------------------------------------------------------------------

## 1. Installing CodingManageR

Open R or RStudio. Choose **one** of the two methods below. You only
need to install the package once per computer.

### Option 1: Install from CRAN (Recommended)

CRAN offers the official stable version. This is the easiest and safest
choice for most users:

``` r

install.packages("CodingManageR")
```

R downloads and installs all helper packages for you. The first install
can take a few minutes.

### Option 2: Install the Development Version from GitHub

Want the newest features before the next CRAN release? Install the app
from GitHub using `pak` (recommended) or `remotes`:

``` r

# Method A: using pak (recommended)
install.packages("pak")
pak::pak("FBerding/CodingManageR")

# Method B: using remotes
install.packages("remotes")
remotes::install_github("FBerding/CodingManageR")
```

> **Tip for R beginners:** R may ask if you want to update older
> packages. Pressing **Enter** or choosing `"All"` is usually safe.

------------------------------------------------------------------------

## 2. Launching on Your Own Computer (Local Mode)

In Local Mode, the app uses **SQLite**. SQLite is a simple database
saved as a single file on your computer. It holds all your scales,
texts, sources, users, and ratings. You do not need a separate database
server.

### Step 1: Start a Clean Project

Paste this command into the R console and press **Enter**:

``` r

CodingManageR::run_app()
```

Your browser or RStudio viewer opens automatically. You will see the
**CodingManageR** sign-in screen (see Figure 1).

![CodingManageR sign-in screen showing username and access token inputs
along with the English and German language
selector.](figures/01-login.png)

Figure 1: CodingManageR sign-in screen with username and access token
fields and the English/German language selector.

You can also choose a specific file name or folder for your project.
Pass the path with `sqlite_path`, or set the `SQLITE_PATH` environment
variable:

``` r

CodingManageR::run_app(sqlite_path = "my_coding_project.sqlite")
```

### Step 2: Optional — Try the App with Built-In Sample Data

Are you using **CodingManageR** for the first time? You can try a demo
database before you build your own project. Demo mode adds sample
scales, example texts, and test accounts:

``` r

CodingManageR::run_app(sqlite_path = "coding_demo.sqlite", empty = FALSE)
```

- Clean project start (`empty = TRUE`, default): creates empty tables
  and only the first administrator account.
- Sample demo start (`empty = FALSE`): also adds example scales, texts,
  sources, reference ratings, and a sample coder (`user1` /
  `token2026`).

### Step 3: Sign In and Change the Default Password

When you start a new database for the first time, the app creates one
administrator account:

- **Username:** `admin`
- **Access Token:** `geheim123`

Sign in with this account. Then open **Admin Console -\> User
Management** and change the `admin` access token right away.

> **New to CodingManageR?** Click **Getting Started** (*Erste Schritte*)
> in the top-right header after you sign in (see Figure 2). It starts an
> interactive step-by-step tour right inside the app.

![CodingManageR authenticated workspace displaying the top navigation
tabs, header actions including Getting Started, KPI summary cards, and
reliability progress chart.](figures/02-dashboard.png)

Figure 2: Authenticated CodingManageR workspace showing the navigation
tabs, Getting Started tour button, personal coding KPIs, and reliability
progress chart.

### Step 4: Managing Multiple Local Projects

In Local Mode, **one `.sqlite` file is one separate project**. Each file
keeps its own scales, sources, case texts, user accounts, ratings, and
training history.

If you run
[`CodingManageR::run_app()`](https://fberding.github.io/CodingManageR/reference/run_app.md)
without `sqlite_path`, the app always opens the same default file in
your R user folder. To keep different studies or courses separate, use a
different `sqlite_path` for each project:

``` r

# Project A: creates (or opens) "study_a.sqlite" in your working folder
CodingManageR::run_app(sqlite_path = "study_a.sqlite")

# Project B: creates (or opens) a second, separate database
CodingManageR::run_app(sqlite_path = "study_b.sqlite")
```

Want to store a project in its own subfolder? Create the folder first.
SQLite only creates new database files inside folders that already
exist:

``` r

dir.create("projects/study_2026", recursive = TRUE, showWarnings = FALSE)
CodingManageR::run_app(sqlite_path = "projects/study_2026/study_2026.sqlite")
```

To open the same project again later, run the command with the same
`sqlite_path`. All saved scales, texts, and ratings will be there.

#### Backing Up, Renaming, Moving, and Archiving Projects

Each project lives in one `.sqlite` file, so backups are easy. Follow
one important rule first:

1.  **Always stop the app first.** Click the red Stop button in RStudio,
    or press `Esc` or `Ctrl+C` in R. While the app runs, SQLite uses
    **Write-Ahead Logging (WAL)**. WAL keeps recent changes in temporary
    helper files (`-wal` and `-shm`). Stopping the app saves those
    helper files cleanly back into your main `.sqlite` file.
2.  **Back up or archive a copy:** Copy the `.sqlite` file to a backup
    drive or save a dated copy (for example
    `study_a_backup_2026-11-22.sqlite`).
3.  **Rename or move a project:** Rename or move the `.sqlite` file in
    your file explorer. Then open it with the new path:
    `CodingManageR::run_app(sqlite_path = "new_folder/renamed_project.sqlite")`.
4.  **Long-term research archiving:** Also export your data as **CSV**
    files (simple table files for Excel, R, or SPSS). You can download
    them in **Admin Console -\> Data & Import/Export**.

> **Important:** Local SQLite files are only for one person on one
> computer. Do not share one `.sqlite` file with several users at the
> same time on a network drive. Use the server setup below for teams.

------------------------------------------------------------------------

## 3. Optional: Multi-User Server Setup (For IT Administrators)

> **Note:** Skip this section if you only use CodingManageR on your own
> computer. This section is for IT staff or project admins who host the
> app on an **Ubuntu** or **Debian** Linux server. It uses **Shiny
> Server Open Source** (the web server) and **MariaDB/MySQL** (the
> shared database).

### Step 1: Install System Packages and MariaDB

Run these commands on the server with `sudo` rights:

``` bash
sudo apt update
sudo apt install -y r-base r-base-dev libmariadb-dev libcurl4-openssl-dev libssl-dev libxml2-dev mariadb-server gdebi-core wget
```

Start MariaDB, enable it at boot, and run its security setup:

``` bash
sudo systemctl start mariadb
sudo systemctl enable mariadb
sudo mariadb-secure-installation
```

### Step 2: Install CodingManageR and RMariaDB System-Wide

Install both packages into the system library. This lets the `shiny`
service user load them.

To install the stable release from **CRAN**:

``` bash
sudo R -e 'install.packages(c("CodingManageR", "RMariaDB"), repos = "https://cloud.r-project.org")'
```

Or install the development version from **GitHub**:

``` bash
sudo R -e 'install.packages(c("remotes", "RMariaDB"), repos = "https://cloud.r-project.org"); remotes::install_github("FBerding/CodingManageR")'
```

### Step 3: Create the Database and Database User

Open the MariaDB command line:

``` bash
sudo mariadb
```

Create the database and a database user. Replace `ChooseAStrongPassword`
with your own strong password:

``` sql
CREATE DATABASE IF NOT EXISTS kodierer_portal
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;

CREATE USER 'shiny_user'@'localhost' IDENTIFIED BY 'ChooseAStrongPassword';
GRANT ALL PRIVILEGES ON kodierer_portal.* TO 'shiny_user'@'localhost';
FLUSH PRIVILEGES;
EXIT;
```

### Step 4: Install Shiny Server Open Source

Download and install Shiny Server. You can check the Posit website for a
newer version number if needed:

``` bash
wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo gdebi shiny-server-1.5.22.1017-amd64.deb
```

### Step 5: Deploy the Application Entrypoint

Create a folder inside `/srv/shiny-server/`. Copy the ready-to-use
`app.R` launcher from **CodingManageR** into that folder:

``` bash
sudo mkdir -p /srv/shiny-server/codingmanager
sudo R -e 'file.copy(system.file("shiny", "app.R", package = "CodingManageR"), "/srv/shiny-server/codingmanager/app.R", overwrite = TRUE)'
sudo chown -R shiny:shiny /srv/shiny-server/codingmanager
```

### Step 6: Configure Database Credentials and Restart

Create an `.Renviron` file inside the app folder. This tells
CodingManageR to connect to MariaDB instead of SQLite:

``` bash
sudo nano /srv/shiny-server/codingmanager/.Renviron
```

Paste these lines and enter the password from Step 3:

``` ini
MYSQL_HOST=127.0.0.1
MYSQL_PORT=3306
MYSQL_USER=shiny_user
MYSQL_PASSWORD=ChooseAStrongPassword
MYSQL_DBNAME=kodierer_portal
```

Lock the file permissions so only the `shiny` user can read the
password. Then restart Shiny Server:

``` bash
sudo chown shiny:shiny /srv/shiny-server/codingmanager/.Renviron
sudo chmod 600 /srv/shiny-server/codingmanager/.Renviron
sudo systemctl restart shiny-server
```

Open `http://<server-ip>:3838/codingmanager/` in a web browser. On the
first visit, CodingManageR creates all 9 database tables in MariaDB
automatically. It also creates the `admin` account with token
`geheim123`. Sign in and change that password right away.

------------------------------------------------------------------------

## Next Steps

Now that CodingManageR is running, open the step-by-step workflow guide.
It shows how to set up scales, texts, sources, coders, training
sessions, and exports:

``` r

vignette("introduction", package = "CodingManageR")
```
