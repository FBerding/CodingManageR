# Einführung in CodingManageR (Deutsch)

## 1 Willkommen

**CodingManageR** unterstützt Forschende, Lehrende und Studierende
dabei, qualitative Textkodierprojekte direkt im Webbrowser
durchzuführen. Nach dem Start der Anwendung sind keine
R-Programmierkenntnisse mehr nötig: Projekte werden per Mausklick
eingerichtet, Kodiererinnen und Kodierer strukturiert geschult und
Bewertungen systematisch erfasst.

Diese Einführung folgt den drei zentralen Phasen eines Kodierprojekts:

| Teil | Schwerpunkt | Hauptzielgruppe |
|:---|:---|:---|
| **Teil 1: Erstmalige Einrichtung (Abschnitt 3)** | Admin-Passwort ändern sowie **Quellen**, **Kodierskalen** und **Falltexte** anlegen | Projektleitung / Administration |
| **Teil 2: Der Kodierprozess (Abschnitt 4)** | **Neue Benutzer** anlegen, **Kodierschulung** absolvieren und **Feldkodierungen** erfassen | Projektleitung & Kodierende |
| **Teil 3: Weitere Admin-Optionen (Abschnitt 5)** | Team-Qualität prüfen, Kodierungen korrigieren, Daten exportieren, CSV-Dateien hochladen und Datenbank prüfen | Projektleitung / Administration |

Haben Sie CodingManageR noch nicht installiert? Lesen Sie zuerst die
Installations- und Startanleitung:

``` r

vignette("CodingManageR-de", package = "CodingManageR")
```

> **Englische Version verfügbar:** Die englischsprachige Fassung dieser
> Einführung lässt sich über
> [`vignette("introduction", package = "CodingManageR")`](https://fberding.github.io/CodingManageR/articles/introduction.md)
> öffnen.

> **Hinweis zur KI-gestützten Erstellung:** Diese Vignette, der
> Quellcode des Pakets **CodingManageR** sowie alle integrierten
> Beispieldaten (Kodierskalen, Quellen und Falltexte) wurden mit Hilfe
> von KI erstellt.

------------------------------------------------------------------------

## 2 Bevor Sie beginnen

### 2.1 Zentrale Begriffe kurz erklärt

Wenn Sie neu in der qualitativen Textkodierung sind, helfen diese sechs
Begriffe bei der Orientierung in der Anwendung:

| Begriff in der App | Verständliche Bedeutung |
|:---|:---|
| **Kodierskala** (oder *Dimension*) | Eine Bewertungsregel mit geordneten ganzen Zahlen ab `0`. Beispiel: `0` = nicht vorhanden, `1` = teilweise vorhanden, `2` = deutlich ausgeprägt. |
| **Falltext** | Ein einzelner Textabschnitt, den eine kodierende Person liest und bewertet — etwa ein Unterrichtsprotokoll, ein Schüleraufsatz oder ein Reflexionsbericht. |
| **Quelle** | Eine bibliografische oder projektspezifische Herkunftsangabe für einen Falltext, eine Kodierskala bzw. ein Kodierhandbuch oder eine Bewertung selbst. Eine **Textquelle** dokumentiert, woher ein Falltext stammt. Eine **Kodierungsquelle** dokumentiert nicht nur die Herkunft von Skala, Leitfaden oder Studienprotokoll, sondern kann ebenso angeben, woher die **Kodierung (Bewertung eines Textes) selbst stammt** — zum Beispiel aus einer externen Studie, einem Expertenkonsens oder einem publizierten Benchmark-Datensatz. |
| **Goldstandard** (Referenzfall) | Ein Falltext mit abgestimmter Referenzbewertung. Zusammen mit weiteren bereits kodierten Fällen einer Kategorie dienen diese Referenzfälle als Übungsgrundlage zur Prüfung der Urteilsübereinstimmung in der Schulung. |
| **Intercoder-Reliabilität** | Ein statistisches Maß für die Übereinstimmung zwischen Beurteilenden (kann zwei oder mehr Kodierende umfassen). In der Kodierschulung von CodingManageR wird die Reliabilität nicht ausschließlich anhand expliziter `gold_standard`-Einträge berechnet, sondern über **alle bereits kodierten Referenzfälle einer Kategorie**. |
| **Zugangs-Token** | Das persönliche Passwort, das zusammen mit dem Benutzernamen bei der Anmeldung eingegeben wird. |

### 2.2 Anwendung starten und Sprache umschalten

Um ein neues, leeres lokales Projekt auf Ihrem Computer zu starten,
öffnen Sie R oder RStudio und führen folgenden Befehl aus:

``` r

CodingManageR::run_app_local(sqlite_path = "mein_projekt.sqlite")
```

Wird CodingManageR an Ihrer Hochschule oder in Ihrem Team auf einem
Server betrieben, öffnen Sie einfach den Weblink Ihrer Administration im
Browser.

> **Schnell-Tipp — erst einmal ausprobieren?** Testen Sie die Anwendung
> mit vorkonfigurierten Beispielskalen, Falltexten und Demokonten, bevor
> Sie Ihr eigenes Projekt aufbauen:
> `CodingManageR::run_app_local(sqlite_path = "demo_projekt.sqlite", empty = FALSE)`

> **Mehrere lokale Projekte parallel verwalten?** Im lokalen Modus
> speichert die App jedes Projekt in einer eigenen **SQLite**-Datei auf
> Ihrem Computer. Verwenden Sie pro Studie einen eigenen Dateinamen,
> etwa `CodingManageR::run_app_local(sqlite_path = "studie_a.sqlite")`
> und `CodingManageR::run_app_local(sqlite_path = "studie_b.sqlite")`.
> Beenden Sie die App immer zuerst, bevor Sie eine `.sqlite`-Datei
> kopieren, umbenennen oder archivieren. Weitere Tipps zu Ordnern und
> Backups finden Sie in
> [`vignette("CodingManageR-de", package = "CodingManageR")`](https://fberding.github.io/CodingManageR/articles/CodingManageR-de.md).

Die Sprache der Oberfläche lässt sich jederzeit zwischen **Deutsch** und
**Englisch** umschalten — sowohl direkt auf der Anmeldeseite als auch
oben rechts in der Kopfleiste. Ihre laufende Arbeit bleibt beim
Sprachwechsel vollständig erhalten.

### 2.3 Integrierte interaktive Einführung („Erste Schritte“)

Sie müssen diese Anleitung während der Arbeit nicht dauerhaft nebenher
geöffnet lassen. Klicken Sie nach der Anmeldung oben rechts in der
Kopfleiste auf **Erste Schritte**, um eine interaktive
Schritt-für-Schritt-Tour direkt in der App zu starten:

- **Rollenabhängige Pfade:** Administratorinnen und Administratoren
  erhalten eine **3-teilige Tour mit 12 Schritten**, die genau den
  Teilen 1, 2 und 3 dieser Vignette entspricht. Reguläre Kodiererinnen
  und Kodierer erhalten eine kompakte **4-Schritte-Tour** für **Teil 2**
  (Schulung, Dashboard und Feldkodierung).
- **Automatische Navigation:** Beim Klick auf „Weiter“ oder „Zurück“
  hebt die Tour das jeweilige Bedienelement mit einem kurzen Hinweis
  hervor und wechselt automatisch in den passenden Reiter.
- **Tastatur- und Live-Sprachunterstützung:** Navigieren Sie mit den
  **Pfeiltasten (Links/Rechts)** zwischen den Schritten und schließen
  Sie die Tour mit **Esc**. Schalten Sie in der Kopfleiste zwischen
  Deutsch und Englisch um, werden alle Tour-Texte sofort mitübersetzt.

------------------------------------------------------------------------

## 3 Teil 1: Erstmalige Einrichtung der Anwendung

Durchlaufen Sie für jedes neue Projekt die folgenden vier Schritte in
genau dieser Reihenfolge: **Quellen -\> Skalen -\> Falltexte**. So
stehen in allen Auswahlfeldern unmittelbar die passenden Einträge
bereit.

### 3.1 Als Administrator anmelden und Standardpasswort ändern

1.  Öffnen Sie die Anmeldeansicht (siehe Abbildung 1) und geben Sie die
    Standard-Zugangsdaten ein:
    - **Benutzername:** `admin`
    - **Zugangs-Token:** `geheim123`
2.  Klicken Sie auf **Einloggen**.
3.  Öffnen Sie in der oberen Navigationsleiste den Reiter
    **Admin-Bereich**. Sie befinden sich direkt im ersten Untertab
    **Benutzerverwaltung**.
4.  Klicken Sie neben `admin` auf die Bearbeiten-Schaltfläche, ersetzen
    Sie `geheim123` durch ein starkes persönliches Passwort und
    speichern Sie die Änderung.

> **Warum dieser Schritt wichtig ist:** Jede neu erstellte Datenbank
> beginnt mit demselben Standardpasswort. Ändern Sie es immer als
> Erstes, um Ihr Projekt abzusichern.

![Anmeldeansicht von CodingManageR auf Deutsch mit Feldern fuer
Benutzername und maskiertes Zugangs-Token, Schaltflaeche Einloggen sowie
Sprachauswahl.](figures-de/01-login.png)

Abbildung 1: Anmeldeansicht mit Feldern für Benutzername und
Zugangs-Token sowie der Sprachauswahl (Deutsch/Englisch) im Kartenkopf.

### 3.2 Schritt 1: Quellen anlegen („Quellenverwaltung“)

Bevor Sie Falltexte einspielen oder Kodierungen starten, legen Sie
mindestens zwei Quellen an: eine **Textquelle** (Herkunft Ihrer
Falltexte) sowie eine **Kodierungsquelle** (dokumentiert das
Kodierhandbuch bzw. Studienprotokoll **und/oder** die Herkunft der
Bewertungen selbst).

1.  Öffnen Sie im **Admin-Bereich** den Untertab **Quellenverwaltung**
    (siehe Abbildung 2).
2.  Klicken Sie auf **Neue Quelle hinzufügen** und füllen Sie die drei
    Pflichtfelder aus:
    - **Kurzreferenz** (z. B. `Example1` oder `Example2`): kompakter
      Bezeichner für Auswahlmenüs und Badges.
    - **Vollständige Referenz / Zitation**: vollständige bibliografische
      Angabe oder Projektbeschreibung.
    - **Lizenz / Nutzungsrechte**: wählen Sie eine Standardlizenz (z. B.
      `CC-BY 4.0`, `CC0 1.0` oder `Eigene Datenerhebung`) oder geben Sie
      eine individuelle Lizenzbezeichnung ein.
3.  Klicken Sie auf **Quelle speichern**. Wiederholen Sie den Vorgang
    für alle Textkorpora, Kodierleitfäden und externen Bewertungsquellen
    Ihres Projekts. Merken Sie sich die numerische **`ID`** aus der
    ersten Tabellenspalte — Sie benötigen diese als `source_id` beim
    CSV-Upload von Falltexten (siehe Abschnitt 3.4.1).

> **Viele Quellen bereits in einer Tabelle vorhanden?** Sie können
> sämtliche Quellen auch gesammelt per CSV-Vorlage importieren (siehe
> Abschnitt 5.4).

![Admin-Bereich Untertab Quellenverwaltung mit Liste wissenschaftlicher
Quellen, Kurzreferenzen, vollstaendigen Zitationen, Lizenz-Badges und
Bearbeitungsschaltflaechen.](figures-de/06-admin-sources.png)

Abbildung 2: Admin-Bereich — Quellenverwaltung zum Erfassen und Pflegen
von Textkorpora, Zitationen und Lizenzen.

### 3.3 Schritt 2: Kodierskalen definieren („Skalenverwaltung“)

Legen Sie als Nächstes die Bewertungsskalen fest, mit denen Ihr Team
arbeiten soll.

1.  Öffnen Sie im **Admin-Bereich** den Untertab **Skalenverwaltung**
    (siehe Abbildung 3).
2.  Klicken Sie auf **Neue Skalendefinition anlegen** und füllen Sie
    folgende Felder aus:
    - **Kurzlabel** (z. B. `KogAkt` oder `Feedback`): kompakte
      Bezeichnung für Tabellen und Diagramme.
    - **Skalenname**: vollständiger Titel der Dimension für die
      Kodierenden.
    - **Allgemeine Definition & Kodieranweisung**: kurze methodische
      Leitlinie, worauf beim Kodieren zu achten ist.
    - **Maximaler Wert (`max_value`)**: höchster Wert der Skala. Jede
      Skala beginnt bei `0`; bei `max_value = 3` entstehen
      beispielsweise vier Stufen (`0`, `1`, `2`, `3`).
    - **Stufenbeschreibungen (`0` bis `max_value`)**: klare Definitionen
      und Ankerbeispiele für jede Stufe. Kodierende sehen diese Texte
      live, sobald sie den Bewertungsregler bewegen.
3.  Klicken Sie auf **Skalendefinition anlegen** und wiederholen Sie den
    Vorgang für alle weiteren Skalen.

> **Wichtige Regel:** Alle Textbeschreibungen lassen sich später
> jederzeit bearbeiten. Der **`max_value` ist jedoch nach dem Anlegen
> unveränderlich gesperrt**, damit bestehende Kodierungen niemals ihre
> Bedeutung verlieren.

![Admin-Bereich Untertab Skalenverwaltung mit Uebersicht der
konfigurierten ordinalen Kodierskalen, Wertebereiche von 0 bis max_value
und Beschreibungen.](figures-de/07-admin-scales.png)

Abbildung 3: Admin-Bereich — Skalenverwaltung mit definierten ordinalen
Kodierskalen, Wertebereichen und Stufenbeschreibungen.

### 3.4 Schritt 3: Falltexte hinzufügen und Quellen zuweisen

Im nächsten Schritt laden Sie Ihre Falltexte in die Datenbank und
verknüpfen jeden Text mit seiner Textquelle.

#### 3.4.1 Falltexte gesammelt per CSV hochladen (empfohlen)

Eine **CSV-Datei** ist eine einfache Texttabelle, die Sie bequem in
Excel oder LibreOffice bearbeiten können.

1.  Öffnen Sie **Admin-Bereich -\> Daten & Import/Export**.
2.  Klicken Sie im Upload-Bereich für **Falltexte (`texts`)** auf
    **CSV-Vorlage herunterladen**. Sie erhalten eine leere Datei mit den
    exakten Spaltennamen (`text_id;text;source_id`).
3.  Tragen Sie Ihre Falltexte in die Spalte `text` und die passende
    numerische **`source_id`** in die Spalte `source_id` ein. Eigene
    `text_id`-Nummern können bei Bedarf ebenfalls vorgegeben werden.
    - **Wichtig (`source_id` muss einer vorhandenen Quelle
      entsprechen):** Jeder `source_id`-Wert in Ihrer CSV-Datei **muss
      exakt mit der numerischen `ID` einer bereits in der Datenbank
      angelegten Quelle übereinstimmen**. Existiert eine `source_id`
      nicht in der Tabelle `sources`, lehnt die strikte
      Fremdschlüssel-Prüfung den Import ab, um fehlerhafte Verknüpfungen
      zu verhindern.
    - **Wo Sie die gültigen Quellen-IDs finden:** Öffnen Sie
      **Admin-Bereich -\> Quellenverwaltung** (siehe Abbildung 2) und
      lesen Sie die allererste Spalte **`ID`** neben jeder registrierten
      Quelle ab (z. B. `1`, `2`, `3`). Alternativ können Sie unter
      **Admin-Bereich -\> Daten & Import/Export** die Tabelle `sources`
      als CSV herunterladen und die Spalte `source_id` direkt in Ihrer
      Tabellenkalkulation nachsehen.
4.  Speichern Sie die Datei als **UTF-8-CSV** (so bleiben Umlaute wie
    `ä`, `ö`, `ü`, `ß` erhalten) mit einem **Semikolon (`;`)** als
    Spaltentrennzeichen und laden Sie sie im selben Bereich hoch.
5.  Prüfen Sie die Vorschautabelle und bestätigen Sie den Import.

#### 3.4.2 Falltexte prüfen und Quellen in der Tabelle zuweisen

1.  Öffnen Sie **Admin-Bereich -\> Falltexte & Quellenzuweisung** (siehe
    Abbildung 4).
2.  Nutzen Sie das Suchfeld, um Texte nach Inhalt, Text-ID oder Quelle
    zu filtern. Die Tabelle zeigt bis zu 25 Einträge pro Seite.
3.  Klicken Sie in einer Zeile auf **Quelle zuweisen**, um die
    Primärquelle des Falltexts festzulegen oder zu ändern.

![Admin-Bereich Untertab Falltexte und Quellenzuweisung mit
durchsuchbarer, paginierter Falltext-Tabelle und zugeordneten
Textquellen.](figures-de/08-admin-texts.png)

Abbildung 4: Admin-Bereich — Falltexte & Quellenzuweisung mit
Suchfunktion, Paginierung und individueller Quellenzuordnung.

#### 3.4.3 Referenzfälle für die Kodierschulung vorbereiten

Für **1. Kodierschulung** benötigt die Datenbank bereits kodierte
Referenzfälle in der jeweiligen Kategorie — dazu zählen sowohl explizite
**Goldstandards** (`typ = gold_standard`) als auch abgeschlossene
Feldkodierungen (`typ = kodierung`). Stellen Sie idealerweise sicher,
dass für jede Skalenstufe von `0` bis `max_value` mindestens ein
kodierter Referenzfall vorliegt.

Dafür gibt es zwei einfache Wege:

- **Option A (direkt in der App):** Melden Sie sich als `admin` an,
  öffnen Sie **2. Kodierungen** und bewerten Sie selbst ein Startpaket
  an Beispieltexten.
- **Option B (CSV-Upload):** Laden Sie unter **Daten & Import/Export**
  die CSV-Vorlage für **Bewertungen & Goldstandard (`user_ratings`)**
  herunter, tragen Sie Ihre Referenzbewertungen ein (achten Sie auch
  hier auf gültige `source_id`-Werte aus der **Quellenverwaltung**) und
  laden Sie die Datei hoch.

### 3.5 Checkliste Teil 1 — Bereit für das Team?

Standardpasswort des `admin`-Kontos geändert.

Mindestens eine **Textquelle** und eine **Kodierungsquelle** in der
**Quellenverwaltung** angelegt (und deren numerische `ID`-Werte
notiert).

Alle **Kodierskalen** mit klaren Beschreibungen für alle Stufen `0` bis
`max_value` erstellt.

**Falltexte** mit gültigen `source_id`-Werten hochgeladen und ihren
Quellen zugeordnet.

Bereits kodierte Referenzfälle oder Goldstandards für alle
Schulungsskalen hinterlegt.

------------------------------------------------------------------------

## 4 Teil 2: Der Kodierprozess

Die Projekteinrichtung ist abgeschlossen. Im nächsten Schritt legen Sie
Benutzerkonten für Ihr Team an, lassen die Kodierenden in der
**Kodierschulung** üben und starten die **Feldkodierung**.

### 4.1 Schritt 1: Neue Benutzer anlegen und Rechte freischalten („Benutzerverwaltung“)

CodingManageR unterscheidet zwei Benutzerrollen:

- **Kodierer / User (`user`)**: Sieht **Dashboard**, **1.
  Kodierschulung** und **2. Kodierungen**. In der Feldkodierung stehen
  ausschließlich jene Skalen, Textquellen und Kodierungsquellen zur
  Verfügung, die von der Administration freigeschaltet wurden.
- **Administrator (`admin`)**: Besitzt Vollzugriff auf alle Skalen, alle
  Quellen sowie den gesamten **Admin-Bereich**.

So legen Sie eine neue kodierende Person an:

1.  Öffnen Sie **Admin-Bereich -\> Benutzerverwaltung** (siehe
    Abbildung 5) und klicken Sie auf **Neuen Benutzer anlegen**.
2.  Vergeben Sie einen eindeutigen **Benutzernamen**, wählen Sie die
    Rolle **Kodierer / User** und belassen Sie den Login-Status auf
    **Aktiv**. Geben Sie ein **Zugangs-Token** ein oder erzeugen Sie per
    Schaltfläche **Generieren** ein Zufallspasswort.
3.  Speichern Sie das Konto und teilen Sie Benutzername und
    Zugangs-Token sicher mit der kodierenden Person.
4.  Suchen Sie das neue Konto in der Tabelle und legen Sie über die
    Schaltflächen **Quellen** und **Skalen** fest:
    - Welche **Kodierskalen** die Person im Modul **2. Kodierungen**
      bewerten darf.
    - Aus welchen **Textquellen** (Text-Pools) Falltexte geladen werden
      dürfen.
    - Unter welchen **Kodierungsquellen** (Protokollen) die Bewertungen
      gespeichert werden.

> **Konto später vorübergehend pausieren?** Stellen Sie den Login-Status
> in der **Benutzerverwaltung** einfach auf **Inaktiv**. Die Person kann
> sich nicht mehr anmelden, alle bisherigen Bewertungen bleiben aber
> vollständig erhalten.

![Admin-Bereich Untertab Benutzerverwaltung mit Benutzerkonten, Rollen,
maskierten Admin-Tokens, Aktiv-Status-Schaltern sowie Skalen- und
Quellenfreigaben.](figures-de/05-admin-users.png)

Abbildung 5: Admin-Bereich — Benutzerverwaltung mit Benutzerrollen,
geschützten Admin-Passwörtern, Login-Status und
Skalen-/Quellenberechtigungen.

### 4.2 Schritt 2: In der Kodierschulung üben und das Dashboard auswerten

Kodiererinnen und Kodierer sollten zunächst in **1. Kodierschulung**
üben, bevor sie reale Projektdaten kodieren. Jedes angemeldete Konto
kann auf allen Skalen trainieren, für die bereits kodierte
Referenzbewertungen vorliegen.

#### 4.2.1 Eine Schulungssitzung absolvieren

1.  Melden Sie sich an und öffnen Sie den Reiter **1. Kodierschulung**.
2.  Wählen Sie im Auswahlfeld **Dimension / Skala wählen** die
    gewünschte Skala aus.
3.  Legen Sie die **Anzahl Trainingsfälle** fest. Die Anwendung zieht
    automatisch eine ausgewogene Auswahl bereits kodierter Referenzfälle
    für diese Kategorie, sodass jede Skalenstufe (`0` bis `max_value`)
    abgedeckt ist.
4.  Optional: Klicken Sie auf **Beispiele & Kriterien anzeigen**, um
    Referenzbeispiele vor dem Start nachzuschlagen.
5.  Klicken Sie auf **Training starten**.
6.  Bewerten Sie jeden Falltext (siehe Abbildung 6):
    - Lesen Sie links den Falltext.
    - Wählen Sie rechts über den Regler Ihre Einstufung; die
      Stufenbeschreibung aktualisiert sich live.
    - Klicken Sie auf **Speichern & Nächster Fall**. Beim letzten Fall
      klicken Sie auf **Training abschließen & Auswertung anzeigen**.
      Mit **Training abbrechen** beenden Sie eine Sitzung vorzeitig ohne
      Speicherung.
7.  Zum Abschluss vergleicht die App Ihre Bewertungen über alle
    gezogenen Fälle hinweg mit den hinterlegten Referenzkodierungen. Sie
    erhalten alle 16 Reliabilitätskennzahlen sowie eine
    Fall-für-Fall-Vergleichstabelle. Über **Schulungsergebnisse als CSV
    herunterladen** sichern Sie das Protokoll oder starten mit **Neue
    Schulung starten** einen weiteren Durchgang.

![Arbeitsbereich 1. Kodierschulung auf Deutsch mit Falltext links und
ordinalem Bewertungsregler samt Live-Stufendefinition
rechts.](figures-de/03-coding-training.png)

Abbildung 6: Arbeitsbereich „1. Kodierschulung“ mit aktuellem
Referenzfalltext, Bewertungsregler und dynamischer Stufenbeschreibung.

#### 4.2.2 Persönlichen Fortschritt im „Dashboard“ prüfen

Öffnen Sie jederzeit den Reiter **Dashboard**, um Ihren Fortschritt zu
verfolgen (siehe Abbildung 7):

- Untertab **Lern- & Arbeitsfortschritt**:
  - Die Kennzahlen-Karten oben zeigen Ihre **Echten Kodierungen**,
    **Schulungsbewertungen** und abgeschlossenen **Schulungssitzungen**.
  - Das Verlaufsdiagramm zeigt, wie sich Ihre Übereinstimmung über
    mehrere Sitzungen entwickelt — wahlweise für die Kernmetriken im
    Vergleich oder für jede der 16 Einzelmetriken.
  - Die Tabellen darunter listen sämtliche Schulungsdurchläufe auf und
    zeigen, wie viele offene Fälle pro Skala noch verbleiben.
- Untertab **Skalenübersicht & Anforderungen**:
  - Bietet ein durchsuchbares Skalenhandbuch aller freigeschalteten
    Kodierskalen mit Wertebereichen und detaillierten Stufentexten.

> **Tipp:** Klicken Sie in der Kopfleiste auf **Ansicht aktualisieren**,
> falls die Administration während Ihrer laufenden Sitzung neue Skalen,
> Texte oder Berechtigungen freischaltet.

![Persoenliches Dashboard auf Deutsch mit Kennzahlen-Karten fuer Echte
Kodierungen, Schulungsbewertungen und Sitzungen sowie dem
Reliabilitaetsverlaufsdiagramm.](figures-de/02-dashboard.png)

Abbildung 7: Persönliches Dashboard mit Kennzahlen-Karten, zeitlichem
Reliabilitätsverlauf über Schulungssitzungen und Aktivitätstabellen pro
Skala.

### 4.3 Schritt 3: Reale Bewertungen in „2. Kodierungen“ erfassen

Sobald die Schulung erfolgreich abgeschlossen ist und die nötigen
Freigaben vorliegen, beginnt die eigentliche Feldkodierung.

#### 4.3.1 Arbeitsbereich konfigurieren

1.  Öffnen Sie den Reiter **2. Kodierungen**.
2.  Treffen Sie drei Einstellungen:
    - **Kodierdimension / Skala:** Wählen Sie eine Ihrer
      freigeschalteten Skalen.
    - **Fallauswahl-Modus:** Ziehen Sie zufällige unkodierte Fälle aus
      allen freigegebenen Textquellen oder gezielt aus einer bestimmten
      Textquelle.
    - **Kodierquelle / Protokoll:** Wählen Sie die Kodierungsquelle,
      unter der Ihre Bewertungen gespeichert werden.
3.  Prüfen Sie den Live-Zähler der noch offenen Fälle und klicken Sie
    auf **Kodierung starten / Fall laden**.

#### 4.3.2 Falltexte bewerten

1.  Lesen Sie links den Falltext (siehe Abbildung 8) und prüfen Sie oben
    im Banner die aktive Skala sowie Text- und Kodierungsquelle.
2.  Wählen Sie rechts mit dem Regler Ihre Einstufung. Orientieren Sie
    sich an der Live-Stufenbeschreibung oder öffnen Sie bei Grenzfällen
    **Beispiele & Kriterien anzeigen**.
3.  Klicken Sie auf **Speichern & Nächster Fall**. Die Bewertung wird
    direkt gespeichert und der nächste offene Falltext automatisch
    geladen.
4.  Wenn Sie die Skala wechseln oder pausieren möchten, kehren Sie
    jederzeit über **Zurück zur Konfiguration** zum Einstellungsbereich
    zurück.

Bereits kodierte Falltexte sowie Goldstandard-Referenzfälle werden
automatisch übersprungen. Sind alle Fälle im gewählten Pool bearbeitet,
informiert Sie die Anwendung, sodass Sie zur nächsten Skala oder
Textquelle wechseln können.

![Arbeitsbereich 2. Kodierungen auf Deutsch mit aktivem Skalen- und
Quellen-Banner, Falltext links, Bewertungsregler mit Stufenbeschreibung
rechts und Schaltflaeche Speichern und Naechster
Fall.](figures-de/04-live-coding.png)

Abbildung 8: Arbeitsbereich „2. Kodierungen“ mit Skalen- und
Quellen-Banner, Falltextansicht, Bewertungsregler mit
Live-Stufenbeschreibung und Speichersteuerung.

### 4.4 Checkliste Teil 2 — Reibungsloser Kodieralltag

Benutzerkonto mit Zugangs-Token in der **Benutzerverwaltung** angelegt.

Erforderliche **Skalen**, **Textquellen** und **Kodierungsquellen** für
das Konto freigeschaltet.

Mindestens eine Sitzung in **1. Kodierschulung** abgeschlossen und
Ergebnisse im **Dashboard** geprüft.

In **2. Kodierungen** Skala, Text-Pool und Kodierungsquelle gewählt und
Bewertungen mit **Speichern & Nächster Fall** erfasst.

------------------------------------------------------------------------

## 5 Teil 3: Weitere Optionen für Administratorinnen und Administratoren

Der **Admin-Bereich** bietet fünf zusätzliche Werkzeuge für
Qualitätskontrolle, Korrekturen, Datenexporte, Massenimporte und
Datenbankwartung.

### 5.1 Team-Qualität überwachen („Reliabilität & Fortschritt“)

Öffnen Sie den Untertab **Reliabilität & Fortschritt**, um Aktivität und
Urteilsübereinstimmung im gesamten Team zu vergleichen (siehe Abbildung
9):

- Prüfen Sie projektweite Summen zu Feldkodierungen, abgeschlossenen
  Schulungssitzungen und Team-Durchschnitten.
- Filtern Sie gezielt nach **Kodierer**, **Dimension / Skala** und
  **Reliabilitätsmaß** und vergleichen Sie die Ergebnisse im
  Balkendiagramm sowie in der Übersichtstabelle.
- Klicken Sie in der Tabelle bei einer Person auf **Details**, um ein
  Detailfenster mit sämtlichen Einzelsitzungen und Feldkodierungen zu
  öffnen. So erkennen Sie rasch Skalen, bei denen zusätzliche
  Ankerbeispiele oder Team-Abstimmungen hilfreich sind.

![Admin-Bereich Untertab Reliabilitaet und Fortschritt auf Deutsch mit
Team-Kennzahlen, Filtersteuerung, Balkendiagramm zum Kodierer-Vergleich
und Uebersichtstabelle.](figures-de/09-admin-reliability.png)

Abbildung 9: Admin-Bereich — Reliabilität & Fortschritt zum Vergleich
der Schulungsleistungen und Intercoder-Reliabilität im Team.

### 5.2 Gespeicherte Bewertungen finden und korrigieren („Kodierungen korrigieren“)

Gelegentlich wird beim Kodieren versehentlich eine falsche Stufe
angeklickt und gespeichert.

1.  Öffnen Sie den Untertab **Kodierungen korrigieren** (siehe Abbildung
    10).
2.  Filtern Sie gespeicherte Einträge nach **Bewertungstyp**
    (`Feldkodierung`, `Goldstandard` oder `Schulung`), **Skala /
    Dimension**, Kodierer, Fall-ID oder einem Suchbegriff aus dem Text.
3.  Klicken Sie in der gewünschten Zeile auf **Wert ändern** und wählen
    Sie den korrigierten Wert. Die App stellt automatisch sicher, dass
    der neue Wert im zulässigen Bereich `0..max_value` der Skala liegt.

![Admin-Bereich Untertab Kodierungen korrigieren auf Deutsch mit Filtern
nach Bewertungstyp, Skala und Suchbegriff sowie durchsuchbarer Tabelle
mit Schaltflaechen Wert aendern.](figures-de/10-admin-edit-codings.png)

Abbildung 10: Admin-Bereich — Kodierungen korrigieren zum gezielten
Filtern nach Typ, Skala, Kodierer oder Stichwort sowie zur sicheren
Wertkorrektur.

### 5.3 Forschungsdatensätze exportieren und sichern („Daten & Import/Export“)

Nutzen Sie diesen Untertab für statistische Auswertungen in R, SPSS,
Excel oder Python sowie für regelmäßige Datensicherungen (siehe
Abbildung 11):

- **Master-Datenexport:** Führt Falltexte, Bewertungen,
  Skalendefinitionen und vollständige Quellenangaben (Text- und
  Kodierungsquellen) in einer auswertungsfertigen CSV-Matrix zusammen.
  Vor dem Download können Sie gezielt nach **Skalen**, **Quellen** und
  **Benutzern** filtern.
- **Einzel-Tabellen als CSV herunterladen:** Sichert jede Kerntabelle
  einzeln (`scale_definitions`, `texts`, `sources`, `user_ratings` und
  `coding_training`) für transparente Archivierung.
- **Vollständige lokale Projektsicherung (SQLite-Modus):** Im lokalen
  Modus können Sie zusätzlich das gesamte Projekt in einem Schritt
  sichern — beenden Sie dazu die App zuerst sauber (damit SQLite die
  WAL-Dateien schließt) und kopieren Sie danach die `.sqlite`-Datei.

### 5.4 Massen-Uploads per CSV mit integrierter Validierung („Daten & Import/Export“)

Fünf Tabellen lassen sich per CSV-Datei gesammelt erweitern oder
aktualisieren: **Skalen**, **Falltexte**, **Quellen-Verzeichnis**,
**Bewertungen & Goldstandard** sowie **Schulungssitzungen**.

So gelingt der Upload auf Anhieb:

1.  Klicken Sie neben dem Upload-Feld auf **CSV-Vorlage herunterladen**,
    damit alle Spaltennamen exakt übereinstimmen.
2.  Prüfen Sie beim Upload von **Falltexten (`texts`)** oder
    **Bewertungen (`user_ratings`)**, dass jede `source_id` genau einer
    existierenden numerischen **`ID`** aus der ersten Spalte unter
    **Admin-Bereich -\> Quellenverwaltung** entspricht (siehe Abschnitt
    3.4.1).
3.  Speichern Sie Ihre Tabelle als **UTF-8-CSV** (erhält Umlaute und
    Sonderzeichen) mit einem **Semikolon (`;`)** als Trennzeichen —
    nicht mit Komma.
4.  Laden Sie die Datei hoch. Die Anwendung bereinigt kleinere
    Formatierungsabweichungen und prüft vor jeder Datenbankänderung
    sämtliche Spalten und Fremdschlüssel.
5.  Im Bestätigungsdialog prüfen Sie Zeilenzahlen und mögliche
    ID-Konflikte: Bei bereits vorhandenen IDs warnt die App und lässt
    Sie entscheiden, ob bestehende Datensätze übersprungen oder mit den
    neuen CSV-Werten aktualisiert werden sollen. Jeder Import läuft
    innerhalb einer geschützten **Datenbank-Transaktion**: Tritt ein
    Fehler auf, wird der Vorgang vollständig zurückgerollt und Ihr
    bestehender Datenbestand bleibt unverändert.

![Admin-Bereich Untertab Daten und Import/Export auf Deutsch mit Filtern
fuer den Master-Datenexport, Einzel-Tabellen-Downloads und
CSV-Upload-Steuerung.](figures-de/11-admin-data-import-export.png)

Abbildung 11: Admin-Bereich — Daten & Import/Export für gefilterte
Master-Datenexporte, Rohdaten-Backups und schema-validierte
CSV-Massenimporte.

### 5.5 Datenbankzustand und Integrität prüfen („DB-Diagnose“)

Öffnen Sie **DB-Diagnose** nach größeren CSV-Imports oder regelmäßig im
Verlauf längerer Studien (siehe Abbildung 12):

- **Status & Bestand aller 9 Tabellen:** Zeigt aktuelle Zeilenzahlen,
  Spaltenanzahlen und Prüfsummen für alle 9 Tabellen (`user_tokens`,
  `sources`, `texts`, `scale_definitions`, `user_ratings`,
  `coding_training` sowie die drei Berechtigungstabellen).
- **Fremdschlüssel- & Konsistenz-Audit:** Prüft auf verwaiste
  Verknüpfungen — etwa Bewertungen ohne gültigen Falltext oder Falltexte
  ohne existierende Quelle — und bestätigt, dass der
  Fremdschlüssel-Schutz aktiv ist.

![Admin-Bereich Untertab DB-Diagnose auf Deutsch mit aktuellen
Zeilenzahlen fuer alle neun Datenbanktabellen und Statuskarten des
Integritaets-Audits.](figures-de/12-admin-db-diagnosis.png)

Abbildung 12: Admin-Bereich — DB-Diagnose mit Live-Zeilenzahlen aller
neun Datenbanktabellen und automatischem Fremdschlüssel-Audit.

### 5.6 Kurzübersicht für Administratorinnen und Administratoren

| Was möchten Sie tun? | Passender Untertab im Admin-Bereich |
|:---|:---|
| Kodierer anlegen, Passwort zurücksetzen oder Skalen & Quellen freischalten | **Benutzerverwaltung** |
| Falltexte durchsuchen und Textquellen zuweisen | **Falltexte & Quellenzuweisung** |
| Zitationen, Lizenzen und Kodierungsquellen verwalten | **Quellenverwaltung** |
| Neue Skala anlegen oder Stufenbeschreibungen pflegen | **Skalenverwaltung** |
| Schulungsleistungen und Intercoder-Reliabilität im Team vergleichen | **Reliabilität & Fortschritt** |
| Einzelne gespeicherte Bewertung finden und korrigieren | **Kodierungen korrigieren** |
| Auswertungsfertige CSV-Daten exportieren oder CSV-Tabellen importieren | **Daten & Import/Export** |
| Tabellenbestände und Datenbank-Integrität prüfen | **DB-Diagnose** |
