# lsl – Ladesäulenregister Loader & Converter

Lädt und konvertiert das offizielle Ladesäulenregister der Bundesnetzagentur in ein besser brauchbares JSON-Format.

## Online-Zugriff auf die JSON-Datei

Die aktuelle `ladesaeulen.json.gz` wird wöchentlich per [GitHub Action](#github-action) aktualisiert und ist
über GitHub Pages abrufbar:

**https://ratopi.github.io/ladesaeule/ladesaeulen.json.gz**

> **Hinweis:** Die Datei ist gzip-komprimiert (ca. 5–10 MB statt 80+ MB unkomprimiert).
> Die meisten HTTP-Clients und Programmiersprachen können gzip transparent dekomprimieren.

## Bauen

Voraussetzung: Erlang/OTP und [rebar3](https://rebar3.org/) müssen installiert sein.

    rebar3 as prod escriptize

Erzeugt das ausführbare Escript unter `_build/prod/bin/lsl`.

## Ausführen

    _build/prod/bin/lsl

Das Tool ermittelt die aktuelle CSV-URL automatisch von der
[E-Mobilitäts-Seite der Bundesnetzagentur](https://www.bundesnetzagentur.de/DE/Fachthemen/ElektrizitaetundGas/E-Mobilitaet/start.html),
lädt die CSV-Datei herunter und schreibt das Ergebnis nach `public/ladesaeulen.json`.

## Testen

    rebar3 eunit

Die Tests umfassen:

- **`lsl_converter_tests`** – Konvertierungs-Tests mit gekürzten CSV-Dateien
  unter `test/data/` (UTF-8 und ISO-8859-1), ohne HTTP-Zugriff.
- **`cell_parser_tests`** – Unit-Tests für den CSV-Parser, u.a. für maskierte
  Zellen, Anführungszeichen in nicht-quotierten Zellen und Chunked Input.

## Wie die Konvertierung abläuft

### 1. URL ermitteln

`lsl_loader` lädt die E-Mobilitäts-Startseite der Bundesnetzagentur und extrahiert per Regex
den Link zur aktuellen CSV-Datei (der Dateiname enthält ein Datum, das sich bei jeder
Aktualisierung ändert).

### 2. Prüfen ob ein Update nötig ist

`lsl_loader` liest über `lsl_json` das `meta`-Objekt aus der bestehenden
`public/ladesaeulen.json` (falls vorhanden) und vergleicht:

- **URL** – hat sich der Dateiname/Link geändert?
- **Last-Modified** – wird per HTTP HEAD-Request geholt und mit dem gespeicherten
  Wert verglichen.

Nur wenn sich etwas geändert hat (oder noch keine Ausgabe existiert), wird die CSV
heruntergeladen und konvertiert.

### 3. CSV streamen & parsen

Die CSV wird per HTTP-Streaming (`httpc`, async) heruntergeladen.
Die empfangenen Chunks werden direkt dem `cell_parser` übergeben – einem
Continuation-basierten CSV-Parser, der Semikolon-getrennte, optional in Anführungszeichen
maskierte Zellen verarbeitet. Die Encoding-Erkennung geschieht automatisch: ein UTF-8 BOM
wird erkannt und entfernt, ansonsten wird heuristisch zwischen UTF-8 und ISO-8859-1
unterschieden (ISO-8859-1 wird on-the-fly in UTF-8 konvertiert). Anführungszeichen
innerhalb nicht-quotierter Zellen werden tolerant als normales Zeichen behandelt.

### 4. Header und Info-Zeilen verarbeiten

Die CSV beginnt mit mehreren Info-Zeilen (Titel, Hinweise, Aktualisierungsdatum).
Diese werden von `lsl_converter` in das `infos`-Array der JSON-Ausgabe übernommen.

Dann folgen zwei Header-Zeilen:

- **Zeile 1** – Gruppierung: „Allgemeine Informationen", „1. Ladepunkt" bis „6. Ladepunkt".
  Diese Zeile dient als Trigger für den Übergang vom Info-Bereich zum Datenbereich.
- **Zeile 2** – Die eigentlichen Spaltennamen (z.B. `Betreiber`, `Steckertypen1`, `P2 [kW]`).
  Die Zuordnung zu den Ladepunkten 1–6 erfolgt über das Suffix im Spaltennamen.

### 5. Datenzeilen in JSON konvertieren

Jede Datenzeile wird von `lsl_converter` anhand der Spaltennamen in eine verschachtelte Map überführt:

| CSV-Spalte(n)                | JSON-Ziel                         |
|------------------------------|-----------------------------------|
| Ladeeinrichtungs-ID         | `id`                              |
| Betreiber                   | `operator`                        |
| Anzeigename (Karte)         | `display_name`                    |
| Status                      | `status`                          |
| Art der Ladeeinrichtung     | `device_type`                     |
| Straße, Hausnummer, …       | `addr { Straße, Hausnummer, … }`  |
| Breitengrad, Längengrad     | `geo { lat, lon }` (als Zahlen)   |
| Standortbezeichnung         | `location_name`                   |
| Informationen zum Parkraum  | `parking_info`                    |
| Bezahlsysteme               | `payment`                         |
| Öffnungszeiten, …           | `opening_hours`, `opening_weekdays`, `opening_daytime` |
| Inbetriebnahmedatum, …      | `charging { Inbetriebnahmedatum, Nennleistung … }` |
| Steckertypen1–6, P1–6, …   | `charging.points[]` – Array mit je `plugs`, `power`, `kW`, `evse_id`, `pkey` |

Leere Zellen werden übersprungen. Ladepunkte ohne Daten tauchen nicht im Array auf.
Werte mit Komma-Dezimaltrenner (z.B. Koordinaten) werden in Fließkommazahlen konvertiert.

### 6. Meta-Informationen anhängen

Am Ende der JSON-Datei wird über `lsl_json` ein `meta`-Objekt geschrieben mit:

- `source` – die URL der CSV-Datei
- `download_time` – Zeitstempel des Downloads
- `source_last_modified` – der `Last-Modified`-Header des HTTP-Response (Zeitstempel der Datei auf dem Server)

### 7. Gzip-Komprimierung

Nach dem Schreiben wird die JSON-Datei mit `zlib:gzip/1` komprimiert und als
`ladesaeulen.json.gz` gespeichert. Die unkomprimierte Datei wird anschließend gelöscht –
nur die `.gz`-Version wird veröffentlicht, um die `gh-pages`-Historie klein zu halten.

### Struktur der Ausgabe

```json
{
  "infos": [
    "Ladesäulenregister Bundesnetzagentur",
    "Hinweis: ",
    "..."
  ],
  "data": [
    {
      "id": "1010338",
      "operator": "Albwerk Elektro- und Kommunikationstechnik GmbH",
      "display_name": "Albwerk Elektro- und Kommunikationstechnik GmbH",
      "status": "In Betrieb",
      "device_type": "Normalladeeinrichtung",
      "addr": {
        "Straße": "Am Berg",
        "Hausnummer": "1",
        "Postleitzahl": "72535",
        "Ort": "Heroldstatt",
        "Bundesland": "Baden-Württemberg",
        "Kreis/kreisfreie Stadt": "Landkreis Alb-Donau-Kreis"
      },
      "geo": { "lat": 48.442398, "lon": 9.659075 },
      "charging": {
        "Inbetriebnahmedatum": "11.01.2020",
        "Nennleistung Ladeeinrichtung [kW]": "22",
        "points": [
          {
            "plugs": ["AC Typ 2 Steckdose"],
            "power": ["22"],
            "evse_id": ["DEAEWE002501"],
            "pkey": "CA49E2E0..."
          },
          {
            "plugs": ["AC Typ 2 Steckdose"],
            "power": ["22"],
            "evse_id": ["DEAEWE002502"],
            "pkey": "AC676D2E..."
          }
        ]
      },
      "payment": "RFID-Karte;Onlinezahlungsverfahren",
      "opening_hours": "247",
      "opening_weekdays": "Montag; Dienstag; Mittwoch; Donnerstag; Freitag; Samstag; Sonntag",
      "opening_daytime": "00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59"
    }
  ],
  "meta": {
    "source": "https://data.bundesnetzagentur.de/.../Ladesaeulenregister_BNetzA_2026-02-27.csv",
    "download_time": "2026-03-07T14:59:36",
    "source_last_modified": "Wed, 04 Mar 2026 14:36:31 GMT"
  }
}
```

## Modulstruktur

| Modul | Aufgabe |
|-------|---------|
| `lsl` | Escript-Einstiegspunkt – startet die Anwendungen und orchestriert den Ablauf |
| `lsl_loader` | HTTP-Kommunikation – CSV-URL von der BNetzA-Seite scrapen, Update-Check per HEAD-Request, CSV-Download via Streaming |
| `lsl_converter` | CSV→JSON-Konvertierung – Header-Mapping, Datenzeilen in verschachtelte Maps überführen, Ladepunkte als Array aufbauen |
| `lsl_json` | JSON-Dateiverwaltung – Ausgabepfad, Datei öffnen/schließen, Meta-Daten lesen und schreiben, gzip-Komprimierung |
| `cell_parser` | Continuation-basierter CSV-Parser – Semikolon-getrennt, maskierte Zellen, automatische Encoding-Erkennung (UTF-8/ISO-8859-1), tolerante Behandlung von Anführungszeichen in nicht-quotierten Zellen |


## GitHub Action

Die Workflow-Datei `.github/workflows/update.yml` automatisiert die Aktualisierung.

**Zeitplan:** Jeden Montag um 6:00 UTC (per Cron) sowie bei jedem Push auf `master`.

**Manuell auslösen:** Der Workflow kann jederzeit manuell gestartet werden:

- **Über die GitHub-Weboberfläche:** Im Repository unter *Actions* → *Update Ladesäulenregister*
  → *Run workflow* klicken.
- **Per GitHub API:**

      curl -X POST \
        -H "Authorization: token DEIN_GITHUB_TOKEN" \
        -H "Accept: application/vnd.github.v3+json" \
        https://api.github.com/repos/ratopi/ladesaeule/actions/workflows/update.yml/dispatches \
        -d '{"ref":"master"}'

**Ablauf:**

1. **Checkout** – Repository auschecken
2. **Setup** – Erlang/OTP 27 und rebar3 installieren (`erlef/setup-beam`)
3. **Build** – Escript bauen (`rebar3 as prod escriptize`)
4. **Test** – EUnit-Tests ausführen (`rebar3 eunit`)
5. **Restore** – Die bestehende `ladesaeulen.json.gz` vom `gh-pages`-Branch holen
   (falls vorhanden), damit der Update-Check vergleichen kann
6. **Konvertierung** – `lsl` ausführen; prüft per HEAD-Request ob sich die CSV
   geändert hat und lädt nur bei Bedarf herunter
7. **Deploy** – `public/`-Verzeichnis in den `gh-pages`-Branch pushen
   (`peaceiris/actions-gh-pages`); nur wenn sich tatsächlich etwas geändert hat

**Hinweis:** GitHub deaktiviert Scheduled Workflows automatisch, wenn ein Repository
60 Tage lang keine Aktivität hat (kein Push, Issue oder PR). In dem Fall muss der
Workflow im Actions-Tab manuell wieder aktiviert werden.

## Lizenz

Dieses Projekt steht unter der [MIT-Lizenz](LICENSE).

## Anmerkung

Die Datei wird von der Bundesnetzagentur ganz offensichtlich von Hand mit Excel erzeugt.
Daher kann es vorkommen, daß sich das Format der Datei unvermittelt ändert.
Falls Du dies bemerkst, freue ich mich über einen Pull-Request oder einen Issue in GitHub:
https://github.com/ratopi/ladesaeule/issues
