# lsl – Ladesäulenregister Loader & Converter

Lädt und konvertiert das offizielle Ladesäulenregister der Bundesnetzagentur in ein besser brauchbares JSON-Format.

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

Die Tests nutzen eine gekürzte CSV-Datei unter `test/data/ladesaeulen_sample.csv` und prüfen
die Konvertierung lokal, ohne HTTP-Zugriff.

## Wie die Konvertierung abläuft

### 1. URL ermitteln

Das Tool lädt die E-Mobilitäts-Startseite der Bundesnetzagentur und extrahiert per Regex
den Link zur aktuellen CSV-Datei (der Dateiname enthält ein Datum, das sich bei jeder
Aktualisierung ändert).

### 2. CSV streamen & parsen

Die CSV wird per HTTP-Streaming (`httpc`, async) heruntergeladen.
Die empfangenen Chunks werden direkt dem `cell_parser` übergeben – einem
Continuation-basierten CSV-Parser, der Semikolon-getrennte, optional in Anführungszeichen
maskierte Zellen verarbeitet und einen UTF-8 BOM am Dateianfang entfernt.

### 3. Header und Info-Zeilen verarbeiten

Die CSV beginnt mit mehreren Info-Zeilen (Titel, Hinweise, Aktualisierungsdatum).
Diese werden in das `infos`-Array der JSON-Ausgabe übernommen.

Dann folgen zwei Header-Zeilen:

- **Zeile 1** – Gruppierung: „Allgemeine Informationen", „1. Ladepunkt" bis „6. Ladepunkt".
  Diese Zeile dient als Trigger für den Übergang vom Info-Bereich zum Datenbereich.
- **Zeile 2** – Die eigentlichen Spaltennamen (z.B. `Betreiber`, `Steckertypen1`, `P2 [kW]`).
  Die Zuordnung zu den Ladepunkten 1–6 erfolgt über das Suffix im Spaltennamen.

### 4. Datenzeilen in JSON konvertieren

Jede Datenzeile wird anhand der Spaltennamen in eine verschachtelte Map überführt:

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

### 5. Meta-Informationen anhängen

Am Ende der JSON-Datei wird ein `meta`-Objekt geschrieben mit:

- `source` – die URL der CSV-Datei
- `download_time` – Zeitstempel des Downloads
- `source_last_modified` – der `Last-Modified`-Header des HTTP-Response (Zeitstempel der Datei auf dem Server)

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

## Online-Zugriff auf die JSON-Datei

Die aktuelle `ladesaeulen.json` wird wöchentlich per GitHub Action aktualisiert und ist
über GitHub Pages abrufbar:

**https://ratopi.github.io/ladesaeule/ladesaeulen.json**

Die Action (`.github/workflows/update.yml`) läuft jeden Montag um 6:00 UTC und kann auch
manuell über den „Run workflow"-Button im Actions-Tab ausgelöst werden.

## Lizenz

Dieses Projekt steht unter der [MIT-Lizenz](LICENSE).

## Anmerkung

Die Datei wird von der Bundesnetzagentur ganz offensichtlich von Hand mit Excel erzeugt.
Daher kann es vorkommen, daß sich das Format der Datei unvermittelt ändert.
Falls Du dies bemerkst, freue ich mich über einen Pull-Request oder einen Issue in GitHub:
https://github.com/ratopi/ladesaeule/issues
