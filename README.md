# ls - A loader and converter for "Ladesäulen" data

Lädt und konvertiert das offizielle Ladesäulenregister der Bundesnetzagentur in ein besser brauchbares JSON-Format.

## Struktur der Einträge

Die erzeugte Datei enthält ein JSON-Objekt.
Das Attribut "info" enthält Meta-Informationen aus dem Header der CSV-Datei.
Das Attribut "data" enthält hier

## Bauen und ausführen

Das Script kann mit einem

    rebar3 as prod escriptize

gebaut werden.

## Anmerkung

Die Datei wird von der Bundesnetzagentur ganz offensichtlich von Hand mit Excel erzeugt.
Daher kann es vorkommen, daß sich das Format der Datei unvermittelt ändert.
Falls Du dies bemerkst, freue ich mich über einen Pull-Request oder einen Issue in GitHub:
https://github.com/ratopi/ladesaeule/issues
