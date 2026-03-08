# Replikationspaket - Bayes-Netzwerk Finanzstabilität

Dieses Repository enthält den Analysecode und die Daten zur Reproduktion der Ergebnisse.

## Version

| Element | Version | Datum |
|---------|---------|-------|
| Manuskript | v1.0 | 2026-03-08 |
| Ergänzungsmaterial | v1.0 | 2026-03-08 |
| R-Analysecode | v1.0 | 2026-02-10 |
| Rohdaten | Snapshot | 2026-01-27 |
| Verarbeitete Daten | Pipeline | 2026-02-10 |

## Autoren

- **Thanh-Phong Lam** -- Hochschule für Bankwesen, Ho-Chi-Minh-Stadt (ORCID: 0009-0001-7790-5671)
- **Thi-Linh Ho** (Korrespondenz) -- Ton-Duc-Thang-Universität (ORCID: 0009-0009-8187-8854)

## Verzeichnisstruktur

```
.
├── output/          Fertige PDF-Dokumente
├── code/            R-Skripte (Pipeline)
│   └── init-R.sh   Startskript
└── data/
    ├── raw/         Originaldaten (5 Quellen)
    └── processed/   Aufbereitete Daten
```

## Datenquellen

| Quelle | Variablen |
|--------|-----------|
| Weltbank GFDD | Finanztiefe |
| Weltbank WDI | BIP, Inflation |
| IWF FSI | NPL, Z-Score, CAR |
| IWF COFER | Devisenreserven |
| IADI | Einlagensicherung |
| Harvard Dataverse | Bankdaten Vietnam |

**Hinweis:** IWF-FSI-Rohdaten (86 MB) sind wegen der Dateigröße nicht enthalten. Bitte direkt vom IWF herunterladen und als `data/raw/IMF_FSI_raw_20260127.csv` speichern.

## Reproduktion

```bash
cd code/
chmod +x init-R.sh
./init-R.sh
```

Voraussetzungen: R >= 4.3.0, Pakete siehe `code/00_setup.R`.

## Synchronisierung

```bash
# Vom privaten Quellrepo
rsync -av manu1-2026-02/R/*.R ../TRAM2026-BN-Banking-public/IUKM2026/code/
rsync -av manu3-IUKM2026/LaTeX/*.pdf ../TRAM2026-BN-Banking-public/IUKM2026/output/
sed -i 's/ (AI-assisted)//g' code/*.R
```

## Lizenz

Eingereicht bei IUKM 2026. Bitte korrekt zitieren.
