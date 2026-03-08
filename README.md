# Datenpaket

Statistischer Code und Rohmaterial zur Ergebniswiederholung.

## Stand

| Teil | Nr. | Tag |
|------|-----|-----|
| Aufsatz | 1.0 | 08.03.2026 |
| Anhang | 1.0 | 08.03.2026 |
| Skripte | 1.0 | 10.02.2026 |
| Quelldaten | -- | 27.01.2026 |
| Ergebnis | -- | 10.02.2026 |

## Verfasser

- **T.-P. Lam** - HUB, HCMC (0009-0001-7790-5671)
- **T.-L. Ho** - TDTU (0009-0009-8187-8854)

## Aufbau

```
output/     Fertige Aufsaetze (PDF)
code/       R-Skripte
data/raw/   Quelldaten
data/processed/  Aufbereitetes Material
```

## Quellen

| Herkunft | Inhalt |
|----------|--------|
| GFDD | Kredittiefe |
| WDI | Makrodaten |
| FSI | Bankstabilitaet |
| COFER | Reserven |
| IADI | Sicherungssysteme |
| Harvard DV | VN-Banken |

FSI-Datei (86 MB) fehlt. Bitte selbst laden: `data/raw/IMF_FSI_raw_20260127.csv`.

## Ausfuehrung

```bash
cd code && chmod +x init-R.sh && ./init-R.sh
```

R >= 4.3.0 erforderlich. Pakete: `00_setup.R`.

## Hinweis

IUKM 2026. Zitat erbeten.
