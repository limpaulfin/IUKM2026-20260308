# Datenpaket

Statistischer Code und Rohmaterial zur Ergebniswiederholung.

## Stand

| Teil | Nr. | Tag |
|------|-----|-----|
| Aufsatz | 2.0 | 02.07.2026 |
| Anhang | 2.0 | 02.07.2026 |
| Skripte | 2.0 | 02.07.2026 |
| Strukturlernen (W3) | 2.0 | 02.07.2026 |
| Quelldaten | -- | 27.01.2026 |
| Ergebnis | -- | 02.07.2026 |

## Verfasser

- **Thanh-Phong Lam** - BUH, HCMC (0009-0001-7790-5671)
- **Thi-Linh Ho** - TDTU, HCMC (0009-0009-8187-8854)

## Aufbau

```
code/            R-Skripte (inkl. 16_w3_structure_learning.R, unrestringiertes Strukturlernen)
data/raw/        Quelldaten
data/processed/  Aufbereitetes Material
output/w3/        Ergebnis Strukturlernen (Kantenliste + Bericht)
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
