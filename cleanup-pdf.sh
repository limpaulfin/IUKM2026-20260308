#!/bin/bash
# Xóa PDF khỏi git tracking + commit + push
# Chạy: cd /home/fong/Projects/TRAM2026-BN-Banking-public/IUKM2026 && bash cleanup-pdf.sh

set -e

cd "$(dirname "$0")"

echo "=== Xóa PDF khỏi git tracking ==="
git rm --cached output/*.pdf 2>/dev/null || echo "Không có PDF trong git index"
rm -f output/*.pdf output/*.b

echo "=== Commit + Push ==="
git add .
git commit -m "fix: remove unpublished PDFs from public repo"
git push origin main

echo "=== Verify ==="
echo "Files còn trong output/:"
ls -la output/ 2>/dev/null || echo "(trống)"

echo "=== Done ==="
