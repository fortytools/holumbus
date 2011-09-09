#!/bin/bash
curl -s -o /tmp/pdf2text.pdf $1
# sudo apt-get install xpdf
pdftotext /tmp/pdf2text.pdf
cat /tmp/pdf2text.txt
rm /tmp/pdf2text.*
