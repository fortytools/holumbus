Howto create a new Hayoo Index
==============================

First, go to Hayoo/HayooIndexer/index:

1.  cache komplett neu von hackage zeihen (dauert etwas):

    make whole-cache

2. neuen index nach json exportieren, kann beliebing oft wiederholt
werden, ohne hackage zu belästigen:

    make whole-json

oder in schrittweise:

    make json-schema
    make json-pkg
    make json-rank
    make json-index

die 4 Ziele schreiben alles in das json/ Unterverzeichnis, vorher
anlegen oder leer machen. die ersten 3 bauen 00-schema.js,
00-packages.js und 00-ranking.js das 4. nimmt immer 100 Packete auf
einmal und schreibt die in eine .js Datei

man hat also anschließend rund 60 Dateien unter json/ die man mit curl
in den server pushen kann


Upload Json files to Hunt
-------------------------

# Start Hunt server
# make insert-json

