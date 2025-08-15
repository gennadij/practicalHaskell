### Install split lib
`cabal install --lib --package-env . split`

### Beispiel für DKB Auszug/Export CSV
`./updateCsv -f test2.csv -o updated2.csv --to-delete -c 0,2,3,4,6,7,9,10,11 -r 0,1`