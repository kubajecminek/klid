# Manuál

Zákon o účetnictví ukládá povinnost vést následující účetní knihy:
 - Deník, v němž se účetní zápisy uspořádají z hlediska časového (chronologicky) a jímž prokazují zaúčtování všech účetních případů v účetním období.
 - Hlavní knihu, v níž se účetní zápisy uspořádají z hlediska věcného (systematicky).
 - Kniha analytických účtů, v nichž se podrobně rozvádějí účetní zápisy hlavní knihy.
 - Knihu podrozvahových účtů.

Pro většinu účetních jednotek je overkill vést účetnictví v softwaru, který má mnoho gigabytů, databázi, spoustu zbytečných funkcí, atp.
Přitom stačí vést deník v CSV soboru a následně si vygenerovat hlavní knihu pro kontrolu souvztažností a účetní závěrku. Aby program v klidu 🙈 fungoval, musí CSV soubor být v následujícím formátu.

```
Datum,Doklad,Částka,Popis,MD,Dal,Poznámka
01.01.2020,ID001,100000,Počáteční stav - 411,701000,411000,,
01.01.2020,ID001,50000,Počáteční stav - 221,221000,701000,,
```
Mrkni se na ukázku správně vedeného deníku [ZDE](example.csv).

Pro CSV soubor je důležité dodržet následující podmínky.
 - Datum musí být ve formátu `DD.MM.YYYY`.
 - V popisu ***nesmí*** být čárky (protože ty běžně oddělují sloupce).
 - Pokud částka obsahuje desetinné místo, pak použijte desetinnou tečku (namísto čárky).

Výhoda plain textových dat je v jednoduché manipulaci dat (například v programu Calc, Emacs, atd).

## Použití
`Klid` je program pro terminál, takže se používá sadou příkazů. Obecně se s programem pracuje následovně (na MS Windows).

```
klid.exe [Příkaz] [SOUBOR]
```

### Příklad č. 1 - Generování deníku
Pokud máte uložený CSV soubor (například `example.csv`) vedle `klidu`, pak účetní deník vygenerujete pomocí příkazu `d` nebo `denik`.

```
klid.exe denik example.csv
```

Výstup:
```
+------------+-------------+--------+------------------------------+--------+--------+-------------+
|   DATUM    |   DOKLAD    | ČÁSTKA |            POPIS             |   MD   |  DAL   |  POZNÁMKA   |
+------------+-------------+--------+------------------------------+--------+--------+-------------+
| 01.01.2020 | ID001       | 100000 | Počáteční stav - 411         | 701000 | 411000 |             |
| 01.01.2020 | ID001       |  50000 | Počáteční stav - 221         | 221000 | 701000 |             |
| 01.01.2020 | ID001       |  50000 | Počáteční stav - 211         | 211000 | 701000 |             |
| 02.01.2020 | FAP001      |  15000 | Nájemné na únor              | 518001 | 321000 |             |
| 02.01.2020 | BV1         |  15000 | Platba nájemného             | 321000 | 221000 |             |
| 15.02.2020 | FAV15022020 | 100000 | VyFa za prodej služeb        | 311000 | 602001 |             |
| 20.02.2020 | BV2         | 100000 | Platba faktury od odběratele | 221000 | 311000 | FAV15022020 |
| 31.12.2020 | ID002       | 100000 | Konečný stav - 411           | 411000 | 702000 |             |
| 31.12.2020 | ID002       |  50000 | Konečný stav - 211           | 702000 | 211000 |             |
| 31.12.2020 | ID002       | 135000 | Konečný stav - 221           | 702000 | 221000 |             |
| 31.12.2020 | ID002       |  15000 | Uzavření účtu - 518001       | 710000 | 518001 |             |
| 31.12.2020 | ID002       | 100000 | Uzavření účtu - 602001       | 602001 | 710000 |             |
| 31.12.2020 | ID002       |  85000 | Zaúčtování zisku             | 710000 | 702000 |             |
+------------+-------------+--------+------------------------------+--------+--------+-------------+
```

Deník bude seřazen chronologicky od nejstarší operace po nejnovější a uspořádán do hezké tabulky.
Deník je možné filtrovat:
  - Podle účtu s argumentem `--ucet <UCET>`.
  - Podle období s argumenty `--od <DD.MM.YYYY>` a `--do <DD.MM.YYYY>`.

Pozn.: Argumenty je možné kombinovat.

### Příklad č. 2 - Generování účetní knihy pro daný účet
Pro hlavní knihu účtů použijte příkaz `k` nebo `kniha`. Tento příkaz vygeneruje účetní knihu pro všechny účty.
Pokud potřebujete vygenerovat hlavní knihu pouze pro jeden konkrétní účet, použijte argument `--ucet <UCET>`

```
klid.exe kniha --ucet 221000 example.csv
```

Výstup:
```
Účet: 221000
+------------+--------+------------------------------+-----------+-----------+------------+-----------+
|   DATUM    | DOKLAD |            POPIS             |  MD [KČ]  | DAL [KČ]  | SALDO [KČ] | PROTIÚČET |
+------------+--------+------------------------------+-----------+-----------+------------+-----------+
| 01.01.2020 | ID001  | Počáteční stav - 221         |  50000.00 |      0.00 |   50000.00 |    701000 |
| 02.01.2020 | BV1    | Platba nájemného             |      0.00 |  15000.00 |  -15000.00 |    321000 |
| 20.02.2020 | BV2    | Platba faktury od odběratele | 100000.00 |      0.00 |  100000.00 |    311000 |
| 31.12.2020 | ID002  | Konečný stav - 221           |      0.00 | 135000.00 | -135000.00 |    702000 |
+------------+--------+------------------------------+-----------+-----------+------------+-----------+
|            |        |             SUMA             | 150000.00 | 150000.00 |    0.00    |           |
+------------+--------+------------------------------+-----------+-----------+------------+-----------+
```

### Příklad č. 3 - Zobrazení všech použitých účtů
Příkazem `u` nebo `ucty` se vám zobrazí všechny unikátní účty, které jste použili na straně Má dáti a nebo Dal.

```
klid.exe ucty example.csv
```

Výstup:
```
+----------------+---------------+
| POŘADOVÉ ČÍSLO | ČÍSLO ÚČTU    |
+----------------+---------------+
|              1 |        211000 |
|              2 |        221000 |
|              3 |        311000 |
|              4 |        321000 |
|              5 |        411000 |
|              6 |        518001 |
|              7 |        602001 |
|              8 |        701000 |
|              9 |        702000 |
|             10 |        710000 |
+----------------+---------------+
```
