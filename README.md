# hledger-diff

hledger-diff is a simple tool to compare two journal files as used with
[hledger](http://hledger.org/).  It looks at the transactions of a single
account and prints out the transactions which are in one journal file but not
in the other.  This is particularly useful for reconciling existing journals
with bank statements.

## Installation

```
cabal install hledger-diff
```

## Usage example

```
» hledger-diff assets:bank:giro 2014.journal bank.journal
Unmatched transactions in the first journal:

2014/01/01 Opening Balances
    assets:bank:giro              EUR ...
    ...
    equity:opening balances       EUR -...

Unmatched transactions in the second journal:

```
