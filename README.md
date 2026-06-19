# PROGETTO PLF – Polinomi in Haskell e Prolog

Questo progetto implementa operazioni su polinomi (addizione, sottrazione, moltiplicazione, divisione con resto e MCD) in due linguaggi: **Haskell** e **Prolog**.  
Lo script `test_poly.py` automatizza il confronto degli output tra i due programmi.

---


## Rappresentazione dei polinomi

Entrambi i programmi usano una lista di coefficienti in **ordine crescente**
per grado (il termine noto è il primo elemento):

```
3 + 2x + x²  →  [3, 2, 1]
x² - 1       →  [-1, 0, 1]
```

---

## Struttura del progetto

```
PROGETTO PLF/
├── Haskell/
│   ├── Main.hs        ← sorgente Haskell
│   ├── Main.exe       ← binario compilato (dopo la compilazione)
│   ├── Main.hi
│   └── Main.o
├── Prolog/
│   └── Main.pl        ← sorgente Prolog
├── test_poly.py       ← script di test automatizzato
└── README.md
```

---

## Requisiti

| Strumento | Versione consigliata | Link |
|-----------|----------------------|------|
| GHC (Glasgow Haskell Compiler) | ≥ 9.x | https://www.haskell.org/ghc/ |
| SWI-Prolog | ≥ 9.x | https://www.swi-prolog.org/ |
| Python | ≥ 3.10 | https://www.python.org/ |

---

## 1. Compilare Haskell

Entrare nella cartella `Haskell/` e compilare con `ghc`:

```bash
cd Haskell
ghc Main.hs -o Main
```

Su **Windows** il binario prodotto sarà `Main.exe`, su Linux/macOS sarà `Main`.

> **Verifica:** dopo la compilazione devono comparire i file `Main.o` e `Main.hi` nella stessa cartella.

---

## 2. Eseguire Haskell manualmente

```bash
# Windows
./Haskell/Main.exe

# Linux / macOS
./Haskell/Main
```

Il programma chiederà interattivamente i coefficienti dei due polinomi A e B
in ordine crescente (termine noto prima), separati da spazio:

```
Inserisci coeff A (ordine crescente): 1 0 -1
Inserisci coeff B (ordine crescente): 1 1
```

Output atteso:

```
A: x^2 - 1
B: x + 1
Somma: x^2 + x
Prodotto: x^3 + x^2 - x - 1
Quoziente: x - 1
Resto: 0
MCD: x + 1
```

---

## 3. Prolog – nessuna compilazione necessaria

SWI-Prolog interpreta direttamente il file `.pl`. Non occorre compilare.

### Esecuzione manuale

```bash
swipl -g main -t halt Prolog/Main.pl
```


---

## 4. Script di test automatizzato – `test_poly.py`

Lo script esegue entrambi i programmi con gli stessi input, confronta gli
output riga per riga e riporta le differenze.

### Esecuzione base (dalla root del progetto)

```bash
python test_poly.py
```

Usa automaticamente `./Haskell/Main.exe` e `./Prolog/Main.pl`.

### Opzioni disponibili

| Opzione | Descrizione | Default |
|---------|-------------|---------|
| `--hs-bin <percorso>` | Percorso al binario Haskell | `./Haskell/Main.exe` |
| `--pl-file <percorso>` | Percorso al file Prolog | `./Prolog/Main.pl` |
| `--verbose` | Mostra l'output completo di entrambi i programmi | disattivo |
| `--timeout <sec>` | Timeout per ogni esecuzione in secondi | `10` |
| `--only <nome>` | Esegue solo i test il cui nome contiene la stringa | tutti |
| `--add-test` | Aggiunge un test personalizzato in modo interattivo | — |

### Esempi

```bash
# Esecuzione standard
python test_poly.py

# Output verboso (utile per debug)
python test_poly.py --verbose

# Eseguire solo i test che contengono "MCD" nel nome
python test_poly.py --only "MCD"

# Percorsi personalizzati (es. su Linux dopo compilazione senza estensione)
python test_poly.py --hs-bin ./Haskell/Main --pl-file ./Prolog/Main.pl

# Aggiungere un test al volo
python test_poly.py --add-test
```

### Interpretare l'output

```
══════════════════════════════════════════════════════════════
   Test automatizzati · Haskell vs Prolog · Polinomi
══════════════════════════════════════════════════════════════

▸ MCD non banale  (x²-1  e  x+1  →  MCD = x+1)
  A = [1.0 0.0 -1.0]
  B = [1.0 1.0]
  ✓ Output identici
  ⏱  0.45s

══════════════════════════════════════════════════════════════
RIEPILOGO  (10 test, 4.23s)
  Passati : 10
✓ Tutti i test superati!
```

In caso di differenze:

```
▸ Esempio fallito
  ✗ Output diversi  (1 differenza)
    MCD
      Haskell : x + 1
      Prolog  : x+1       ← differenza di spaziatura
```

### Codici di uscita

| Codice | Significato |
|--------|-------------|
| `0` | Tutti i test superati |
| `1` | Almeno un test fallito o in errore |


