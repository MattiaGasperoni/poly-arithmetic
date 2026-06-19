#!/usr/bin/env python3
"""
test_poly.py
============
Script di test automatizzato per Main.hs (Haskell) e Main.pl (Prolog).
Entrambi i programmi ricevono lo stesso input e devono produrre lo stesso output.

Uso:
    python test_poly.py                          # esegue i test predefiniti
    python test_poly.py --verbose                # mostra output completo
    python test_poly.py --hs-bin ./Main          # binario Haskell compilato
    python test_poly.py --pl-file ./Main.pl      # file Prolog
    python test_poly.py --add-test               # aggiunge test interattivo
    python test_poly.py --only "Lineari"         # esegue solo test con quel nome
"""

import subprocess
import sys
import argparse
import re
import time
from dataclasses import dataclass
from typing import Optional

# ─────────────────────────────────────────────
# Colori terminale
# ─────────────────────────────────────────────
GREEN  = "\033[92m"
RED    = "\033[91m"
YELLOW = "\033[93m"
CYAN   = "\033[96m"
BOLD   = "\033[1m"
RESET  = "\033[0m"
DIM    = "\033[2m"

def ok(msg):   return f"{GREEN}✓{RESET} {msg}"
def fail(msg): return f"{RED}✗{RESET} {msg}"
def warn(msg): return f"{YELLOW}⚠{RESET} {msg}"
def info(msg): return f"{CYAN}ℹ{RESET} {msg}"

# ─────────────────────────────────────────────
# Struttura test case
# ─────────────────────────────────────────────
@dataclass
class TestCase:
    name: str
    coeff_a: list
    coeff_b: list
    description: str = ""

# ─────────────────────────────────────────────
# Suite di test predefiniti
# ─────────────────────────────────────────────
DEFAULT_TESTS = [
    TestCase("Costanti",                 [3.0],              [5.0],            "Due polinomi costanti"),
    TestCase("Lineari semplici",         [1.0, 2.0],         [3.0, 4.0],       "1 + 2x  e  3 + 4x"),
    TestCase("Quadratico e lineare",     [1.0, 0.0, 1.0],   [1.0, 1.0],       "1 + x²  diviso  1 + x"),
    TestCase("Con zero intermedio",      [0.0, 0.0, 1.0],   [0.0, 1.0],       "x²  diviso  x"),
    TestCase("Coefficienti negativi",    [-1.0, 2.0, -3.0], [1.0, -1.0],      "-1 + 2x - 3x²  e  1 - x"),
    TestCase("MCD non banale",           [1.0, 0.0, -1.0],  [1.0, 1.0],       "x²-1  e  x+1  →  MCD = x+1"),
    TestCase("Polinomio zero come A",    [0.0],              [1.0, 1.0],       "0  diviso  1 + x"),
    TestCase("Grado alto",              [1.0, -2.0, 1.0, 0.0, 1.0], [1.0, 1.0, 1.0], "grado 4 diviso grado 2"),
    TestCase("Coefficienti decimali",    [0.5, 1.5, -2.0],  [0.5, 0.5],       "Coefficienti non interi"),
    TestCase("Stessi polinomi (MCD=se)", [1.0, 2.0, 1.0],   [1.0, 2.0, 1.0], "A = B = 1 + 2x + x²"),
]

# ─────────────────────────────────────────────
# Esecuzione programmi
# ─────────────────────────────────────────────

def fmt_coeffs(coeffs: list) -> str:
    return " ".join(str(c) for c in coeffs)

def run_haskell(binary: str, ca: list, cb: list, timeout: int) -> tuple:
    inp = fmt_coeffs(ca) + "\n" + fmt_coeffs(cb) + "\n"
    try:
        r = subprocess.run([binary], input=inp, capture_output=True, text=True, timeout=timeout)
        if r.returncode != 0:
            return None, f"Exit {r.returncode}: {r.stderr.strip()}"
        return r.stdout, None
    except FileNotFoundError:
        return None, f"Binario non trovato: {binary}"
    except subprocess.TimeoutExpired:
        return None, "Timeout scaduto"
    except Exception as e:
        return None, str(e)

def run_prolog(pl_file: str, ca: list, cb: list, timeout: int) -> tuple:
    inp = fmt_coeffs(ca) + "\n" + fmt_coeffs(cb) + "\n"
    try:
        r = subprocess.run(
            ["swipl", "-g", "main", "-t", "halt", pl_file],
            input=inp, capture_output=True, text=True, timeout=timeout
        )
        if r.returncode != 0:
            return None, f"Exit {r.returncode}: {r.stderr.strip()}"
        return r.stdout, None
    except FileNotFoundError:
        return None, "swipl non trovato nel PATH"
    except subprocess.TimeoutExpired:
        return None, "Timeout scaduto"
    except Exception as e:
        return None, str(e)

# ─────────────────────────────────────────────
# Parsing e confronto output
# ─────────────────────────────────────────────
LABELS = ["A", "B", "Somma", "Prodotto", "Quoziente", "Resto", "MCD"]

def parse_output(raw: str) -> dict:
    result = {}
    for line in raw.splitlines():
        line = line.strip()
        for label in LABELS:
            if line.startswith(f"{label}:"):
                val = line[len(label)+1:].strip()
                val = re.sub(r'\s+', ' ', val)
                result[label] = val
                break
    return result

def compare(hs: dict, pl: dict) -> tuple:
    diffs = []
    for label in LABELS:
        v_hs = hs.get(label, "<MANCANTE>")
        v_pl = pl.get(label, "<MANCANTE>")
        if v_hs != v_pl:
            diffs.append((label, v_hs, v_pl))
    return len(diffs) == 0, diffs

# ─────────────────────────────────────────────
# Stampa risultati
# ─────────────────────────────────────────────

def sep(char="─", w=62):
    print(DIM + char * w + RESET)

def indent(text, prefix="    "):
    return "\n".join(prefix + l for l in text.splitlines())

def print_result(tc: TestCase, hs_out, hs_err, pl_out, pl_err, verbose=False) -> Optional[bool]:
    """
    Stampa il risultato del test.
    Ritorna True=pass, False=fail, None=error.
    """
    print(f"\n{BOLD}▸ {tc.name}{RESET}", end="")
    if tc.description:
        print(f"  {DIM}({tc.description}){RESET}", end="")
    print()
    print(f"  A = [{fmt_coeffs(tc.coeff_a)}]")
    print(f"  B = [{fmt_coeffs(tc.coeff_b)}]")

    if hs_err:
        print(f"  {fail('Haskell')} → {RED}{hs_err}{RESET}")
    if pl_err:
        print(f"  {fail('Prolog')}  → {RED}{pl_err}{RESET}")
    if hs_err or pl_err:
        return None

    hs_p = parse_output(hs_out)
    pl_p = parse_output(pl_out)
    matched, diffs = compare(hs_p, pl_p)

    if matched:
        print(f"  {ok('Output identici')}")
        if verbose:
            sep("·")
            for label in LABELS:
                if label in hs_p:
                    print(f"    {CYAN}{label:10}{RESET} {hs_p[label]}")
    else:
        print(f"  {fail(f'Output diversi  ({len(diffs)} differenze)')}")
        for label, v_hs, v_pl in diffs:
            print(f"    {YELLOW}{label}{RESET}")
            print(f"      Haskell : {v_hs}")
            print(f"      Prolog  : {v_pl}")
        if verbose:
            sep("·")
            print(f"  {DIM}-- Output grezzo Haskell --{RESET}")
            print(indent(hs_out))
            print(f"  {DIM}-- Output grezzo Prolog --{RESET}")
            print(indent(pl_out))

    return matched

def print_summary(passed, failed, errors, elapsed):
    total = passed + failed + errors
    sep("═")
    print(f"{BOLD}RIEPILOGO{RESET}  ({total} test, {elapsed:.2f}s)")
    print(f"  {GREEN}Passati : {passed}{RESET}")
    if failed: print(f"  {RED}Falliti : {failed}{RESET}")
    if errors: print(f"  {YELLOW}Errori  : {errors}{RESET}")
    sep("═")
    if failed == 0 and errors == 0:
        print(f"{GREEN}{BOLD}✓ Tutti i test superati!{RESET}")
    else:
        print(f"{RED}{BOLD}✗ Alcuni test non superati.{RESET}")

# ─────────────────────────────────────────────
# Test interattivo
# ─────────────────────────────────────────────

def interactive_test() -> Optional[TestCase]:
    print(f"\n{CYAN}=== Aggiungi test personalizzato ==={RESET}")
    name = input("Nome: ").strip() or "Personalizzato"
    desc = input("Descrizione (invio per saltare): ").strip()
    try:
        ca = [float(x) for x in input("Coefficienti A (ordine crescente): ").split()]
        cb = [float(x) for x in input("Coefficienti B (ordine crescente): ").split()]
    except ValueError:
        print(fail("Input non valido."))
        return None
    return TestCase(name=name, coeff_a=ca, coeff_b=cb, description=desc)

# ─────────────────────────────────────────────
# Entry point
# ─────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Testa Haskell vs Prolog – Polinomi")
    parser.add_argument("--hs-bin",   default="./Haskell/Main.exe", help="Binario Haskell compilato (default: ./Haskell/Main.exe)")
    parser.add_argument("--pl-file",  default="./Prolog/Main.pl",   help="File Prolog (default: ./Prolog/Main.pl)")
    parser.add_argument("--verbose",  action="store_true", help="Mostra output completo")
    parser.add_argument("--timeout",  type=int, default=10, help="Timeout per esecuzione in secondi (default: 10)")
    parser.add_argument("--add-test", action="store_true", help="Aggiunge un test interattivo")
    parser.add_argument("--only",     default=None,        help="Filtra per nome test (case-insensitive)")
    args = parser.parse_args()

    tests = list(DEFAULT_TESTS)

    if args.add_test:
        extra = interactive_test()
        if extra:
            tests.append(extra)
            print(info(f"Test '{extra.name}' aggiunto alla suite."))

    if args.only:
        key = args.only.lower()
        tests = [t for t in tests if key in t.name.lower()]
        if not tests:
            print(fail(f"Nessun test trovato con nome contenente '{args.only}'"))
            sys.exit(1)

    print(f"\n{BOLD}{'═'*62}{RESET}")
    print(f"{BOLD}   Test automatizzati · Haskell vs Prolog · Polinomi{RESET}")
    print(f"{BOLD}{'═'*62}{RESET}")
    print(info(f"Haskell : {args.hs_bin}"))
    print(info(f"Prolog  : {args.pl_file}"))
    print(info(f"Test    : {len(tests)}"))

    passed = failed = errors = 0
    t_start = time.time()

    for tc in tests:
        t0 = time.time()
        hs_out, hs_err = run_haskell(args.hs_bin, tc.coeff_a, tc.coeff_b, args.timeout)
        pl_out, pl_err = run_prolog(args.pl_file, tc.coeff_a, tc.coeff_b, args.timeout)
        res = print_result(tc, hs_out, hs_err, pl_out, pl_err, args.verbose)
        print(f"  {DIM}⏱  {time.time()-t0:.2f}s{RESET}")
        if res is None:   errors += 1
        elif res:         passed += 1
        else:             failed += 1

    print_summary(passed, failed, errors, time.time() - t_start)
    sys.exit(0 if (failed == 0 and errors == 0) else 1)


if __name__ == "__main__":
    main()