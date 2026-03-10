#!/usr/bin/env python3

import requests
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
import time
import argparse
import sys
import os
from typing import Optional

GENESIS_BINANCE = datetime(2017, 1, 1, tzinfo=timezone.utc)
SALT_ESCANEIG   = timedelta(days=30)
MIDA_LOT        = timedelta(minutes=1000)
COLS_OHLC       = ["data", "open", "max", "min", "close"]

PARELLS = {
    "BTC/USD": {"binance": "BTCUSDT",  "kraken": "XBTUSD",  "coingecko": "bitcoin"},
    "ETH/USD": {"binance": "ETHUSDT",  "kraken": "ETHUSD",  "coingecko": "ethereum"},
    "BNB/USD": {"binance": "BNBUSDT",  "kraken": None,      "coingecko": "binancecoin"},
    "XRP/USD": {"binance": "XRPUSDT",  "kraken": "XRPUSD",  "coingecko": "ripple"},
    "SOL/USD": {"binance": "SOLUSDT",  "kraken": "SOLUSD",  "coingecko": "solana"},
}

def _ms_a_dt(ms: int) -> datetime:
    return datetime.fromtimestamp(ms / 1000, tz=timezone.utc)

def _dt_a_ms(dt: datetime) -> int:
    return int(dt.timestamp() * 1000)

def _get_segur(sessio: requests.Session, url: str, params: dict, timeout: int = 30) -> Optional[dict]:
    try:
        resposta = sessio.get(url, params=params, timeout=timeout)
        resposta.raise_for_status()
        return resposta.json()
    except Exception:
        return None


class FontBinance:
    URL  = "https://api.binance.com/api/v3/klines"
    NOM  = "Binance"

    def __init__(self):
        self.sessio = requests.Session()
        self.sessio.headers.update({"User-Agent": "CryptoDataAPI/1.0"})

    def _obtenir_raw(self, simbol: str, inici: datetime, final: datetime, limit: int = 1000):
        params = {
            "symbol":    simbol,
            "interval":  "1m",
            "startTime": _dt_a_ms(inici),
            "endTime":   _dt_a_ms(final),
            "limit":     limit,
        }
        return _get_segur(self.sessio, self.URL, params)

    def _trobar_inici(self, simbol: str, inici: datetime, final: datetime) -> Optional[datetime]:
        cursor = inici
        while cursor < final:
            final_tros = min(cursor + SALT_ESCANEIG, final)
            veles = self._obtenir_raw(simbol, cursor, final_tros, limit=1)
            if veles:
                return _ms_a_dt(veles[0][0])
            cursor = final_tros + timedelta(milliseconds=1)
            time.sleep(0.05)
        return None

    def obtenir(self, parell: str, inici: datetime, final: datetime) -> pd.DataFrame:
        simbol = PARELLS[parell]["binance"]
        if simbol is None:
            return pd.DataFrame()

        inici_real = self._trobar_inici(simbol, inici, final)
        if inici_real is None:
            return pd.DataFrame()

        files = []
        cursor = inici_real
        reintents = 0

        while cursor < final:
            final_lot = min(cursor + MIDA_LOT, final)
            veles = self._obtenir_raw(simbol, cursor, final_lot)

            if veles is None:
                reintents += 1
                if reintents > 5:
                    break
                time.sleep(2 ** reintents)
                continue

            reintents = 0
            if not veles:
                cursor = final_lot + timedelta(milliseconds=1)
                time.sleep(0.05)
                continue

            for v in veles:
                files.append({
                    "data":      _ms_a_dt(v[0]),
                    "open":  float(v[1]),
                    "max":     float(v[2]),
                    "min":     float(v[3]),
                    "close": float(v[4]),
                })

            pct = (cursor - inici_real) / max((final - inici_real), timedelta(seconds=1)) * 100
            sys.stdout.write(f"\r  [{self.NOM}] {pct:.1f}%  ({len(files):,} veles)")
            sys.stdout.flush()

            cursor = _ms_a_dt(veles[-1][6] + 1)
            time.sleep(0.1)

        sys.stdout.write("\r" + " " * 60 + "\r")
        return pd.DataFrame(files, columns=COLS_OHLC) if files else pd.DataFrame()


class FontKraken:
    URL  = "https://api.kraken.com/0/public/OHLC"
    NOM  = "Kraken"
    TROS = timedelta(minutes=720)

    def __init__(self):
        self.sessio = requests.Session()
        self.sessio.headers.update({"User-Agent": "CryptoDataAPI/1.0"})

    def obtenir(self, parell: str, inici: datetime, final: datetime) -> pd.DataFrame:
        simbol = PARELLS[parell]["kraken"]
        if not simbol:
            return pd.DataFrame()

        files = []
        cursor = inici

        while cursor < final:
            des_de = int(cursor.timestamp())
            dades = _get_segur(self.sessio, self.URL, {"pair": simbol, "interval": 1, "since": des_de})

            if dades is None or dades.get("error"):
                break

            resultat = dades.get("result", {})
            ohlc = resultat.get(simbol) or resultat.get(list(resultat.keys())[0], []) if resultat else []

            if not ohlc:
                cursor += self.TROS
                time.sleep(0.5)
                continue

            for v in ohlc:
                ts = datetime.fromtimestamp(v[0], tz=timezone.utc)
                if inici <= ts <= final:
                    files.append({
                        "data":      ts,
                        "open":  float(v[1]),
                        "max":     float(v[2]),
                        "min":     float(v[3]),
                        "close": float(v[4]),
                    })

            pct = (cursor - inici) / max((final - inici), timedelta(seconds=1)) * 100
            sys.stdout.write(f"\r  [{self.NOM}] {pct:.1f}%  ({len(files):,} veles)")
            sys.stdout.flush()

            cursor = datetime.fromtimestamp(ohlc[-1][0], tz=timezone.utc) + timedelta(minutes=1)
            time.sleep(0.5)

        sys.stdout.write("\r" + " " * 60 + "\r")
        return pd.DataFrame(files, columns=COLS_OHLC) if files else pd.DataFrame()


class FontCoinGecko:
    URL = "https://api.coingecko.com/api/v3/coins/{id}/market_chart/range"
    NOM = "CoinGecko"

    def __init__(self):
        self.sessio = requests.Session()
        self.sessio.headers.update({"User-Agent": "CryptoDataAPI/1.0"})

    def obtenir(self, parell: str, inici: datetime, final: datetime) -> pd.DataFrame:
        id_moneda = PARELLS[parell]["coingecko"]
        if not id_moneda:
            return pd.DataFrame()

        url = self.URL.format(id=id_moneda)
        params = {"vs_currency": "usd", "from": int(inici.timestamp()), "to": int(final.timestamp())}
        dades = _get_segur(self.sessio, url, params, timeout=60)

        if not dades or "prices" not in dades:
            return pd.DataFrame()

        files = []
        for ms, preu in dades["prices"]:
            ts = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
            files.append({"data": ts, "open": preu, "max": preu, "min": preu, "close": preu})

        return pd.DataFrame(files, columns=COLS_OHLC) if files else pd.DataFrame()


class APIDades:
    def __init__(self):
        self.fonts = [FontBinance(), FontKraken(), FontCoinGecko()]

    def obtenir_parell(self, parell: str, inici: datetime, final: datetime, directori: str = ".") -> pd.DataFrame:
        print(f"\n[{parell}]  {inici.date()} → {final.date()}")

        df = pd.DataFrame()
        for font in self.fonts:
            print(f"  Intentant {font.NOM}...")
            try:
                df = font.obtenir(parell, inici, final)
            except Exception as e:
                print(f"  {font.NOM} ha tingut l'exception: {e}")
                df = pd.DataFrame()

            if not df.empty:
                print(f"  ✓ {font.NOM}: {len(df):,} veles")
                break
            else:
                print(f"  ✗ {font.NOM}: sense dades")

        if df.empty:
            print(f"  No hi ha dades per {parell}.")
            return df

        df = df.drop_duplicates(subset=["data"]).sort_values("data").reset_index(drop=True)
        df = df[(df["data"] >= inici) & (df["data"] <= final)]

        os.makedirs(directori, exist_ok=True)
        parell_net = parell.replace("/", "")
        etiqueta   = f"{inici.strftime('%Y%m%d')}_{final.strftime('%Y%m%d')}"
        fitxer     = os.path.join(directori, f"{parell_net}_1m_{etiqueta}.csv")
        df[COLS_OHLC].to_csv(fitxer, index=False)
        print(f"  Guardat a: {fitxer}")
        return df

    def obtenir_tot(self, inici: datetime, final: datetime, directori: str = ".") -> dict:
        resultats = {}
        for parell in PARELLS:
            resultats[parell] = self.obtenir_parell(parell, inici, final, directori)
        return resultats


def resum(dades: dict):
    print("\n Resum:")
    for parell, df in dades.items():
        print(f"\n{parell}:")
        if df.empty:
            print("  Sense dades")
            continue
        print(f"  Veles        : {len(df):,}")
        print(f"  Des de       : {df['data'].min()}")
        print(f"  Fins a       : {df['data'].max()}")
        print(f"  Rang de preu : ${df['min'].min():,.2f} – ${df['max'].max():,.2f}")
        print(f"  Últim tanc.  : ${df['close'].iloc[-1]:,.2f}")
        retorns = df['close'].pct_change().dropna()
        print(f"  Vol. An.     : {retorns.std() * np.sqrt(365*24*60) * 100:.1f}%")


def _parsejar_data(s: str) -> datetime:
    return datetime.strptime(s, "%Y-%m-%d").replace(tzinfo=timezone.utc)


def main():
    analitzador = argparse.ArgumentParser(
        description="API per obtenir dades de Criptomonedes (Alterna entre Binance, Kraken i CoinGecko per si algun falla)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemples:
  python criptoapi.py
  python criptoapi.py --inici 2023-01-01
  python criptoapi.py --inici 2023-01-01 --final 2023-12-31
  python criptoapi.py --dies 30
  python criptoapi.py --dies 7 --sortida ./dades
        """
    )
    analitzador.add_argument("--inici",   type=str, default=None)
    analitzador.add_argument("--final",   type=str, default=None)
    analitzador.add_argument("--dies",    type=int, default=None)
    analitzador.add_argument("--sortida", type=str, default=".")
    args = analitzador.parse_args()

    ara = datetime.now(timezone.utc).replace(second=0, microsecond=0)

    if args.dies is not None:
        inici = ara - timedelta(days=args.dies)
        final = ara
    else:
        inici = _parsejar_data(args.inici) if args.inici else GENESIS_BINANCE
        final = _parsejar_data(args.final) if args.final else ara

    api   = APIDades()
    dades = api.obtenir_tot(inici=inici, final=final, directori=args.sortida)
    resum(dades)
    print("\nFet.")
    return dades


if __name__ == "__main__":
    main()