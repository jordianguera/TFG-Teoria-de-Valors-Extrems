#!/usr/bin/env python3

import requests
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
import time
import argparse
import sys
import os
from typing import Dict

BINANCE0 = datetime(2017, 1, 1, tzinfo=timezone.utc)

Escaneig = timedelta(days=30)
Batch1000 = timedelta(minutes=1000)


class BinanceDataAPI:
    baseURL = "https://api.binance.com/api/v3/klines"

    simbols = {
        "BTC/USD": "BTCUSDT",
        "ETH/USD": "ETHUSDT",
        "BNB/USD": "BNBUSDT",
        "XRP/USD": "XRPUSDT",
        "SOL/USD": "SOLUSDT"
    }

    columnes = [
        "open_time", "open", "high", "low", "close", "volume",
        "close_time", "quote_volume", "trades", "taker_buy_base",
        "taker_buy_quote", "ignore"
    ]

    def __init__(self):
        self.session = requests.Session()
        self.session.headers.update({"User-Agent": "CryptoDataAPI/1.0"})

    def msdatetime(self, ms: int) -> datetime:
        return datetime.fromtimestamp(ms / 1000, tz=timezone.utc)

    def datetimems(self, dt: datetime) -> int:
        return int(dt.timestamp() * 1000)

    def buscaklines(self, simbol: str, inici: datetime, final: datetime, limit: int = 1000):
        params = {
            "symbol": simbol,
            "interval": "1m",
            "startTime": self.datetimems(inici),
            "endTime": self.datetimems(final),
            "limit": limit
        }
        response = self.session.get(self.baseURL, params=params, timeout=30)
        response.raise_for_status()
        return response.json()

    def inicicrypto(self, simbol: str, inici: datetime, final: datetime) -> datetime:
        cursor = inici
        while cursor < final:
            finalchunk = min(cursor + Escaneig, final)
            try:
                klines = self.buscaklines(simbol, cursor, finalchunk, limit=1)
                if klines:
                    actual_start = self.msdatetime(klines[0][0])
                    print(f"Primera dada trobada: {actual_start.date()}")
                    return actual_start
            except requests.exceptions.RequestException:
                pass
            cursor = finalchunk + timedelta(milliseconds=1)
            time.sleep(0.05)
        return None

    def dadeshistoriques(self, pair: str, inici: datetime, final: datetime,
                         guardarcsv: bool = True, output_dir: str = "."):

        simbol = self.simbols[pair]
        dies = (final - inici).days
        velesaprox = dies * 24 * 60
        print(f"\n[{pair}] Veles aproximades: ~{velesaprox:,} ({inici.date()} -> {final.date()})")
        print(f"Cercant inici real del parell...")

        actual_start = self.inicicrypto(simbol, inici, final)
        if actual_start is None:
            print(f"  [{pair}] Cap dada disponible en tot el rang.")
            return pd.DataFrame()

        dadescomp = []
        iniciactual = actual_start
        comptereint = 0
        intentsmax = 5

        while iniciactual < final:
            batchfinal = min(iniciactual + Batch1000, final)

            try:
                klines = self.buscaklines(simbol, iniciactual, batchfinal)
                comptereint = 0

                if not klines:
                    iniciactual = batchfinal + timedelta(milliseconds=1)
                    time.sleep(0.05)
                    continue

                dadescomp.extend(klines)

                progress = (iniciactual - actual_start) / (final - actual_start) * 100
                sys.stdout.write(f"\r  Progres: {progress:.1f}% ({len(dadescomp):,} veles)")
                sys.stdout.flush()

                ultimtancament = klines[-1][6]
                iniciactual = self.msdatetime(ultimtancament + 1)
                time.sleep(0.1)

            except requests.exceptions.RequestException as e:
                comptereint += 1
                if comptereint > intentsmax:
                    print(f"\n  Intents maxims assolits. Error: {e}")
                    break
                print(f"\n  Error: {e}. Reintent {comptereint}/{intentsmax}")
                time.sleep(2 ** comptereint)
                continue

        print(f"\r  Progres: 100% ({len(dadescomp):,} veles)  ")

        if not dadescomp:
            print(f"  [{pair}] Cap dada obtinguda.")
            return pd.DataFrame()

        df = pd.DataFrame(dadescomp, columns=self.columnes)

        df = df.rename(columns={"open_time": "date"})
        df["date"] = pd.to_datetime(df["date"], unit="ms", utc=True)
        df["close_time"] = pd.to_datetime(df["close_time"], unit="ms", utc=True)

        df = df[(df["date"] >= inici) & (df["date"] <= final)]

        for col in ["open", "high", "low", "close", "volume", "quote_volume",
                    "taker_buy_base", "taker_buy_quote"]:
            df[col] = pd.to_numeric(df[col], errors="coerce")

        df["trades"] = df["trades"].astype(int)
        df = df.drop(columns=["ignore"])
        df = df.drop_duplicates(subset=["date"])
        df = df.sort_values("date").reset_index(drop=True)

        if guardarcsv:
            os.makedirs(output_dir, exist_ok=True)
            date_tag = f"{inici.strftime('%Y%m%d')}_{final.strftime('%Y%m%d')}"
            filename = f"{output_dir}/{simbol}_1m_{date_tag}.csv"
            df.to_csv(filename, index=False)
            print(f"  Guardat: {filename}")

        return df

    def obtparells(self, inici: datetime, final: datetime, output_dir: str = "."):
        data = {}
        for pair in self.simbols.keys():
            data[pair] = self.dadeshistoriques(pair, inici, final, output_dir=output_dir)
        return data


class CryptoDataAPI:
    def __init__(self):
        self.binance = BinanceDataAPI()

    def obtenirtot(self, inici: datetime, final: datetime, output_dir: str = "."):
        return self.binance.obtparells(inici, final, output_dir)

def resum(data: Dict[str, pd.DataFrame]):
    for pair, df in data.items():
        print(f"\n{pair}:")
        if df.empty:
            print("  Sense dades.")
            continue
        print(f"  Nombre de dades: {len(df):,}")
        print(f"  Dates: {df['date'].min()} ; {df['date'].max()}")
        print(f"  Rang de preus: ${df['low'].min():,.2f} ; ${df['high'].max():,.2f}")
        print(f"  Ultim tancament: ${df['close'].iloc[-1]:,.2f}")
        print(f"  Volum total: {df['volume'].sum():,.2f}")
        returns = df['close'].pct_change().dropna()
        print(f"  Retorn mitja (per minut): {returns.mean() * 100:.6f}%")
        print(f"  Volatilitat anualitzada: {returns.std() * np.sqrt(365 * 24 * 60) * 100:.1f}%")


def merge_by_date(data: Dict[str, pd.DataFrame], col: str = "close") -> pd.DataFrame:
    frames = {
        pair: df.set_index("date")[col].rename(pair)
        for pair, df in data.items()
        if not df.empty
    }
    if not frames:
        return pd.DataFrame()
    merged = pd.concat(frames.values(), axis=1, join="outer")
    merged.index.name = "date"
    return merged.sort_index()


def tempsaprox(inici: datetime, final: datetime, num_pairs: int = 5) -> float:
    dies = (final - inici).days
    velesperpair = dies * 24 * 60
    reqperpair = velesperpair // 1000 + 1
    return reqperpair * 0.15 * num_pairs / 60


def parse_date(s: str) -> datetime:
    return datetime.strptime(s, "%Y-%m-%d").replace(tzinfo=timezone.utc)


def main():
    parser = argparse.ArgumentParser(
        description="API per obtenir dades de 1 minut pels parells de criptomonedes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemples:
  python criptoapi.py                               # Tot l'historial (des de 2017-01-01)
  python criptoapi.py --start 2021-01-01            # Des d'una data fins avui
  python criptoapi.py --start 2023-01-01 --end 2023-12-31
  python criptoapi.py --days 30                     # Ultims 30 dies
        """
    )
    parser.add_argument("--start", type=str, default=None,
                        help="Data d'inici (YYYY-MM-DD). Per defecte: 2017-01-01")
    parser.add_argument("--end", type=str, default=None,
                        help="Data de fi (YYYY-MM-DD). Per defecte: avui")
    parser.add_argument("--days", type=int, default=None,
                        help="Alternativa: ultims N dies (sobreescriu --start/--end)")
    parser.add_argument("--output", type=str, default=".",
                        help="Directori per als CSVs (per defecte: directori actual)")
    args = parser.parse_args()

    final = datetime.now(timezone.utc).replace(second=0, microsecond=0)

    if args.days is not None:
        inici = final - timedelta(days=args.days)
    else:
        inici = parse_date(args.start) if args.start else BINANCE0
        final = parse_date(args.end) if args.end else final

    api = CryptoDataAPI()
    temps = tempsaprox(inici, final, num_pairs=5)
    print(f"\nRang: {inici.date()} -> {final.date()}")
    print(f"Temps aproximat: ~{temps:.0f} minuts")

    data = api.obtenirtot(inici=inici, final=final, output_dir=args.output)

    print("\n--- Resum ---")
    resum(data)

    merged = merge_by_date(data, col="close")
    if not merged.empty:
        date_tag = f"{inici.strftime('%Y%m%d')}_{final.strftime('%Y%m%d')}"
        merged_path = f"{args.output}/merged_close_{date_tag}.csv"
        os.makedirs(args.output, exist_ok=True)
        merged.to_csv(merged_path)
        print(f"\nMerged close prices guardat: {merged_path}")

    print("\nFet.")
    return data


if __name__ == "__main__":
    main()