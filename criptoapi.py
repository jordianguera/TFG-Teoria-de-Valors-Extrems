#!/usr/bin/env python3

import requests
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
import time
import argparse
import sys
import os
from typing import Dict, Optional

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
        self.session.headers.update({
            "User-Agent": "CryptoDataAPI/1.0"
        })
    
    def msdatetime(self, ms: int):
        return datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
    
    def datetimems(self, dt: datetime):
        return int(dt.timestamp() * 1000)
    
    def obtenircolumnes(self, symbol: str, inici: datetime, final: datetime, 
                     interval: str = "1m", limit: int = 1000):

        params = {
            "symbol": symbol,
            "interval": interval,
            "startTime": self.datetimems(inici),
            "endTime": self.datetimems(final),
            "limit": limit
        }
        
        response = self.session.get(self.baseURL, params=params, timeout=30)
        response.raise_for_status()
        return response.json()
    
    def dadeshistoriques(self, pair: str, dies: int = 365, 
                               guardarcsv: bool = True, output_dir: str = "."):
        
        symbol = self.simbols[pair]
        final = datetime.now(timezone.utc)
        inici = final - timedelta(days=dies)
        
        velesaprox = dies * 24 * 60
        print(f"Veles aproximades : ~{velesaprox:,}")
        dadescomp = []
        iniciactual = inici
        comptereq = 0
        comptereint = 0
        intentsmax = 5
        
        while iniciactual < final:
            batchfinal = min(iniciactual + timedelta(minutes=1000), final)
            
            try:
                klines = self.obtenircolumnes(symbol, iniciactual, batchfinal)
                
                if not klines:
                    break
                
                dadescomp.extend(klines)
                comptereq += 1
                comptereint = 0
                
                progress = (iniciactual - inici) / (final - inici) * 100
                sys.stdout.write(f"\r Progres: {progress:.1f}% ({len(dadescomp):,} veles)")
                sys.stdout.flush()
                
                ultimtancament = klines[-1][6]
                iniciactual = self.msdatetime(ultimtancament + 1)
                
                time.sleep(0.1) # S'ha de posar una pausa perquè Binance té un límit de solicituds per minut
                
            except requests.exceptions.RequestException as e:
                comptereint += 1
                if comptereint > intentsmax:
                    print(f"\n S'han assolit els intents màxims. Error: {e}")
                    break
                print(f"\n Error: {e}. Reintent {comptereint}/{intentsmax}")
                time.sleep(2 ** comptereint)  # Pausa exponencial per si s'ha saturat l'API
                continue
        
        print(f"\r Progres: 100% ({len(dadescomp):,} veles)")
        
        df = pd.DataFrame(dadescomp, columns=self.columnes)
        
        df["open_time"] = pd.to_datetime(df["open_time"], unit="ms", utc=True)
        df["close_time"] = pd.to_datetime(df["close_time"], unit="ms", utc=True)
        
        df = df[(df["open_time"] >= inici) & (df["open_time"] <= final)]
        
        columnesnum = ["open", "high", "low", "close", "volume", "quote_volume", 
                       "taker_buy_base", "taker_buy_quote"]
        for col in columnesnum:
            df[col] = pd.to_numeric(df[col], errors="coerce")
        
        df["trades"] = df["trades"].astype(int)
        
        df = df.drop(columns=["ignore"])
        df = df.drop_duplicates(subset=["open_time"])
        df = df.sort_values("open_time").reset_index(drop=True)
        
        if guardarcsv:
            os.makedirs(output_dir, exist_ok=True)
            filename = f"{output_dir}/{symbol}_1m_{dies}d.csv"
            df.to_csv(filename, index=False)
            print(f" Guardat: {filename}")
        
        return df
    
    def obtparells(self, dies: int = 365, output_dir: str = "."):
        data = {}
        for pair in self.simbols.keys():
            data[pair] = self.dadeshistoriques(pair, dies, output_dir=output_dir)
        return data


class CryptoDataAPI:
    
    def __init__(self):
        self.binance = BinanceDataAPI()
        
    def obtenirtot(self, days: int = 365, source: str = "binance",
                  output_dir: str = "."):
        return self.binance.obtparells(days, output_dir)
        
def resum(data: Dict[str, pd.DataFrame]):
    for pair, df in data.items():
        print(f"\n{pair}:")
        print(f" Nombre de dades: {len(df):,}")
        print(f" Dates: {df['open_time'].min()} ; {df['open_time'].max()}")
        print(f" Rang de preus: ${df['low'].min():,.2f} ; ${df['high'].max():,.2f}")
        print(f" Ultim tancament: ${df['close'].iloc[-1]:,.2f}")
        print(f" Volum total: {df['volume'].sum():,.2f}")
        
        returns = df['close'].pct_change().dropna()
        print(f" Retorn mitjà: {returns.mean() * 24 * 60 * 100:.2f}%")
        print(f" Volatilitat anualitzada: {returns.std() * np.sqrt(365 * 24 * 60) * 100:.1f}%")


def tempsaprox(days: int, num_pairs: int = 2) -> float:
    velesperpair = days * 24 * 60
    reqperpair = velesperpair // 1000 + 1
    return reqperpair * 0.15 * num_pairs / 60

def main():
    parser = argparse.ArgumentParser(
        description="API per obtenir dades de 1 minut pels parells de criptomonedes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
        Exemples:
        python criptoapi.py                      # Per defecte 7 dies
        python criptoapi.py --days 30            # 30 dies
        python criptoapi.py --days 365           # Tot 1 any
        """
    )
    parser.add_argument("--days", type=int, default=7, 
                        help="Nombre de dies per obtenir (per defecte: 7)")
    parser.add_argument("--output", type=str, default=".", 
                        help="Directori pel CSV")
    args = parser.parse_args()
    source = "binance"
    api = CryptoDataAPI()
    temps = tempsaprox(args.days, 2)
    print(f"\n Temps aproximat: ~{temps:.1f} minut")
    data = api.obtenirtot(days=args.days, output_dir=args.output)
    
    resum(data)
    print("Fet. CSV guardat")
    
    return data

if __name__ == "__main__":
    main()
