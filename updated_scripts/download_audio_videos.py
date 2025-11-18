"""
Usage:
    python download_audio_videos.py \
      --input "D:/multimodal_files_audio_2025.xlsx" \
      --output "D:/AudioVideos"
"""

import pandas as pd
import asyncio
import aiohttp
import aiofiles
import os
import logging
import argparse
import backoff
from typing import Dict

# Constants
DEFAULT_OUTPUT_DIR = "D:/AudioVideos"
CHUNK_SIZE = 1024 * 1024  # 1 MB per chunk

# Video column names for this Excel file
VIDEO_COLUMNS = [
    "TS_Audio_Recording",
    "SL_Audio_Recording",
    "LK_Recording",
    "HS_Audio_Recording",
    "WD_Audio_Recording",
    "AN_Audio"
]

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.FileHandler("download_audio.log"), logging.StreamHandler()]
)

@backoff.on_exception(
    backoff.expo,
    (aiohttp.ClientError, asyncio.TimeoutError),
    max_tries=5,
    jitter=None
)
async def download_file(session: aiohttp.ClientSession, url: str, save_path: str):
    async with session.get(url) as response:
        if response.status != 200:
            raise aiohttp.ClientError(f"Status {response.status}")
        async with aiofiles.open(save_path, "wb") as f:
            async for chunk in response.content.iter_chunked(CHUNK_SIZE):
                await f.write(chunk)

async def process_participant(row: pd.Series, output_dir: str, session: aiohttp.ClientSession) -> Dict:
    raw_pid = row["ID"]
    pid = str(raw_pid).zfill(3)  # Pad ID to 3 digits
    participant_folder = os.path.join(output_dir, pid, "audio")
    os.makedirs(participant_folder, exist_ok=True)

    status = {"ID": pid}
    has_any = False

    for col in VIDEO_COLUMNS:
        url = row.get(col, "")
        if pd.isna(url) or not isinstance(url, str) or not url.strip().startswith("http"):
            status[col] = "missing"
            continue

        has_any = True
        clean_col = col.rstrip('_')
        filename = f"P{pid}_{clean_col}.mp4"
        save_path = os.path.join(participant_folder, filename)

        try:
            await download_file(session, url.strip(), save_path)
            logging.info(f"✅ Downloaded {filename}")
            status[col] = "success"
        except Exception as e:
            logging.error(f"❌ Failed to download {filename}: {e}")
            status[col] = "failed"

    if not has_any:
        logging.warning(f"No recordings found for participant {pid}.")
    return status

async def main(input_file: str, output_dir: str):
    df = pd.read_excel(input_file)
    connector = aiohttp.TCPConnector(limit=10)
    timeout = aiohttp.ClientTimeout(total=600)

    async with aiohttp.ClientSession(connector=connector, timeout=timeout) as session:
        tasks = [process_participant(row, output_dir, session) for _, row in df.iterrows()]
        results = await asyncio.gather(*tasks)

    report_df = pd.DataFrame(results)
    report_path = os.path.join(output_dir, "download_report.csv")
    report_df.to_csv(report_path, index=False)
    logging.info(f"📄 Report saved to: {report_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Download audio-video recordings from Excel URLs.")
    parser.add_argument("--input", type=str, default="D:/multimodal_files_audio_2025.xlsx",
                        help="Path to the input Excel file.")
    parser.add_argument("--output", type=str, default=DEFAULT_OUTPUT_DIR,
                        help="Base output directory.")
    args = parser.parse_args()

    os.makedirs(args.output, exist_ok=True)
    asyncio.run(main(args.input, args.output))
