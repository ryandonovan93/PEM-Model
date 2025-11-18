import os
import asyncio
import aiohttp
import aiofiles
import pandas as pd
import logging
import argparse
import backoff
import traceback
from aiohttp import ClientError

# Constants
DEFAULT_OUTPUT_DIR = "D:/Videos"
CHUNK_SIZE = 1024 * 1024  # 1MB
VIDEO_COLUMNS = ["AN_Video", "AWD_Video", "HMS_Video", "LK_Video", "SSL_Video", "TS_Video"]
MAX_CONCURRENT_DOWNLOADS = 10  # Adjust based on system/network
OVERWRITE_EXISTING = False

# Configure logging
logging.basicConfig(
    filename='download.log',
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

# Retry decorator with exponential backoff
@backoff.on_exception(backoff.expo, ClientError, max_tries=5)
async def download_video(session, url, save_path):
    async with session.get(url) as response:
        if response.status != 200:
            raise ClientError(f"Bad status code: {response.status}")
        async with aiofiles.open(save_path, 'wb') as f:
            async for chunk in response.content.iter_chunked(CHUNK_SIZE):
                await f.write(chunk)

async def process_participant(session, sem, row, output_dir, statuses, overwrite=False):
    async with sem:
        participant_id = row['ID']
        pid_str = str(participant_id).zfill(3)
        participant_folder = os.path.join(output_dir, f"{pid_str}")
        os.makedirs(participant_folder, exist_ok=True)

        row_status = {"ID": participant_id}

        for col in VIDEO_COLUMNS:
            url = row.get(col)
            if pd.isna(url) or not isinstance(url, str) or not url.startswith("http"):
                row_status[col] = "missing"
                continue

            clean_col = col.strip().replace(" ", "")
            filename = f"P{pid_str}_{clean_col}.mp4"
            save_path = os.path.join(participant_folder, filename)

            if os.path.exists(save_path) and not overwrite:
                logging.warning(f"{filename} already exists. Skipping.")
                row_status[col] = "skipped"
                continue

            try:
                await download_video(session, url, save_path)
                logging.info(f"Downloaded {filename} for participant {participant_id}")
                row_status[col] = "success"
            except Exception as e:
                logging.error(f"Failed to download {filename} for participant {participant_id}: {traceback.format_exc()}")
                row_status[col] = "failed"

        statuses.append(row_status)
        print(f"Processed participant {participant_id}")

async def main(input_file, output_dir, overwrite=False):
    df = pd.read_excel(input_file)

    # Clean relevant column names
    df.columns = [col.strip().replace(" ", "") for col in df.columns]
    statuses = []
    sem = asyncio.Semaphore(MAX_CONCURRENT_DOWNLOADS)

    async with aiohttp.ClientSession() as session:
        tasks = [
            process_participant(session, sem, row, output_dir, statuses, overwrite)
            for _, row in df.iterrows()
        ]
        await asyncio.gather(*tasks)

    report_path = os.path.join(output_dir, "download_report.csv")
    pd.DataFrame(statuses).to_csv(report_path, index=False)
    print(f"\n✅ Download report saved to: {report_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Async video downloader from Firebase links in Excel.")
    parser.add_argument("--input", default="videos.xlsx", help="Path to Excel input file")
    parser.add_argument("--output", default=DEFAULT_OUTPUT_DIR, help="Directory to save videos")
    parser.add_argument("--overwrite", action="store_true", help="Overwrite existing video files")

    args = parser.parse_args()

    asyncio.run(main(args.input, args.output, args.overwrite))
