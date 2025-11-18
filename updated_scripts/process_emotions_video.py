import os
import pandas as pd
from feat import Detector
import logging

# ----------- Logging setup (Point 5) -----------
logging.basicConfig(
    filename='video_emotion_analysis.log',
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
console = logging.StreamHandler()
console.setLevel(logging.INFO)
formatter = logging.Formatter('%(asctime)s [%(levelname)s] %(message)s')
console.setFormatter(formatter)
logging.getLogger('').addHandler(console)
logging.getLogger('feat').setLevel(logging.WARNING)

# ----------- Initialize detector (with AU/landmarks disabled for speed) -----------
detector = Detector()
detector.au_model = None
detector.landmark_model = None

# ----------- Setup -----------
video_labels = ["AN", "AWD", "HMS", "LK", "SSL", "TS"]
emotions = ["Anger", "Disgust", "Fear", "Happiness", "Sadness", "Surprise", "Neutral"]
emotion_cols = [e.lower() for e in emotions]

base_dir = r"D:\Videos"
csv_path = os.path.join(os.getcwd(), "video_emotion_analysis_results.csv")

# ----------- Resume support (Point 1) -----------
if os.path.exists(csv_path):
    processed_ids = set(pd.read_csv(csv_path)["Participant_ID"].astype(str))
else:
    processed_ids = set()

participant_folders = sorted(os.listdir(base_dir))  # full dataset now

for participant_folder in participant_folders:
    participant_path = os.path.join(base_dir, participant_folder)
    if not os.path.isdir(participant_path):
        continue

    participant_id = participant_folder
    if participant_id in processed_ids:
        logging.info(f"Skipping {participant_id} (already processed)")
        continue

    try:
        results = {"Participant_ID": participant_id}
        logging.info(f"Processing participant {participant_id}")

        for label in video_labels:
            video_filename = f"converted_P{participant_id}_{label}_Video.mp4"
            video_path = os.path.join(participant_path, video_filename)

            if not os.path.exists(video_path):
                logging.warning(f"Missing file: {video_path}")
                for emotion in emotions:
                    results[f"{label}_{emotion}"] = float('nan')
                continue

            logging.info(f"  Processing video: {label}")
            try:
                fex = detector.detect(video_path, data_type="video", skip_frames=10)

                if fex.empty or not all(col in fex.columns for col in emotion_cols):
                    raise ValueError("No face detected or emotion columns missing")

                fex_emotions = fex[emotion_cols]
                emotion_means = fex_emotions.mean()
                del fex

                for emotion in emotion_cols:
                    results[f"{label}_{emotion.capitalize()}"] = emotion_means[emotion]

            except Exception as e:
                logging.error(f"    Failed to process video {label} for {participant_id}: {e}")
                for emotion in emotions:
                    results[f"{label}_{emotion}"] = float('nan')

        # Per-participant totals
        for emotion in emotions:
            values = [
                results.get(f"{label}_{emotion}", float('nan')) for label in video_labels
            ]
            values = [v for v in values if pd.notna(v)]
            results[f"{emotion}_Total_Mean"] = sum(values) / len(values) if values else float('nan')

        # Save this participant immediately (Point 1)
        df_row = pd.DataFrame([results])
        if not os.path.exists(csv_path):
            df_row.to_csv(csv_path, index=False, mode='w')
        else:
            df_row.to_csv(csv_path, index=False, mode='a', header=False)

        logging.info(f"✓ Finished and saved results for {participant_id}")

    except Exception as e:
        logging.error(f"[ERROR] Failed to process participant {participant_id}: {e}")
        continue

logging.info("All done.")
