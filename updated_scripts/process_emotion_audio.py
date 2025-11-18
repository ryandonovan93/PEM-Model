# process_emotion_audio.py
# ---------------------------------------------------------
# 7-class audio emotion recognition on CPU with progress bar and flat outputs
# Model: r-f/wav2vec-english-speech-emotion-recognition
# ---------------------------------------------------------

import os
# Block any torchvision imports from transformers (we don't need it for audio)
os.environ["TRANSFORMERS_NO_TORCHVISION"] = "1"

import warnings
warnings.filterwarnings("ignore", category=UserWarning)

import numpy as np
import pandas as pd
import librosa
import soundfile as sf
from tqdm import tqdm
import torch
from transformers import pipeline, AutoConfig

# ==================== CONFIG ====================
AUDIO_ROOT = r"AudioVideos"   # root folder described in the prompt
SR = 16000                    # resample to 16kHz for the model
SEG_SEC = 5.0                 # segment length in seconds (non-overlapping)
SAVE_SEGMENT_CSV = True       # set False to skip per-segment probabilities CSV (faster I/O)
MODEL_NAME = "r-f/wav2vec-english-speech-emotion-recognition"

# Canonical emotions and mapping (happy -> Joy)
EMOTIONS = ["Anger", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Neutral"]
LABEL_MAP = {
    "angry": "Anger",
    "disgust": "Disgust",
    "fear": "Fear",
    "happy": "Joy",        # treat happy/happiness as Joy
    "sad": "Sadness",
    "surprise": "Surprise",
    "neutral": "Neutral",
}

# ==================== SETUP =====================
# Force CPU and use all threads
device = -1  # CPU for transformers pipeline
torch.set_num_threads(max(1, os.cpu_count() or 1))

# Peek the model's label set for sanity
try:
    cfg = AutoConfig.from_pretrained(MODEL_NAME)
    print("Model id2label:", getattr(cfg, "id2label", None))
except Exception as e:
    print("[WARN] Could not read id2label from model config:", e)

clf = pipeline("audio-classification", model=MODEL_NAME, top_k=None, device=device)

# ==================== HELPERS ===================
def parse_video_label(fname_noext: str) -> str:
    """
    Expected filename: [PID]_[LABEL]_TRANSCRIPTION.wav
    Returns the [LABEL] part; if not present, returns the base name.
    """
    parts = fname_noext.split("_")
    if len(parts) > 1:
        return parts[1]
    return parts[0]

def iter_segments(y: np.ndarray, sr: int, seg_sec: float):
    seg_len = int(seg_sec * sr)
    total = len(y)
    n = total // seg_len  # drop remainder
    for i in range(n):
        start = i * seg_len
        yield i, y[start:start + seg_len]

def dominant_emotion_from_scores(scores):
    """
    scores: list of dicts [{'label': 'angry', 'score': 0.9}, ...]
    returns (dominant_label, probs_dict) with EMOTIONS keys
    """
    probs = {emo: 0.0 for emo in EMOTIONS}
    for item in scores:
        raw = item["label"].strip().lower()
        mapped = LABEL_MAP.get(raw)
        if mapped:
            probs[mapped] = float(item["score"])
    dom = max(probs.items(), key=lambda kv: kv[1])[0]
    return dom, probs

def count_total_segments_fast(root=AUDIO_ROOT, seg_sec=SEG_SEC):
    """
    Fast pre-scan using soundfile to avoid decoding whole audio.
    Uses original sample rate to estimate duration, then floors(duration/seg_sec).
    Good enough for progress bar.
    """
    total = 0
    for participant in os.listdir(root):
        td = os.path.join(root, participant, "transcription")
        if not os.path.isdir(td):
            continue
        for fn in os.listdir(td):
            if not fn.lower().endswith(".wav"):
                continue
            fp = os.path.join(td, fn)
            try:
                with sf.SoundFile(fp) as f:
                    dur = f.frames / float(f.samplerate)
                if dur >= seg_sec:
                    total += int(dur // seg_sec)
            except Exception:
                pass
    return total

# ==================== MAIN ======================
def main():
    # Pre-count segments for a global tqdm progress bar
    total_segments = count_total_segments_fast()
    print(f"Total ~{SEG_SEC:.0f}s segments to process (est.): {total_segments}")

    rows = []                # participant summary table rows
    segment_rows = []        # per-segment probabilities (optional)
    all_video_labels = set() # to assemble flat columns per label

    # Create a single global progress bar
    pbar = tqdm(total=total_segments if total_segments > 0 else None,
                desc="Scoring segments", unit="seg")

    # Track global dominant counts for post-run validation
    global_dom_counts = {emo: 0 for emo in EMOTIONS}

    for participant in sorted(os.listdir(AUDIO_ROOT)):
        trans_dir = os.path.join(AUDIO_ROOT, participant, "transcription")
        if not os.path.isdir(trans_dir):
            continue

        per_video_counts = {}  # {video_label: {emo: count}}

        for fname in sorted(os.listdir(trans_dir)):
            if not fname.lower().endswith(".wav"):
                continue

            fpath = os.path.join(trans_dir, fname)
            base = os.path.splitext(fname)[0]
            video_label = parse_video_label(base)

            # Load and resample to SR
            try:
                y, sr = librosa.load(fpath, sr=SR, mono=True)
            except Exception as e:
                print(f"[WARN] Failed to load {fname}: {e}")
                continue

            seg_len = int(SEG_SEC * SR)
            if len(y) < seg_len:
                print(f"[WARN] {fname} shorter than {SEG_SEC}s — skipping.")
                continue

            counts = {emo: 0 for emo in EMOTIONS}

            for seg_idx, seg in iter_segments(y, SR, SEG_SEC):
                try:
                    res = clf({"array": seg.astype(np.float32), "sampling_rate": SR}, top_k=None)
                    dom, probs = dominant_emotion_from_scores(res)
                    counts[dom] += 1
                    global_dom_counts[dom] += 1

                    if SAVE_SEGMENT_CSV:
                        segment_rows.append({
                            "Participant_ID": participant,
                            "Video": video_label,
                            "SegmentIdx": seg_idx,
                            **probs
                        })
                except Exception as e:
                    print(f"[WARN] classify failed on {fname} seg {seg_idx}: {e}")
                finally:
                    if pbar is not None:
                        pbar.update(1)

            per_video_counts[video_label] = counts
            all_video_labels.add(video_label)

        # Summarise this participant
        if per_video_counts:
            n_vid = len(per_video_counts)
            mean_cols = {
                f"{emo}_Total_Mean": sum(per_video_counts[v].get(emo, 0) for v in per_video_counts) / n_vid
                for emo in EMOTIONS
            }

            row = {"Participant_ID": participant}
            row.update(mean_cols)
            for vlbl, cnts in per_video_counts.items():
                for emo in EMOTIONS:
                    row[f"{vlbl}_{emo}"] = cnts.get(emo, 0)
            rows.append(row)

    if pbar is not None:
        pbar.close()

    # Build summary DataFrame with stable columns
    base_cols = ["Participant_ID"] + [f"{emo}_Total_Mean" for emo in EMOTIONS]
    video_cols = []
    for vl in sorted(all_video_labels):
        for emo in EMOTIONS:
            video_cols.append(f"{vl}_{emo}")

    summary_df = pd.DataFrame(rows)
    for c in base_cols + video_cols:
        if c not in summary_df.columns:
            summary_df[c] = 0
    summary_df = summary_df[base_cols + video_cols].fillna(0)
    summary_df.to_csv("audio_emotion_summary.csv", index=False)
    print("Saved audio_emotion_summary.csv")

    if SAVE_SEGMENT_CSV:
        seg_df = pd.DataFrame(segment_rows,
                              columns=["Participant_ID", "Video", "SegmentIdx"] + EMOTIONS)
        seg_df.to_csv("audio_emotion_segments.csv", index=False)
        print("Saved audio_emotion_segments.csv")

    # ==================== POST-RUN VALIDATION ====================
    print("\n=== Post-run validation ===")
    # 1) Global dominant counts (from tally we kept in memory)
    print("Dominant emotion counts across ALL segments:")
    for emo in EMOTIONS:
        print(f"  {emo:9s} : {global_dom_counts.get(emo, 0)}")

    # 2) Column sums from per-segment CSV (if saved)
    if SAVE_SEGMENT_CSV and os.path.exists("audio_emotion_segments.csv"):
        try:
            df_seg = pd.read_csv("audio_emotion_segments.csv")
            print("\nSum of probabilities by emotion across ALL segments (should be > 0 for all emitted labels):")
            for emo in EMOTIONS:
                s = df_seg[emo].sum() if emo in df_seg.columns else 0.0
                print(f"  {emo:9s} : {s:.4f}")
        except Exception as e:
            print("[WARN] Could not read audio_emotion_segments.csv for validation:", e)
    else:
        print("(Per-segment CSV not saved; skip probability-sum check.)")

if __name__ == "__main__":
    main()
