import os
import pandas as pd
from nrclex import NRCLex

# Configuration: normalization mode ('emotion' or 'tokens')
normalize_by = 'emotion'

base_dir = "AudioVideos"
labels = ["AN", "HS", "LK", "SL", "TS", "WD"]
emotions = ["anger", "disgust", "fear", "joy", "sadness", "surprise"]

# Prepare storage for each participant: {pid: {label: [list of emotion-score dicts]}}
participant_scores = {}

for pid in os.listdir(base_dir):
    part_path = os.path.join(base_dir, pid)
    trans_path = os.path.join(part_path, "transcription")
    # Skip if transcription folder missing
    if not os.path.isdir(trans_path):
        continue
    
    # Gather valid text files
    files = [f for f in os.listdir(trans_path)
             if f.endswith(".txt") and not f.endswith("_Cleaned.txt")]
    if not files:
        continue
    
    # Initialize participant's data
    participant_scores[pid] = {lbl: [] for lbl in labels}
    
    for fname in files:
        # Expect format: [PID]_[LABEL]_TRANSCRIPTION.txt
        parts = fname.split("_")
        if len(parts) < 3:
            continue  # unexpected name
        label = parts[1]
        if label not in labels:
            continue
        
        path = os.path.join(trans_path, fname)
        with open(path, 'r', encoding='utf-8') as f:
            text = f.read()
        if not text.strip():
            continue
        
        # Compute raw emotion counts with NRCLex
        text_obj = NRCLex(text)
        raw_scores = text_obj.raw_emotion_scores
        # Extract the six relevant emotions (default 0 if missing)
        counts = {emo: raw_scores.get(emo, 0) for emo in emotions}
        total_emotion_words = sum(counts.values())
        total_tokens = len(text_obj.words)
        
        # Normalize scores
        norm_scores = {}
        for emo in emotions:
            num = counts[emo]
            if normalize_by == 'emotion':
                denom = total_emotion_words
            else:  # 'tokens' mode
                denom = total_tokens
            norm_scores[emo] = (num / denom) if (denom and num) else 0.0
        
        # Append this file's scores to the participant's label bucket
        participant_scores[pid][label].append(norm_scores)

# Prepare DataFrame rows
rows = []
for pid, lbl_dict in participant_scores.items():
    # Flatten all scores for this participant
    all_scores = {emo: [] for emo in emotions}
    for lbl in labels:
        for score_dict in lbl_dict[lbl]:
            for emo, val in score_dict.items():
                all_scores[emo].append(val)
    # If no scores found at all, skip
    if not any(all_scores.values()):
        continue
    
    # Compute overall means
    overall_means = {emo: (sum(vals)/len(vals) if vals else 0.0)
                     for emo, vals in all_scores.items()}
    
    row = {
        "Participant": pid,
        **{emo.capitalize(): overall_means[emo] for emo in emotions}
    }
    # Compute per-label means
    for lbl in labels:
        for emo in emotions:
            scores_list = [d[emo] for d in lbl_dict[lbl]]
            col_name = f"{emo.capitalize()}_{lbl}"
            row[col_name] = (sum(scores_list)/len(scores_list)) if scores_list else 0.0
    
    rows.append(row)

# Define column order
cols = ["Participant"] + [emo.capitalize() for emo in emotions]
for lbl in labels:
    for emo in emotions:
        cols.append(f"{emo.capitalize()}_{lbl}")

df = pd.DataFrame(rows, columns=cols)
df.to_csv("text_emotion_summary.csv", index=False)