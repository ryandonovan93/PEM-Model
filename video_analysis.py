from tkinter import X
from feat import Detector
from feat.tests.utils import get_test_data_path
import os, glob
import csv
import pandas as pd
import matplotlib as plt


face_model = "retinaface"
landmark_model = "mobilenet"
au_model = "rf"
emotion_model = "resmasknet"

detector = Detector(face_model = face_model, landmark_model = landmark_model, au_model = au_model, emotion_model = emotion_model)

emotions = ["Anger", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Neutral"]
l = ["AN.mp4", "HS.mp4", "LK.mp4", "SL.mp4", "TS.mp4", "WD.mp4"]


d_data = {}

def video_analysis(x):
    d_data = {}
    k = []
    l = ["AN.mp4", "HS.mp4", "LK.mp4", "SL.mp4", "TS.mp4", "WD.mp4"]
    for j in range(1, x+1):   
        #d_data.append(j)
        for a in l: 
            i = (f"P{j}_{a}")  
            k.append(i) 
            print(f"Analysing Participant{j}'s reaction to the video {a}") 
            video_place = "/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/video_expression/" 
            test_videos = os.path.join(video_place, i) 
            video_predictions = detector.detect_video(test_videos, skip_frames= 10) 
            emotions_videos = video_predictions.emotions() 
            plot_emotion = emotions_videos.plot()
            plot_emotion.savefig('f"P{j}_{a}".png')
            mean_of_emotions = emotions_videos.mean()
            for (h, e) in zip(mean_of_emotions, emotions):
                ex = f"{e}_{a}"
                if ex not in d_data:
                    d_data[ex] = [h]
                else:
                    d_data[ex].append(h)
    df_raw = d_data  
    df = pd.DataFrame(df_raw)
    df.to_csv("/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/emotive_facial_expression_analysis.csv")
    return df_raw, df, plot_emotion      

video_analysis(114)




        