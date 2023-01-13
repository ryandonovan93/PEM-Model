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

# d_headers = ["Participant ID"]
# for a in l:
#     for e in emotions:
#         d_headers.append(f"{e}_{a}")
    
#print(d_headers)

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
            video_predictions = detector.detect_video(test_videos, skip_frames= 2000) 
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

video_analysis(1)





# a = test_my_functions(2)

# op = pd.Series(a)
# li = op.str.len()

# df = pd.DataFrame(a)
# df
            
# print(d_data)
# print(d_headers)

# emotions = ["Anger", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Neutral"]
# l = ["AN.mp4", "HS.mp4", "LK.mp4", "SL.mp4", "TS.mp4", "WD.mp4"]

# d_headers = ["Participant ID"]
# for a in l:
#     for e in emotions:
#         d_headers.append(f"{e}_{a}")
    
# print(d_headers)

# print(d_data)
# print(d_headers)

# d = {d_headers: d_data}

# ts = d_data + d_headers

# d = {'Header': d_headers, 'Data': d_data}
# d

# df = pd.DataFrame(a)
# df
# df.to_csv("/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/emotive_facial_expression_analysis.csv")







#Check what happens if there is no file, e.g. P2_AN does not exist - does it output the results. 

#append column 1 of mean of emotions to header
            #print(mean_of_emotions.iloc[:, 1])
            #emotion_name_list = mean_of_emotions.iloc[:, 1].tolist()
            #header.append(emotion_name_list)
            
            #append column 2 of mean of emotions to data
            #print(mean_of_emotions.iloc[:, 2])
            #emotion_list = mean_of_emotions.iloc[:, 2].tolist()
            #data.append(emotion_list)
            
            
    #write csv file

    # write the header
        #writer.writerow(header)

    # write multiple rows
        #writer.writerows(data)   
        
# with open('/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/EFX_data.csv', 'w', encoding='UTF8', newline='') as f:
#     writer = csv.writer(f)
        
        
        