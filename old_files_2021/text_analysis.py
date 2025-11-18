from nrclex import NRCLex as nr
import os
import pandas as pd

"""
Select only the relevant keys from the text_object ("anger", "fear", "disgust", "joy", "sadness", "surprise")
Rename those keys within a loop - append ending of file. 


"""


with open(text_file) as f:
    lines = f.readlines()
    line_analysis = str(lines)
    text_object = nr(line_analysis)
    emotive_responses = text_object.affect_frequencies
    text_object.affect_dict
    print(emotive_responses)
    print(type(emotive_responses))


def text_analysis(x):
    d= {}
    l = ["AN.txt", "HS.txt", "LK.txt", "SL.txt", "TS.txt", "WD.txt"]
    for j in range(1, x+1):   
            #d_data.append(j)
        for a in l:
            try:
                i = (f"P{j}_{a}")  
                print(f"Analysing Participant{j}'s text{a}") 
                text_place = "/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/text/" 
                test_files = os.path.join(text_place, i) 
                with open(test_files) as f:
                    lines = f.readlines()
                    line_analysis = str(lines)
                    text_object = nr(line_analysis)
                    emotive_responses = text_object.affect_frequencies
                    print(emotive_responses)
                    print(type(emotive_responses))
                    emo_keys = emotive_responses.keys()
                    emo_values = emotive_responses.values()
                    for (e, h) in zip(emo_keys, emo_values):
                        ex = f"{e}_{a}"
                        if ex not in d:
                            d[ex] = [h]
                        else:
                            d[ex].append(h)
                            
            except FileNotFoundError:
                print('File not found!')
     
    df_raw = d 
    df = pd.DataFrame.from_dict(d, orient='index')
    df.to_csv("/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/emotive_text_analysis.csv")   
    return df 

text_analysis(166)









