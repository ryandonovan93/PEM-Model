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
                # emo_keys = [emo_keys1]
                # emo_values = ["NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN"]
                # for (e, h) in zip(emo_keys, emo_values):
                #     ex = f"{e}_{a}"
                #     if ex not in d:
                #         d[ex] = [h]
                #     else:
                #         d[ex].append(h)       
    df_raw = d 
    #df = pd.DataFrame(df_raw)
    df = pd.DataFrame.from_dict(d, orient='index')
    df.to_csv("/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/emotive_text_analysis.csv")   
    return df 

text_analysis(179)







from nrclex import NRCLex as nr
import os
import pandas as pd

"""
Select only the relevant keys from the text_object ("anger", "fear", "disgust", "joy", "sadness", "surprise")
Rename those keys within a loop - append ending of file. 


"""

#### iterate through keys and value
### append each key-value pair to dict 
### Need to figure out what to do if there is no text file

#where i create the file path
    
    
    
    #                 if t[0] not in d:
    #                     d[t[0]]= [i[1]]
    #                 else:
    #                     d[t[0]].append(t[1])
    # print(d)
                    
    #df_raw = d  
    #df = pd.DataFrame(df_raw)
    #df.to_csv("/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/Python/results/emotive_text_expression_analysis.csv")
    #return df_raw, df                
            
            


### Read from file, copy to the text to variable, and insert text into text_object, return the sentences, return the affect_list, affect_dict, and raw_emotion_scores, frequencies, and top_emotions

#                 for i in a:
            #     print (i)
            #     if i[0] not in d:
            #     d[i[0]]= [i[1]]
            #     else:
            #         d[i[0]].append(i[1])
            # print (d)
                #d_data[key].append[value]
                #d_data
                #d_data[ip].append(jp)
                
                # for ip in emotive_responses.keys()
                #     ex = f"{ip}_{a}"
                #     if ex not in emotive_responses.keys:
                #         d_data[ex] = [jp]
                #     else:
                #         d_data[ip].append(jp)