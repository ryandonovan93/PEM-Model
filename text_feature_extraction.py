from nrclex import NRCLex as nr
import os
import pandas as pd

text_file = "/Users/ryandonovan 1/Desktop/PEM_2021_Data_Analysis/AC_Modalities/Example_Data/Text_Feature_Extraction/master.txt"

with open(text_file) as f:
    lines = f.readlines()
    line_analysis = str(lines)
    text_object = nr(line_analysis)
    emotive_responses = text_object.affect_frequencies
    text_object.affect_dict
    print(emotive_responses)
    print(type(emotive_responses))