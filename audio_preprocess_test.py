#%%

import os
import numpy as np

from pydub import AudioSegment, effects
import librosa
import librosa.display
import noisereduce as nr

import matplotlib.pyplot as plt
from librosa import display   
import IPython.display as ipd


path = r'C:\Users\Ryan.Donovan\Desktop\PhD\PhD Data\Audio\RAVDESS\Actor_02\03-01-01-01-01-01-02.wav'

rawsound = AudioSegment.from_file(path)

x, sr = librosa.load(path, sr = None)

plt.figure(figsize=(12, 1))
librosa.display.waveshow(x, sr)
plt.title('Initial Audio')

rawsound

# 2. Normalize to +5.0 dBFS, Transform audio signals to an array.

normalizedsound = effects.normalize(rawsound, headroom = 5.0) 
normal_x = np.array(normalizedsound.get_array_of_samples(), dtype = 'float32')

plt.figure(figsize=(12,2))
librosa.display.waveshow(normal_x, sr)
plt.title('Normalized audio')

normalizedsound

# 3. Trim silence in the beginning and end.

xt, index = librosa.effects.trim(normal_x, top_db = 30)

plt.figure(figsize=(6,2))
librosa.display.waveshow(xt, sr)
plt.title('Trimmed audio')

ipd.display(ipd.Audio(data = xt, rate=sr))




# %%

# 4. Right-side padding for length equalization.
#    173056 = maximum lengthed audio (the extraction of this value is performed in the "SpeechEmotionRecognion_Model" notebook.)

padded_x = np.pad(xt, (0, 173056-len(xt)), 'constant')

plt.figure(figsize=(12,2))
librosa.display.waveshow(padded_x, sr)
plt.title('Padded audio')

ipd.display(ipd.Audio(data = padded_x, rate=sr))
# %%

# 5. Noise reduction
#    Note: although there is no noise to reduce from RAVDESS nor TESS databases, reduce_noise function by noisereduce library attributes a uniform stamper to the audio files. 
from noisereduce.noisereducev1 import reduce_noise


final_x = reduce_noise(audio_clip=padded_x, 
                          noise_clip=padded_x, 
                          verbose=False)

plt.figure(figsize=(12,2))
librosa.display.waveshow(final_x, sr)
plt.title('Noise-reduced audio')

ipd.display(ipd.Audio(data = final_x, rate=sr))
# %%
# Feature extraction

frame_length = 2048
hop_length = 512

f1 = librosa.feature.rms(final_x, frame_length=frame_length, hop_length=hop_length) # Energy - Root Mean Square (RMS)
print('Energy shape:', f1.shape)
f2 = librosa.feature.zero_crossing_rate(final_x, frame_length=frame_length, hop_length=hop_length) # Zero Crossed Rate (ZCR)
print('ZCR shape:', f2.shape)
f3 = librosa.feature.mfcc(final_x, sr=sr, S=None, n_mfcc=13, hop_length = hop_length) # MFCCs
print('MFCCs shape:', f3.shape)
# %%
