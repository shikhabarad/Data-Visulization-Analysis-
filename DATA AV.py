import matplotlib
matplotlib.use('TkAgg')
#import numpy as np
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
import tkinter as tk
from tkinter import filedialog
import pandas as pd

class mclass:
    def __init__(self,  window):
        self.window = window
        self.box = tk.Entry(window)
        self.button =tk.Button (window, text="select the file", command=self.plot)
        self.box.pack ()
        self.button.pack()

    def plot (self):
        filename = filedialog.askopenfilename()
        print(filename)   
        df= pd.read_csv(filename)
        print(df.head())
        a=df[['Users','New Users']].plot()
        fig = Figure(figsize=(5,5))
        a.set_title ("Estimation Graph", fontsize=16)
        a.set_ylabel("Y", fontsize=14)
        a.set_xlabel("X", fontsize=14)

        canvas = FigureCanvasTkAgg(fig, master=self.window)
        canvas.get_tk_widget().pack()
        canvas.draw()

window=tk.Tk()
window.geometry("600x400")
start= mclass (window)
window.mainloop()