import customtkinter
import tkinter
import threading
import pandas as pd
import numpy as np
from scipy.io import wavfile
from time import sleep
import sounddevice as sd
import datetime
import sys

customtkinter.set_appearance_mode("dark")
customtkinter.set_default_color_theme("dark-blue")
num_device = [1,1] # Para la MOTU en la Mac Mini
mask_channel = 7 # Canal para el enmascarante esto corresponde al canal 5 de la MOTU
db_roved_max = 10 # maximo roveo en dB (para arriba y para abajo en pasos de 1 dB)
fs = 96000
audio_path = "./Audios/" # de donde lee los audio
data_path = "./Data/" # donde guarda los csv y npz
file_stimulus_prefix = "wnoise_"
file_dB_fijo_parlante = "dB_fijo_parlante.npy"
file_dB_fijo_oido = "dB_fijo_oido.npy" # falta generar
file_mask = "enmascarante_cut.wav"
file_bloques = "bloques.npy"
distancias = [2.0, 2.9, 4.2, 6.0] # SETEAR DISTANCIAS
height_string = ["piso","cabeza"]
condition_string = ["parlante","oido","roved"]

class Experiment_GUI():
    def __init__(self):
        self.trial = 1
        self.bloque = 0
        self.height = 0
        self.condition = 0
        self.ntrials = 0
        self.nsub = 0
        self.pos = 0
        self.last = "0"
        self.newdata = False
        self.terminate_early = False
        self.root = customtkinter.CTk()
        self.root.title("Experimento Elevacion")
        self.root.geometry("600x800+600+0")

        frame1 = customtkinter.CTkFrame(master=self.root)
        frame1.pack(pady=10, padx=60, fill="both", expand=True)

        label1 = customtkinter.CTkLabel(master=frame1, text="Altura : ",font=("Roboto",30))
        label1.grid(row=0,column=0,pady=6,padx=10)
        self.nheight = customtkinter.CTkLabel(master=frame1, text=height_string[self.height],font=("Roboto",30))
        self.nheight.grid(row=0,column=1,padx=30)

        label2 = customtkinter.CTkLabel(master=frame1, text="Condicion : ",font=("Roboto",30))
        label2.grid(row=1,column=0,pady=6,padx=10)
        self.ncondition = customtkinter.CTkLabel(master=frame1, text=condition_string[self.condition],font=("Roboto",30))
        self.ncondition.grid(row=1,column=1,padx=30)  

        label3 = customtkinter.CTkLabel(master=frame1, text="Numero de Trial: ",font=("Roboto",30))
        label3.grid(row=2,column=0,pady=6,padx=10)
        self.ntrial = customtkinter.CTkLabel(master=frame1, text=str(self.trial+1)+" de "+str(self.ntrials),font=("Roboto",30))
        self.ntrial.grid(row=2,column=1,padx=30)  

        frame2 = customtkinter.CTkFrame(master=self.root)
        frame2.pack(pady=10, padx=60, fill="both", expand=True)

        self.position = customtkinter.CTkLabel(master=frame2, text=self.pos,font=("Roboto",280),text_color="green")
        self.position.pack(pady=8,padx=15) 

        frame3 = customtkinter.CTkFrame(master=self.root)
        frame3.pack(pady=10, padx=60, fill="both", expand=True)

        label4 = customtkinter.CTkLabel(master=frame3, text="Distancia: ",font=("Roboto",30))
        label4.grid(row=0,column=0,pady=8,padx=10)
        self.entry = customtkinter.CTkEntry(master=frame3,placeholder_text="Distancia",font=("Roboto",30))
        self.entry.grid(row=0,column=1,pady=8,padx=10) 
        self.entry.bind('<Return>', self.new_entry)

        label5 = customtkinter.CTkLabel(master=frame3, text="Anterior: ",font=("Roboto",24),text_color="gray")
        label5.grid(row=1,column=0,pady=8,padx=10)
        self.answer = customtkinter.CTkLabel(master=frame3, text="",font=("Roboto",24),text_color="gray")
        self.answer.grid(row=1,column=1,pady=8,padx=10)

        frame4 = customtkinter.CTkFrame(master=self.root)
        frame4.pack(pady=10, padx=60, fill="both", expand=True)

        button = customtkinter.CTkButton(master=frame4, text="Interrumpir", command=self.terminate)
        button.pack(pady=8,padx=10) 
    
    def validate_float(self,input):
        try:
            float(input)
            return True
        except ValueError:
            return False

    def new_entry(self,event=None):
        if self.validate_float(self.entry.get()):
            self.newdata = True
            self.last = self.entry.get()

    def update_window(self):
        print("Update")
        self.nheight.configure(text=height_string[self.height])
        self.ncondition.configure(text=condition_string[self.condition])
        self.ntrial.configure(text=str(self.trial+1)+" de "+str(self.ntrials))
        self.position.configure(text=str(self.pos))
        self.answer.configure(text=self.last)
        self.entry.delete(0,'end')
        self.newdata = False

    def terminate(self):
        print("Terminate")
        self.terminate_early = True
        self.newdata = True

class Subject_GUI():
    def __init__(self):
        self.subname = ""
        self.nsub = 1
        self.training = 0
        self.newdata = False
        self.save_and_close = False
        self.root = customtkinter.CTk()
        self.root.title("Experimento Elevacion Sujeto/Bloque")
        self.root.geometry("600x400+0+0")
        self.height = tkinter.IntVar(0) # 0 para piso 1 para cabeza
        self.condition = tkinter.IntVar(0) # fijo_parlante, fijo_oido, roved

        frame1 = customtkinter.CTkFrame(master=self.root)
        frame1.pack(pady=12, padx=10, fill="both", expand=True)

        label1 = customtkinter.CTkLabel(master=frame1, text="Sujeto",font=("Roboto",32))
        label1.grid(row=0,column=0,pady=12,padx=10)
        self.entry_sub = customtkinter.CTkEntry(master=frame1,placeholder_text="Nombre",font=("Roboto",24))
        self.entry_sub.grid(row=0,column=1,padx=2) 

        label2 = customtkinter.CTkLabel(master=frame1, text="ID",font=("Roboto",32))
        label2.grid(row=1,column=0,pady=12,padx=10)
        self.entry_id = customtkinter.CTkEntry(master=frame1,font=("Roboto",24))
        self.entry_id.configure(validate='key', validatecommand= (self.root.register(self.validate_int), '%S'))
        self.entry_id.grid(row=1,column=1,padx=10) 

        frame2 = customtkinter.CTkFrame(master=self.root)
        frame2.pack(pady=12, padx=10, fill="both", expand=True)       

        label3 = customtkinter.CTkLabel(master=frame2, text="Altura:",font=("Roboto",24))
        label3.grid(row=0,column=0,pady=10,padx=10)
        self.rb_height1 = customtkinter.CTkRadioButton(master=frame2, text=height_string[0],font=("Roboto",20),value=0,variable=self.height)
        self.rb_height2 = customtkinter.CTkRadioButton(master=frame2, text=height_string[1],font=("Roboto",20),value=1,variable=self.height)
        self.rb_height1.grid(row=0,column=1,padx=10) 
        self.rb_height2.grid(row=0,column=2,padx=10) 

        label4 = customtkinter.CTkLabel(master=frame2, text="Intensidad:",font=("Roboto",24))
        label4.grid(row=1,column=0,pady=10,padx=10)
        self.rb_condition1 = customtkinter.CTkRadioButton(master=frame2, text=condition_string[0],font=("Roboto",20),value=0,variable=self.condition)
        self.rb_condition2 = customtkinter.CTkRadioButton(master=frame2, text=condition_string[1],font=("Roboto",20),value=1,variable=self.condition)
        self.rb_condition3 = customtkinter.CTkRadioButton(master=frame2, text=condition_string[2],font=("Roboto",20),value=2,variable=self.condition)
        self.rb_condition1.grid(row=1,column=1,padx=10) 
        self.rb_condition2.grid(row=1,column=2,padx=10) 
        self.rb_condition3.grid(row=1,column=3,padx=10) 

        label5 = customtkinter.CTkLabel(master=frame2, text="Entrenamiento:",font=("Roboto",24))
        label5.grid(row=2,column=0,pady=10,padx=10)
        self.checkbox = customtkinter.CTkCheckBox(master=frame2,text="")
        self.checkbox.grid(row=2,column=1,padx=10)
        
        frame3 = customtkinter.CTkFrame(master=self.root)
        frame3.pack(pady=6, padx=10, fill="both", expand=True)

        start = customtkinter.CTkButton(master=frame3, text="Comenzar", fg_color=("black","green"),command=self.starts)
        start.grid(row=0,column=0,pady=12,padx=10) 
        stop = customtkinter.CTkButton(master=frame3, text="Terminar", fg_color=("black","red"),command=self.stops)
        stop.grid(row=0,column=1,pady=12,padx=10) 

    def validate_int(self,input):
        try:
            int(input)
            return True
        except ValueError:
            return False
        
    def starts(self):
        print("Start")
        self.newdata = True
        self.subname = self.entry_sub.get()
        self.nsub = int(self.entry_id.get())
        self.training = self.checkbox.get()

    def stops(self):
        print("Stopping")
        self.newdata = True
        self.save_and_close = True


def load_wavs(apath,file_stimulus_prefix,file_mask):
    parlantes = ['P1','P3','P4','P5']
    # carga los audios
    _, w1 = wavfile.read(apath + file_stimulus_prefix + '1_' + parlantes[0] + '.wav')
    _, w2 = wavfile.read(apath + file_stimulus_prefix + '2_' + parlantes[1] + '.wav')
    _, w3 = wavfile.read(apath + file_stimulus_prefix + '3_' + parlantes[2] + '.wav')
    _, w4 = wavfile.read(apath + file_stimulus_prefix + '4_' + parlantes[3] + '.wav')
    _, mask = wavfile.read(apath + file_mask) 
    w = np.vstack([w1,w2,w3,w4])
    return w,mask

def audio_init(device=None,fs=fs):
    # Inicia el Audio, chequear la configuracion el numero de device
    # Normalmente device = [input, output] donde el numero es el
    # device que devuelve el comando query_devices de sounddevice
    print("Iniciando Audio")
    devices = sd.query_devices() # por si hay dudas de cual es el dispositivo descomentar
    #print(devices)
    if device is not None:
        sd.default.device = device
    devicename = devices[sd.default.device[1]]['name']    
    print("Usando salida de audio" + devicename)
    sd.default.samplerate = fs

def play_stim(stimuli,db_adjust,nchannel):
    # le da play a la señal stimuli con el ajuste en db db?adjust
    # a traves del canal nchannel. Atencion normalmentes es non blocking
    # pero aca usamos la opcion blocking para que no se pueda hacer nada 
    # mientras se reproduce el audio, esto incluye modificar las ventanas 
    # o ingresar datos. SIEMPRE espera a que termine el sonido
    a = np.power(10,db_adjust/20)
    sd.play(stimuli*a,fs,blocking=True,mapping=[nchannel])

def threading_sleep(duration):
    # Rutina para esperar la cantidad de segundo que esta en duration
    t1 = threading.Thread(target=loop_sleep(duration))
    t1.start()

def loop_sleep(duration,step=1.0):
    t = 0.0
    while (t<duration):
        sleep(step)
        t += step

def save_data(respuestas,metadata,column_names):
    # rutina para guardar datos en el array respuestas.
    # Atencion si bine muchos datos de respuestas son enteros como el numero de sujeto 
    # o las condiciones, esta todo en float (porque la respuesta puede ser float)
    # guarda 2 archivos. Un CSV con la informacion que esta en los nombres de las columnas
    # y un npz de backup con mas datos (metadata incluyendo el nombre del sujeto)
    # es un archivo por bloque y condicion y el nombre del archivo es por ejemplo
    # S32_piso_roved_fecha.csv para el sujeto con ID 32 parlantes en el piso y condicion roved
    # si no se hizo hasta el ultimo trial agrega la palabra incompleto al final

    df = pd.DataFrame(data = respuestas, 
                index = np.arange(metadata["ntrials"]), 
                columns = column_names)             
    filename = 'S' + str(metadata["id_anon"]) + '_' + height_string[metadata["altura"]] + '_' + condition_string[metadata["condicion"]] + '_' + metadata["date"]
    if metadata["completo"] is not True:
        filename += "_incompleto"
    df.to_csv(data_path + filename+".csv", index=False)
    np.savez(data_path + filename+".npz", respuestas=respuestas,metadata=metadata)
    print("Data Saved")  

def mainloop(GUI,respuestas,trial_order,mask,w,db_adjust):
    # LOOP Principal que se usa tanto para el bloque de entrenamiento como para el principal
    # Recibe la GUI del experimento el array de respuestas la lista de las posiciones de los
    # parlantes en trial_order y las senales de audio y sus ajustes en dB
    # El lopp se puede cortar con el boton de interrumpir y luego va a grabar hasta donde haya
    # guardado de respuestas pero con un flag en el nombre del archivo

    for n,itrial in enumerate(trial_order):
        GUI.trial = n
        GUI.pos = itrial
        GUI.update_window()
        GUI.root.update()
        threading_sleep(2.0)
        answer_OK = False
        print("Trial")
        while answer_OK == False:
            # Enmascarante
            play_stim(mask,-12,mask_channel)
            # Estimulo
            if GUI.condition == 2:
                #roved
                db_adj = db_adjust[itrial-1]+np.random.randint(-db_roved_max,db_roved_max+1)
            else:
                db_adj = db_adjust[itrial-1]
            
            threading_sleep(1.0)
            play_stim(w[itrial-1,:],db_adj,itrial+2)
            #play_stim(w[itrial-1,:],db_adj,1) #Para probar sin multicannal 
            
            # Espera la respuesta
            while GUI.newdata == False:
                GUI.root.update()
                GUI.entry.focus_set()
                GUI.root.update_idletasks()
            if (float(GUI.last) > 0):
                answer_OK = True 
            else:
                GUI.entry.delete(0,'end')
                GUI.update_window()
                GUI.root.update()      
        if GUI.terminate_early:
            print("Returning")
            return False
        print("Respuesta : " + GUI.last)
        respuestas[n,:6] = [GUI.nsub,GUI.bloque,GUI.height,GUI.condition,itrial,distancias[itrial-1]]
        respuestas[n,-1] = float(GUI.last)
        GUI.newdata == False
    return True    

#########################################
# BLOQUE PRINCIPAL

def main():
    #inicia audio
    audio_init(device=num_device,fs=fs)
    #carga audios
    w,mask = load_wavs(audio_path,file_stimulus_prefix,file_mask)
    # Crea Ventanas
    Subj_Gui = Subject_GUI() # Ventana de Sujeto dond se definen los bloques y las condiciones
    ExpGui = Experiment_GUI() # ventana del experimento con los numeros de trials y las respuestas
    column_names = ['nsub','bloque','altura','condicion','itrial','distancia','respuesta']
    bloques = np.load(file_bloques)
    # Loop master que chequea que no se cierre la ventana principal de sujeto
    while (Subj_Gui.save_and_close == False):
        # Espera nueva data en la ventana del Sujeto
        while (Subj_Gui.newdata == False):
            Subj_Gui.root.update()
            Subj_Gui.root.update_idletasks()
            
        if (Subj_Gui.save_and_close):
            # Cierra el programa si se apreto Terminar en la ventana de sujeto
            print("SAVE AND CLOSE")
            ExpGui.root.destroy()
            Subj_Gui.root.destroy()
            break

        else:   
            # Comienza un nuevo bloque
            # Primero actualiza la ventana de sujeto y copia informacion de esa ventana a la ventana de experimento
            Subj_Gui.root.update() 
            Subj_Gui.newdata = False
            ExpGui.terminate_early = False # Por si se repite un bloque resetea este flag
            ExpGui.nsub = Subj_Gui.nsub 
            ExpGui.height = Subj_Gui.height.get() 
            ExpGui.condition = Subj_Gui.condition.get()
            ExpGui.bloque = ExpGui.height*3+ExpGui.condition
            # Carga el ajuste en dB dependiendo del bloque y condicion
            if Subj_Gui.condition == 0:
                db_adjust = np.load(file_dB_fijo_parlante)
            else:    
                db_adjust = np.load(file_dB_fijo_oido)

            # Chequea si tiene que hacer el pre entrenamiento
            if Subj_Gui.training:
                print("entrenamiento")
                db_adjust = np.load(file_dB_fijo_parlante)
                seq_array = np.array([[1,3,2,4],[4,2,3,1]])
                seq = seq_array[np.random.choice(2,1)[0]]
                ExpGui.ntrials = 4
                respuestas_train = np.zeros((4,7),dtype=float)
                mainloop(ExpGui,respuestas_train,seq,mask,w,db_adjust)
            
            # esto lo saca de bloques.npz
            trial_order = bloques[Subj_Gui.nsub,ExpGui.bloque,:]
            ntrials = len(trial_order)
            ExpGui.ntrials = ntrials
            # Metadata con todos los datos del bloque y condicion
            date = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
            metadata={"sujeto":Subj_Gui.subname,
                "id_anon":Subj_Gui.nsub,
                "bloque":ExpGui.bloque,
                "altura":ExpGui.height,
                "condicion":ExpGui.condition,
                "orden_trials":trial_order,
                "ntrials":ntrials,
                "date":date,
                "completo":False,
            }
            respuestas = np.zeros((ntrials,7),dtype=float)
            # MAIN LOOP
            metadata["completo"]=mainloop(ExpGui,respuestas,trial_order,mask,w,db_adjust)
            # finaliza bloque
            save_data(respuestas,metadata,column_names)          

if __name__ == "__main__":
    main()    