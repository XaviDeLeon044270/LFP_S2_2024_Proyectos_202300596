from tkinter import Tk, Canvas, Entry, Text, Button, PhotoImage, Label, messagebox, filedialog
import subprocess
import tkinter as tk
from pathlib import Path


OUTPUT_PATH = Path(__file__).parent
ASSETS_PATH = OUTPUT_PATH / Path(r"C:\Users\Admin\Desktop\LFP_S2_2024_Proyecto1_202300596\assets\frame0")


def relative_to_assets(path: str) -> Path:
    return ASSETS_PATH / Path(path)

def enviar_datos():
    # Obtener el dato ingresado en la entrada
    dato = entry_1.get("1.0", tk.END)
    
    # Ejecutar el programa Fortran y enviar el dato
    comando = subprocess.run(
        ["gfortran", "-o", "main.exe", "main.f90"],
        check=True  # Asegurarse de que la salida se maneje como texto
    )
    resultado = subprocess.run(
        ["./main.exe"],  # Ejecutable de Fortran
        input=dato,  # la data que se manda a Fortran
        stdout=subprocess.PIPE,  # la data que viene de Fortran   
        text=True  # la salida se maneja como texto
    )

    # Procesar la salida para dividirla en partes
    salida = resultado.stdout.strip()
    partes = salida.split(",")

    if len(partes) == 4:
        # Mostrar los valores en los Labels correspondientes
        label_poblacion.config(text="Población: " + partes[1])
        label_nombre.config(text="Nombre: " + partes[2])
        
        # Mostrar la imagen en un Label
        imagen_path = partes[3].strip()
        imagen = tk.PhotoImage(file=imagen_path)
        label_bandera.config(image=imagen)
        label_bandera.image = imagen  # Guardar referencia para evitar que se borre la imagen

    # Mostrar la salida completa en el área de texto
    entry_1.insert(tk.END, salida + '\n')

window = Tk()

window.geometry("700x500")
window.configure(bg = "#FFFFFF")


canvas = Canvas(
    window,
    bg = "#FFFFFF",
    height = 500,
    width = 700,
    bd = 0,
    highlightthickness = 0,
    relief = "ridge"
)

canvas.place(x = 0, y = 0)
canvas.create_rectangle(
    0.0,
    0.0,
    700.0,
    35.0,
    fill="#6A6CF7",
    outline="")

canvas.create_text(
    166.0,
    0.0,
    anchor="nw",
    text="Proyecto 1 - Analizador de mercado",
    fill="#FFFFFF",
    font=("Inter", 20 * -1)
)

entry_image_1 = PhotoImage(
    file=relative_to_assets("entry_1.png"))
entry_bg_1 = canvas.create_image(
    166.0,
    268.0,
    image=entry_image_1
)
entry_1 = Text(
    bd=0,
    bg="#DDDDDD",
    fg="#000716",
    highlightthickness=0
)
entry_1.place(
    x=41.0,
    y=93.0,
    width=250.0,
    height=348.0
)

canvas.create_rectangle(
    409.0,
    93.0,
    659.0,
    235.0,
    fill="#DDDDDD",
    outline="")

canvas.create_rectangle(
    409.0,
    316.0,
    659.0,
    443.0,
    fill="#DDDDDD",
    outline="")

canvas.create_rectangle(
    41.0,
    68.0,
    166.0,
    93.0,
    fill="#6A6DF8",
    outline="")

canvas.create_rectangle(
    409.0,
    266.0,
    659.0,
    316.0,
    fill="#6A6DF8",
    outline="")

canvas.create_rectangle(
    409.0,
    68.0,
    470.0,
    93.0,
    fill="#6A6DF8",
    outline="")

canvas.create_text(
    41.0,
    68.0,
    anchor="nw",
    text="Editor de código:",
    fill="#FFFFFF",
    font=("Inter", 15 * -1)
)

canvas.create_text(
    409.0,
    68.0,
    anchor="nw",
    text="Gráfico:",
    fill="#FFFFFF",
    font=("Inter", 15 * -1)
)

import os
from tkinter import messagebox

def select_file():
    filename = filedialog.askopenfilename()
    if not filename:
        return
    _, ext = os.path.splitext(filename)
    if ext.lower() not in ['.org', '.ORG']:
        messagebox.showerror("Error", "El archivo seleccionado no es correcto. Por favor, selecciona un archivo .org o .ORG")
    else:
        with open(filename, 'r') as file:
            content = file.read()
        entry_1.delete('1.0', tk.END)
        entry_1.insert('1.0', content)
        print(f"Archivo seleccionado: {filename}")
        
button_image_1 = PhotoImage(
    file=relative_to_assets("button_1.png"))
button_1 = Button(
    image=button_image_1,
    borderwidth=0,
    highlightthickness=0,
    command=select_file,
    relief="flat"
)
button_1.place(
    x=104.0,
    y=458.0,
    width=125.0,
    height=25.0
)

button_image_2 = PhotoImage(
    file=relative_to_assets("button_2.png"))
button_2 = Button(
    image=button_image_2,
    borderwidth=0,
    highlightthickness=0,
    command=enviar_datos,
    relief="flat"
)
button_2.place(
    x=310.0,
    y=244.0,
    width=79.0,
    height=25.0
)

button_image_3 = PhotoImage(
    file=relative_to_assets("button_3.png"))
button_3 = Button(
    image=button_image_3,
    borderwidth=0,
    highlightthickness=0,
    command=quit,
    relief="flat"
)
button_3.place(
    x=78.0,
    y=17.0,
    width=80.0,
    height=18.0
)

def show_info():
    info = "Nombre: Xavi Alexander De León Perdomo\nCarnet: 202300596"
    messagebox.showinfo("Acerca de", info)

button_image_4 = PhotoImage(file=relative_to_assets("button_4.png"))
button_4 = Button(
    image=button_image_4,
    borderwidth=0,
    highlightthickness=0,
    command=show_info,
    relief="flat"
)
button_4.place(
    x=0.0,
    y=17.0,
    width=80.0,
    height=18.0
)

label_nombre = Label(window, text="País seleccionado:", bg="#6A6DF8", fg="#FFFFFF", font=("Inter", 15 * -1))
label_nombre.place(x=409.0, y=266.0)

label_poblacion = Label(window, text="Población:", bg="#6A6DF8", fg="#FFFFFF", font=("Inter", 15 * -1))
label_poblacion.place(x=409.0, y=291.0)

label_bandera = Label(window)
label_bandera.place(x=881, y=464)
window.resizable(False, False)
window.mainloop()