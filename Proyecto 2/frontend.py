import tkinter as tk
from tkinter import ttk
from tkinter import filedialog, messagebox


class App:
    def __init__(self, root):
        self.root = root
        self.root.title("Generador de Páginas Web")
        self.root.geometry("1200x700")

        self.create_menu()
        self.create_main_layout()
        self.create_left_side()
        self.create_right_side()
        self.create_status_bar()

        self.root.bind("<Configure>", self.update_table_width)

    def create_menu(self):
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)

        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Archivo", menu=file_menu)
        file_menu.add_command(label="Nuevo", command=self.new_file)
        file_menu.add_command(label="Abrir", command=self.open_file)
        file_menu.add_command(label="Guardar", command=self.save_file)
        file_menu.add_command(label="Guardar como", command=self.save_as)
        file_menu.add_separator()
        file_menu.add_command(label="Salir", command=self.root.quit)

        menubar.add_command(label="Análisis", command=self.run_analysis)
        menubar.add_command(label="Tokens", command=self.show_tokens)

    def create_main_layout(self):
        self.main_paned = tk.PanedWindow(self.root, orient=tk.HORIZONTAL)
        self.main_paned.pack(fill=tk.BOTH, expand=1)

        self.left_frame = tk.Frame(self.main_paned)
        self.right_frame = tk.Frame(self.main_paned)

        self.main_paned.add(self.left_frame, width=500)
        self.main_paned.add(self.right_frame, width=500)

    def create_left_side(self):
        self.left_paned = tk.PanedWindow(self.left_frame, orient=tk.VERTICAL)
        self.left_paned.pack(fill=tk.BOTH, expand=1)

        self.text_area = tk.Text(self.left_paned, wrap="word", undo=True)
        self.left_paned.add(self.text_area, height=400)

        self.error_table = ttk.Treeview(self.left_paned, columns=("Tipo", "Línea", "Columna", "Token", "Descripción"), show="headings", height=5)
        self.error_table.heading("Tipo", text="Tipo")
        self.error_table.heading("Línea", text="Línea")
        self.error_table.heading("Columna", text="Columna")
        self.error_table.heading("Token", text="Token")
        self.error_table.heading("Descripción", text="Descripción")
        self.left_paned.add(self.error_table, height=200)

    def create_right_side(self):
        self.visualization_area = tk.Canvas(self.right_frame, bg="light blue")
        self.visualization_area.pack(fill=tk.BOTH, expand=1)
        self.visualization_area.bind("<Motion>", self.update_mouse_position)

    def create_status_bar(self):
        self.status_bar = ttk.Label(self.root, text="Posición del cursor: 1:0 | Posición del mouse: 0:0")
        self.status_bar.pack(side="bottom", fill="x")
        self.text_area.bind("<KeyRelease>", self.update_cursor_position)

    def update_table_width(self, event=None):
        # Get the width of the left frame
        left_width = self.left_frame.winfo_width()
        
        # Calculate the width for each column (5 columns in total)
        column_width = left_width // 5
        
        # Set the width for each column
        for col in ("Tipo", "Línea", "Columna", "Token", "Descripción"):
            self.error_table.column(col, width=column_width)

    def new_file(self):
        if self.is_text_modified():
            response = messagebox.askyesnocancel("Guardar cambios", "¿Desea guardar los cambios antes de crear un nuevo archivo?")
            if response is None:  # User clicked Cancel
                return
            elif response:  # User clicked Yes
                self.save_file()
                if self.is_text_modified():  # If save was cancelled or failed
                    return

        self.text_area.delete(1.0, tk.END)
        self.current_file = None

    def is_text_modified(self):
        if hasattr(self, 'current_file'):
            with open(self.current_file, 'r') as file:
                original_content = file.read()
            return original_content != self.text_area.get(1.0, tk.END)
        else:
            return self.text_area.get(1.0, tk.END).strip() != ""

    def open_file(self):
        file = filedialog.askopenfile(defaultextension=".txt", filetypes=[("Todos los archivos", "*.*"), ("Archivos de texto", "*.txt")])
        if file:
            self.text_area.delete(1.0, tk.END)
            self.text_area.insert(1.0, file.read())
            file.close()

    def save_file(self):
        if hasattr(self, 'current_file'):
            with open(self.current_file, "w") as file:
                file.write(self.text_area.get(1.0, tk.END))
        else:
            self.save_as()

    def save_as(self):
        file = filedialog.asksaveasfile(defaultextension=".txt", filetypes=[("Todos los archivos", "*.*"), ("Archivos de texto", "*.txt")])
        if file:
            self.current_file = file.name
            text = self.text_area.get(1.0, tk.END)
            file.write(text)
            file.close()

    def run_analysis(self):
        pass

    def show_tokens(self):
        pass

    def update_cursor_position(self, event=None):
        cursor_position = self.text_area.index(tk.INSERT)
        line, column = cursor_position.split(".")
        mouse_x, mouse_y = self.visualization_area.winfo_pointerxy()
        mouse_x -= self.visualization_area.winfo_rootx()
        mouse_y -= self.visualization_area.winfo_rooty()
        self.status_bar.config(text=f"Posición del cursor: {line}:{column} | Posición del mouse: {mouse_x}:{mouse_y}")

    def update_mouse_position(self, event):
        cursor_position = self.text_area.index(tk.INSERT)
        self.status_bar.config(text=f"Posición del cursor: {cursor_position} | Posición del mouse: {event.x}:{event.y}")

if __name__ == "__main__":
    root = tk.Tk()
    editor = App(root)
    root.mainloop()