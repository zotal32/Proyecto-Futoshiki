import os
import random

def generate_latin_square(size):
    """Genera una matriz válida de N x N sin repeticiones en filas/columnas."""
    base = list(range(1, size + 1))
    matrix = []
    for i in range(size):
        # Desplazamos la fila base para asegurar la propiedad de cuadrado latino
        row = base[i:] + base[:i]
        matrix.append(row)
    
    # Mezclamos filas y columnas para que no sea predecible
    random.shuffle(matrix)
    # Transponer, mezclar y volver a transponer (mezcla de columnas)
    matrix = list(map(list, zip(*matrix)))
    random.shuffle(matrix)
    return matrix

def get_relations(matrix, size, horizontal=True):
    """Genera las matrices de relaciones basadas en una solución."""
    relations = []
    if horizontal:
        # Para hRel: compara celdas adyacentes en la misma fila (size x size-1)
        for row in matrix:
            rel_row = []
            for j in range(size - 1):
                if row[j] < row[j+1]: rel_row.append("RLT")
                elif row[j] > row[j+1]: rel_row.append("RGT")
                else: rel_row.append("Ind")
            relations.append(rel_row)
    else:
        # Para vRel: compara celdas adyacentes en la misma columna (size-1 x size)
        for i in range(size - 1):
            rel_row = []
            for j in range(size):
                if matrix[i][j] < matrix[i+1][j]: rel_row.append("RLT")
                elif matrix[i][j] > matrix[i+1][j]: rel_row.append("RGT")
                else: rel_row.append("Ind")
            relations.append(rel_row)
    return relations

def format_haskell_list(matrix):
    """Convierte listas de Python a formato de lista de Haskell [[...]]"""
    return str(matrix).replace("'", "")

def generate_futoshiki_string(name, size):
    solution = generate_latin_square(size)
    
    # Creamos la matriz vacía (cells)
    empty_cells = [[0 for _ in range(size)] for _ in range(size)]
    
    # Obtenemos relaciones
    h_rel = get_relations(solution, size, horizontal=True)
    v_rel = get_relations(solution, size, horizontal=False)
    
    # Construcción del string final
    futoshiki_str = (
        f'("{name}",Futoshiki {{'
        f'size = {size}, '
        f'cells = {format_haskell_list(empty_cells)}, '
        f'hRel = {format_haskell_list(h_rel)}, '
        f'vRel = {format_haskell_list(v_rel)}'
        f'}},Single ({format_haskell_list(solution)}))'
    )
    return futoshiki_str

def save_futoshikis_to_file(directory, filename, count_per_size):
    # 1. Crear la ruta completa
    ruta_completa = os.path.join(directory, filename)
    
    # 2. Asegurarse de que el directorio existe, si no, crearlo
    if not os.path.exists(directory):
        os.makedirs(directory)
        print(f"Carpeta creada en: {directory}")

    with open(ruta_completa, "w", encoding="utf-8") as f:
        for size, count in count_per_size.items():
            for i in range(1, count + 1):
                name = f"Test-{size}x{size}-{i:02d}"
                line = generate_futoshiki_string(name, size)
                f.write(line + "\n")
    
    print(f"Archivo guardado exitosamente en: {ruta_completa}")

#Aqui se puede configurar los tamaños y cuandos quieres de cada uno, 
#cuanto más grande sea el tamaño y la cantidad más tiempo tardará en resolver el programa los futoshikis
config = {
    3: 3, #Tamaño: Cantidad
    4: 3,
    5: 3,
    6: 3,
    7: 3,
    8: 3,
}

# --- CONFIGURACIÓN DE RUTA ---
# En Windows usa r'C:\Tu\Ruta\Carpeta' (la 'r' es importante para las barras \)
# En Linux/Mac usa '/home/usuario/documentos/futoshiki'

ruta = "xxxxxxx" #Colocar  de la carpeta donde quieres guardar los archivoss
nombreArchivo = input('Introduzca el nombre del archio (nombreArchivo.txt) ')
save_futoshikis_to_file(ruta, nombreArchivo, config)
print(f"Archivo {nombreArchivo} creado con éxito.")