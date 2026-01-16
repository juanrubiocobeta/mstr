import matplotlib.pyplot as plt
import math

def generar_primos(n_max):
    es_primo = [True] * (n_max + 1)
    es_primo[0] = es_primo[1] = False
    for i in range(2, int(math.sqrt(n_max)) + 1):
        if es_primo[i]:
            for j in range(i * i, n_max + 1, i):
                es_primo[j] = False
    return [num for num, es in enumerate(es_primo) if es]

CANTIDAD_NUMEROS = 10000
primos = generar_primos(CANTIDAD_NUMEROS)

x_vals = []
y_vals = []
for p in primos:
    x_vals.append(p * math.cos(p))
    y_vals.append(p * math.sin(p))

fig, ax = plt.subplots(figsize=(10, 10), facecolor='black')
ax.set_facecolor('black')

sc = plt.scatter(x_vals, y_vals, s=2, c='white', alpha=0.8, picker=5)

plt.axis('off')
plt.title(f"Espirales de Primos (Pasa el ratón por encima)", color='white')

annot = ax.annotate("", xy=(0,0), xytext=(15,15), textcoords="offset points",
                    bbox=dict(boxstyle="round", fc="black", ec="white", alpha=0.9),
                    color='white',
                    arrowprops=dict(arrowstyle="->", color='white'))
annot.set_visible(False)

def actualizar_anotacion(ind):
    idx = ind["ind"][0]
    
    pos = sc.get_offsets()[idx]
    annot.xy = pos
    
    numero_primo = primos[idx]
    annot.set_text(f"Primo: {numero_primo}")

def hover(event):
    vis = annot.get_visible()
    
    if event.inaxes == ax:
        cont, ind = sc.contains(event)
        if cont:
            actualizar_anotacion(ind)
            annot.set_visible(True)
            fig.canvas.draw_idle()
        else:
            if vis:
                annot.set_visible(False)
                fig.canvas.draw_idle()

fig.canvas.mpl_connect("motion_notify_event", hover)

plt.tight_layout()
plt.show()