# import fuerza_bruta_1
# import fuerza_bruta_2
# import backtrack1
# import backtrack2
# import constraint1
# import constraint2
# import evolutivo
# import recocido
import time

def leer_problemas():
    problemas = []
    soluciones = []
    with open("problemas", "r") as f:
        for ind, n in enumerate(f.readlines()):
            palabras = []
            n = n.replace('\n', '')
            if ind % 2 == 0:
                for i in n.split(" "):
                    if i not in ['+', '=', ]:
                        palabras.append(i)
                problemas.append(tuple(palabras))
            else:
                sol = []
                for i in n.split(","):
                    i = i.replace(' ', '')
                    sol.append(i)
                soluciones.append(tuple(sol))
    sols = []
    for i in soluciones:
        dic = {item.split('=')[0]: int(item.split('=')[1]) for item in i}
        sols.append(dic)
    probs_y_sols = list(zip(problemas, sols))
    return probs_y_sols

def test_ejecucion_tiempos(problemas):
    # probamos velocidades de ejecucion todos sobre el problema de SEND + MORE = MONEY
    algos = [fb1, fb2, cb1, cb2, bkt1, bkt2, ev]
    tiempos = []
    ress=[]
    for algo in algos:
        start = time.time()
        #res = algo(problemas[0][0])
        end = time.time()
        time_elapsed = end - start
        ress.append(res)
        tiempos.append(time_elapsed)
    return zip(algos,tiempos,ress)
def main():
    problemas = leer_problemas()
    a= test_ejecucion_tiempos(problemas)
    print(a)

if __name__ == "__main__":
    main()