from fuerza_bruta import fb1, fb2, fb3, fb4
from backtrack import btk
from cons import cons1, cons2
from evolutivo import ev
from recocido import sa
import time ; from pprint import pprint ; from ortools.constraint_solver import pywrapcp
from ortools.sat.python import cp_model ; import itertools


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
    algos = [cons1, cons2, fb1, fb2, fb3, fb4, bkt, ev, sa]
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
    tiempos = test_ejecucion_tiempos(problemas)
    print(tiempos)


if __name__ == "__main__":
    main()