import fuerza_bruta
import backtrack
import constraint
import evolutivo
import recocido
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

def test_ejecucion_tiempos():
    # probamos velocidades de ejecucion todos sobre el problema de SEND + MORE = MONEY
    algos = [backtrack.backtr, recocido.sim_an, constraint.cons1, constraint.cons2, evolutivo.preset_gen_algo, fuerza_bruta.fb1, fuerza_bruta.fb2, fuerza_bruta.fb3, fuerza_bruta.fb4]
    tiempos = []
    ress=[]
    for algo in algos:
        start = time.time()
        print('usando "' + str(algo.__name__)+'"')
        print('la solución es:')
        res = algo()
        end = time.time()
        time_elapsed = end - start
        print('tomo ' + str(time_elapsed) +' segundos')
        ress.append(res)
        tiempos.append(time_elapsed)
        print('-----------------------------')
    return zip([algo.__name__ for algo in algos],ress,tiempos,ress)
def main():
    problemas = leer_problemas()
    #output1 = test_ejecucion_tiempos()
    print(problemas)



if __name__ == "__main__":
    main()


import fuerza_bruta
import backtrack
import constraint
import evolutivo
import recocido
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

def test_ejecucion_tiempos():
    # probamos velocidades de ejecucion todos sobre el problema de SEND + MORE = MONEY
    algos = [backtrack.backtr, recocido.sim_an, constraint.cons1, constraint.cons2, evolutivo.preset_gen_algo, fuerza_bruta.fb1, fuerza_bruta.fb2, fuerza_bruta.fb3, fuerza_bruta.fb4]
    tiempos = []
    ress=[]
    for algo in algos:
        start = time.time()
        print('usando "' + str(algo.__name__)+'"')
        print('la solución es:')
        res = algo()
        end = time.time()
        time_elapsed = end - start
        print('tomo ' + str(time_elapsed) +' segundos')
        ress.append(res)
        tiempos.append(time_elapsed)
        print('-----------------------------')
    return zip([algo.__name__ for algo in algos],ress,tiempos,ress)
def main():
    problemas = leer_problemas()
    #output1 = test_ejecucion_tiempos()
    print(problemas)



if __name__ == "__main__":
    main()
