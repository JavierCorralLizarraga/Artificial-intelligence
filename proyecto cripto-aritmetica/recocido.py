import random
import math
def recocido_simulado(estado, funcion_objetivo, funcion_vecino, funcion_probabilidad_aceptacion, plan_temp):
    estado_actual = estado
    energia_actual = funcion_objetivo(estado_actual)
    mejor_estado = estado_actual
    mejor_energia = energia_actual
    temp = plan_temp(0)
    for i in range(1, 1000):
        nuevo_estado = funcion_vecino(estado_actual)
        nueva_energia = funcion_objetivo(nuevo_estado)
        probabilidad_aceptacion = funcion_probabilidad_aceptacion(energia_actual, nueva_energia, temp)
        if probabilidad_aceptacion > random.random():
            estado_actual = nuevo_estado
            energia_actual = nueva_energia
            if energia_actual < mejor_energia:
                mejor_estado = estado_actual
                mejor_energia = energia_actual
        temp = plan_temp(i)
    return mejor_estado


def sim_an():
    letras = ['s', 'e', 'n', 'd', 'm', 'o', 'r', 'y']
    digitos = range(10)
    estado = {letra: random.choice(digitos) for letra in letras}

    def funcion_objetivo(estado):
        s = estado['s']
        e = estado['e']
        n = estado['n']
        d = estado['d']
        m = estado['m']
        o = estado['o']
        r = estado['r']
        y = estado['y']
        send = s * 1000 + e * 100 + n * 10 + d
        more = m * 1000 + o * 100 + r * 10 + e
        money = m * 10000 + o * 1000 + n * 100 + e * 10 + y
        return abs(send + more - money)

    def neighbor_function(state):
        new_state = state.copy()
        letter1, letter2 = random.sample(letras, 2)
        new_state[letter1], new_state[letter2] = new_state[letter2], new_state[letter1]
        return new_state

    def funcion_probabilidad_aceptacion(energia_actual, nueva_energia, temp):
        if nueva_energia < energia_actual:
            return 1.0
        else:
            return math.exp((energia_actual - nueva_energia) / temp)

    def plan_temp(i):
        return max(0.01, 1000.0 / (1 + i))

    solution = recocido_simulado(estado, funcion_objetivo, neighbor_function, funcion_probabilidad_aceptacion,
                                 plan_temp)
    print(solution)
    return solution