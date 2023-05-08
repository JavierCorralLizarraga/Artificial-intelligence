import itertools

def fb1():
    nums = [1,2,3,4,5,6,7,8,9,0]
    nums4 = list(itertools.permutations(nums, 4)) # permutaciones de 4 numeros
    nums4 = [int("".join(map(lambda x: str(x), i))) for i in nums4] # convertimos a int
    numsA = list(itertools.combinations(nums4, 2)) # tomamos todas las combinaciones posibles de pares
    res = [(i+j, (i,j)) for i,j in numsA] # sacamos la suma de todos esos pares (aqui se tarda lo mas) (alguna optimizacion o algun otro approach podria funcionar)
    res=list(filter(lambda x: len(str(x[0]))==5, res)) # filtramos solo las sumas que den un numero de 5 digitos
    def uniqueDigits(n): # funcion que detecta si un numero tiene todos los digitos distintos
        s = repr(n[0])
        return len(s) == len(set(s))
    res = list(filter(uniqueDigits, res)) # filtramos por los que tengan un resultado con todos los numeros distintos
    aux = [(i, j[::-1]) for (i, j) in res]
    aux.extend(res)
    # empezamos a filtrar el espacio de probabilidades con restricciones
    aux = [(i, j) for i, j in aux if str(i)[0] == str(j[1])[0]]  # M
    a = [i for i, j in aux]
    b = [i for i, val in enumerate(a) if val == 10652]
    aux = [(i, j) for i, j in aux if str(i)[1] == str(j[1])[1]]  # O
    aux = [(i, j) for i, j in aux if str(i)[2] == str(j[0])[2]]  # N
    aux = [(i, j) for i, j in aux if str(i)[3] == str(j[0])[1]]  # E1
    aux = [(i, j) for i, j in aux if str(i)[3] == str(j[1])[3]]  # E2
    aux = [(i, j) for i, j in aux if str(j[0])[1] == str(j[1])[3]]  # E3
    aux = [(i, j) for i, j in aux if str(j[0])[3] != str(j[1])[2]]  # R != D
    aux = [(i, j) for i, j in aux if str(j[0])[2] != str(j[1])[2]]  # R != N
    aux = [(i, j) for i, j in aux if str(j[0])[0] != str(j[1])[2]]  # R != S
    print(len(aux))
    print(aux, 'listo')
    a = [i for i, j in aux]
    b = [i for i, val in enumerate(a) if val == 10652]
    print(b)
    print(aux)

def fb2():
    a=[(send, more, money) for send in range(1023,9877) for more in range(1023,9877) for money in [send+more]
      if send + more > 9999
        and str(send)[1] == str(more)[3]
        and str(money)[:4] == str(more)[:2] + str(send)[2:0:-1]
        and len("".join(set(str(money) + str(send) + str(more)))) == 8]
    print(a)

def fb3():
    def send_more_money():
        for s in range(1, 10):
            for e in range(10):
                for n in range(10):
                    for d in range(10):
                        for m in range(1, 10):
                            for o in range(10):
                                for r in range(10):
                                    for y in range(10):
                                        if len(set([s, e, n, d, m, o, r, y])) == 8:
                                            send = s * 1000 + e * 100 + n * 10 + d
                                            more = m * 1000 + o * 100 + r * 10 + e
                                            money = m * 10000 + o * 1000 + n * 100 + e * 10 + y
                                            if send + more == money:
                                                return {'S': s, 'E': e, 'N': n, 'D': d,
                                                        'M': m, 'O': o, 'R': r, 'Y': y}
        return None

    solution = send_more_money()
    if solution:
        print('Solution found:')
        for k, v in solution.items():
            print(f'{k} = {v}')
    else:
        print('No solution found')

def fb4():
    problema = "SEND+MORE==MONEY"
    palabras = problema.upper().replace(" ", "").split("+")
    res = palabras[-1]
    letras = set("".join(palabras))
    assert len(letras) <= 10, "Too many letters"
    primeras_letras = set(word[0] for word in palabras)
    n = len(primeras_letras)
    letras_ordenadas = "".join(primeras_letras) + "".join(letras - primeras_letras)
    chars = tuple(ord(c) for c in letras_ordenadas)
    digitos = tuple(range(10))
    cero = digitos[0]
    for posible in itertools.permutations(digitos, len(letras)):
        if cero not in posible[:n]:
            posible = [str(i) for i in posible]
            chars = [chr(i) if i == 43 or i == 61 else i for i in chars]
            ecuacion = problema.translate(dict(zip(chars, posible)))
            if eval(ecuacion):
                return ecuacion
    return None