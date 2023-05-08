import itertools

def fb1(tup):
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
    return aux


a=[(send, more, money) for send in range(1023,9877) for more in range(1023,9877) for money in [send+more]
  if send + more > 9999
    and str(send)[1] == str(more)[3]
    and str(money)[:4] == str(more)[:2] + str(send)[2:0:-1]
    and len("".join(set(str(money) + str(send) + str(more)))) == 8]
print(a)