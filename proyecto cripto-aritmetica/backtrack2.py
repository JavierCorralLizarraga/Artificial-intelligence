from itertools import permutations
def solve_backtrack2(problema):
    palabras = problema.upper().replace(" ", "").split("+")
    res = palabras.pop()
    palabras.append(res)
    letras = set("".join(palabras))
    assert len(letras) <= 10, "Too many letters"
    primeras_letras = set(palabra[0] for palabra in palabras)
    n = len(primeras_letras)
    letras_ordenadas = "".join(primeras_letras) + "".join(letras - primeras_letras)
    chars = tuple(ord(c) for c in letras_ordenadas)
    digitos = tuple(ord(c) for c in "0123456789")
    cero = digitos[0]
    for posible in permutations(digitos, len(letras)):
        if cero not in posible[:n]:
            ecuacion = problema.translate(dict(zip(chars, posible)))
            if eval(ecuacion):
                return ecuacion

problema = "SEND + MORE == MONEY"
print(solve_backtrack2(problema))