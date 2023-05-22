from pprint import pprint

def backtr():
    str1 = 'send'
    str2 = 'more'
    str3 = 'money'

    letras = list(set(str1 + str2 + str3))
    letras.sort()

    def suma(a1,a2):  # takes two lists of digits representing two numbers and returns a list of digits representing their su
        l = len(a1)
        res = [None] * l
        acarreo = 0
        for i in reversed(range(l)):
            if a1[i] is None or a2[i] is None:
                acarreo = 0
                continue
            res[i] = a1[i] + a2[i] + acarreo
            if res[i] >= 10:
                res[i] -= 10
                acarreo = 1
            else:
                acarreo = 0
        if a1[0] is None or a2[0] is None:
            return [None] + res
        return [acarreo] + res

    def reemplaza(string,
                  mapeo):  # unction takes a string and a mapping of characters to digits and returns a list of digits where each character in the string is replaced by its corresponding digit in the mapping
        return [mapeo.get(string[i], None) for i in range(len(string))]

    def matchea(res1, res2):  # checa si dos listas de digitos son iguales
        for i, v1 in enumerate(res1):
            v2 = res2[i]
            if v2 != v1 \
                    and v2 is not None \
                    and v1 is not None \
                    and not (i + 1 < len(res2) \
                             and res2[i + 1] is None \
                             and v1 == v2 + 1):
                return False
        return True

    def value_count(mapping, c):
        m = dict(mapping)
        count = 0
        for i in valores_posibles(mapping):
            m[c] = i
            if is_valid(m):
                count += 1
        return count

    def ordenamiento_mas_restringido(mapping):
        min_count = 10000
        result = None
        for c in letras:
            if c not in mapping:
                count = value_count(mapping, c)
                if count < min_count:
                    min_count = count
                    result = c
        return result

    def valores_posibles(mapeo):
        vals = [mapeo[key] for key in mapeo]
        for i in range(10):
            if i not in vals:
                yield i

    def ordenamiento_menos_restringido(mapeo, c):
        def calidad(i):
            m = dict(mapeo)
            m[c] = i
            return value_count(m, ordenamiento_mas_restringido(m))

        ordering = list(valores_posibles(mapeo))
        ordering.sort(key=calidad)
        return reversed(ordering)

    def is_valid(mapeo):
        if mapeo.get(str3[0], None) == 0:
            return False
        summ = suma(reemplaza(str1, mapeo), reemplaza(str2, mapeo))
        return matchea(reemplaza(str3, mapeo), summ)

    def solve(mapeo):
        if not is_valid(mapeo):
            return False
        mapeo = dict(mapeo)
        if len(mapeo) == len(letras):
            pprint(mapeo)
            return
        c = ordenamiento_mas_restringido(mapeo)
        for i in ordenamiento_menos_restringido(mapeo, c):
            mapeo[c] = i
            solve(mapeo)

    solve({})