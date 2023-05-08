from pprint import pprint
s1 = 'send'
s2 = 'more'
s3 = 'money'

letras = list(set(s1 + s2 + s3))
letras.sort()
def suma(a1, a2):
    l = len(a1)
    res = [None] * l
    carry = 0
    for i in reversed(range(l)):
        if a1[i] is None or a2[i] is None:
            carry = 0
            continue
        res[i] = a1[i] + a2[i] + carry
        if res[i] >= 10:
            res[i] -= 10
            carry = 1
        else:
            carry = 0
    if a1[0] is None or a2[0] is None:
        return [None] + res
    return [carry] + res
def replace(string, mapeo):
    return [mapeo.get(string[i], None) for i in range(len(string))]
def matches(res1, res2):
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
    for i in possible_values(mapping):
        m[c] = i
        if is_valid(m):
            count += 1
    return count
def most_restrained_variable(mapping):
    min_count = 10000
    vals = [mapping[key] for key in mapping]
    result = None
    for c in letras:
        if c not in mapping:
            count = value_count(mapping, c)
            if count < min_count:
                min_count = count
                result = c
    return result
def possible_values(mapping):
    vals = [mapping[key] for key in mapping]
    for i in range(10):
        if i not in vals:
            yield i
def least_constrained_ordering(mapping, c):
    def howgood(i):
        m = dict(mapping)
        m[c] = i
        return value_count(m, most_restrained_variable(m))
    ordering = list(possible_values(mapping))
    ordering.sort(key=howgood)
    return reversed(ordering)
def is_valid(mapping):
    if mapping.get(s3[0], None) == 0:
        return False
    summ = suma(replace(s1, mapping), replace(s2, mapping))
    return matches(replace(s3, mapping), summ)
def solve(mapeo):
    if not is_valid(mapeo):
        return False
    mapeo = dict(mapeo)
    if len(mapeo) == len(letras):
        pprint(mapeo)
        print('done!')
        exit()
    c = most_restrained_variable(mapeo)
    for i in least_constrained_ordering(mapeo, c):
        mapeo[c] = i
        solve(mapeo)

print(solve({}))