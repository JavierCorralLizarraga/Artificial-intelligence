from ortools.constraint_solver import pywrapcp
def cons1():
    # Constraint programming engine
    solver = pywrapcp.Solver('send more money')
    base = 10
    # variables de decision
    digitos = list(range(0, base))
    digitos_sin_0 = list(range(1, base))
    s = solver.IntVar(digitos_sin_0, 'S') # como es el primer digito no puede ser 0
    m = solver.IntVar(digitos_sin_0, 'M') # igual aca
    e = solver.IntVar(digitos, 'E')
    n = solver.IntVar(digitos, 'N')
    d = solver.IntVar(digitos, 'D')
    o = solver.IntVar(digitos, 'O')
    r = solver.IntVar(digitos, 'R')
    y = solver.IntVar(digitos, 'Y')

    # necesitamos un grupo de variables en lista para utilizar la restriccion AllDifferent
    letras = [s,m,e,n,d,o,r,y]
    # verificamos que tenemos suficientes digitos
    assert base >= len(letras)
    # definimos las restricciones
    solver.Add(solver.AllDifferent(letras))
    # SEND + MORE = MONEY
    solver.Add( (d + e) + base * (n + r) + base ** 2 * (e + o) + base ** 3 *(s + m) \
                == y + e * base + n * base ** 2 + o * base ** 3 + m * base ** 4)
    solution_count = 0
    db = solver.Phase(letras, solver.INT_VAR_DEFAULT, solver.INT_VALUE_DEFAULT)
    solver.NewSearch(db)
    while solver.NextSolution():
        print(letras)
        assert (s.Value()*base**3 + e.Value()*base**2 + n.Value()*base + d.Value() \
                + m.Value()*base**3 + o.Value()*base**2 + r.Value()*base + e.Value() \
                == m.Value()*base**4 + o.Value()*base**3 + n.Value()*base**2 + e.Value()*base + y.Value()
                )
        solution_count += 1
    solver.EndSearch()
    print(f'Number of solutions found: {solution_count}')