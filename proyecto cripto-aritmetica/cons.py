from ortools.constraint_solver import pywrapcp
from ortools.sat.python import cp_model

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

def cons2():
    model = cp_model.CpModel()

    # Define variables
    S = model.NewIntVar(1, 9, 'S')
    M = model.NewIntVar(1, 9, 'M')
    E = model.NewIntVar(0, 9, 'E')
    N = model.NewIntVar(0, 9, 'N')
    D = model.NewIntVar(0, 9, 'D')
    O = model.NewIntVar(0, 9, 'O')
    R = model.NewIntVar(0, 9, 'R')
    Y = model.NewIntVar(0, 9, 'Y')
    C = [model.NewIntVar(0, 9, f'C{i}') for i in range(4)]

    # Add constraints
    model.AddAllDifferent([S, E, N, D, M, O, R, Y])
    model.Add(C[3] == M)
    model.Add(C[2] + S + M == O + C[3] * 10)
    model.Add(C[1] + E + O == N + C[2] * 10)
    model.Add(C[0] + N + R == E + C[1] * 10)
    model.Add(D + E == Y + C[0] * 10)

    # Solve the model
    solver = cp_model.CpSolver()
    status = solver.Solve(model)

    if status == cp_model.OPTIMAL:
        print(
            f'Solution found:\nS = {solver.Value(S)}\nE = {solver.Value(E)}\nN = {solver.Value(N)}\nD = {solver.Value(D)}\nM = {solver.Value(M)}\nO = {solver.Value(O)}\nR = {solver.Value(R)}\nY = {solver.Value(Y)}')
    else:
        print('No solution found.')
