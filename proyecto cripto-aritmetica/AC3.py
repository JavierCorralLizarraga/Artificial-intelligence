
# Define the variables and their domains
variables = ['S', 'E', 'N', 'D', 'M', 'O', 'R', 'Y']
domains = {'S': [9], 'E': [5,6,7,8], 'N': [0,1,2,3,4,6,7,8,9], 'D': [0,1,2,3,4,5,6,8,9],
           'M': [1], 'O': [0,2,4,6,7,8], 'R': [0,2,3,4,5,6,7,8,9], 'Y': [0,2,5,6,7,8,9]}

# Define the constraints
binary_constraints = [('S', 'M'), ('S', 'E'), ('M', 'O'), ('O', 'R'), ('R', 'E'), ('E', 'Y'),
                      ('S', 'D'), ('E', 'N'), ('N', 'D'), ('D', 'Y')]

nary_constraint = ('S', 'E', 'N', 'D', 'M', 'O', 'R', 'Y')

# Define the neighbors function
def neighbors(var):
    if var in nary_constraint:
        return [v for v in variables if v != var]
    else:
        return [v for v in variables if (v, var) in binary_constraints or (var, v) in binary_constraints]

# Define the revise function
def revise(Xi, Xj, domains):
    revised = False
    if (Xi, Xj) in binary_constraints:
        values_i = domains[Xi]
        values_j = domains[Xj]
        for x in values_i:
            if not any([x != y for y in values_j]):
                domains[Xi].remove(x)
                revised = True
    elif Xi in nary_constraint:
        values_i = domains[Xi]
        values_j = [domains[v][0] for v in neighbors(Xi)]
        for x in values_i:
            if not any([x + sum(values_j) - values_j[k] == 10 for k in range(len(values_j))]):
                domains[Xi].remove(x)
                revised = True
    return revised

# Define the AC-3 algorithm
def ac3(queue, domains):
    while queue:
        (Xi, Xj) = queue.pop(0)
        if revise(Xi, Xj, domains):
            if len(domains[Xi]) == 0:
                return False
            for Xk in neighbors(Xi):
                if Xk != Xj:
                    queue.append((Xk, Xi))
        if (Xj, Xi) in queue:
            queue.remove((Xj, Xi))
            queue.append((Xi, Xj))
            # Define the initial queue
            queue = binary_constraints[:]
            for var in nary_constraint:
                for neighbor in neighbors(var):
                    queue.append((var, neighbor))

# Define the initial queue
queue = binary_constraints[:]
for var in nary_constraint:
    for neighbor in neighbors(var):
        queue.append((var, neighbor))

# Run the AC-3 algorithm
ac3(queue, domains)

# Print the final domain of each variable
print(domains)

# Find the solution
solution = {var: domains[var][0] for var in variables}

# Print the solution
print(solution)
