from random import *
def copy_list(list):
    newlist = []
    for x in list:
        newlist.append(x)
    return newlist
class Individual:
    def __init__(self, size=0):
        self.chromosome = []
        for i in range(size):
            self.chromosome.append(0)
        self.size = size
        self.fit = 0
    def __repr__(self):
        s = "chr: %s f: %f len: %d" % (str(self.chromosome),
                                       self.fit, self.size)
        return s
    def copy(self):
        ind = Individual()
        ind.chromosome = copy_list(self.chromosome)
        ind.fit = self.fit
        ind.size = self.size
        return ind
    def fromlist(self, L):
        self.size = len(L)
        self.chromosome = copy_list(L)
        self.fit = 0
    def __lt__(self, other):
        return self.fit < other.fit
    def __eq__(self, other):
        return self.fit == other.fit
    def init_random(self, limit=10):
        for i in range(self.size):
            self.chromosome[i] = randint(0, limit - 1)

    def mutation_opp(self, prob):
        for i in range(self.size):
            r = random()
            if r <= prob:
                self.chromosome[i] = 9 - self.chromosome[i]
    def crossover_1pt(self, parent2):
        site = randint(0, self.size - 1)
        for i in range(site):
            self.chromosome[i], parent2.chromosome[i] = parent2.chromosome[i], self.chromosome[i]  # the famous swap

def eval_ind(ind):
    eval_sendmoremoney(ind)
def eval_sendmoremoney(ind):
    D = ind.chromosome[0]
    E = ind.chromosome[1]
    M = ind.chromosome[2]
    N = ind.chromosome[3]
    O = ind.chromosome[4]
    R = ind.chromosome[5]
    S = ind.chromosome[6]
    Y = ind.chromosome[7]
    x1 = ind.chromosome[8]
    x2 = ind.chromosome[9]
    x3 = ind.chromosome[10]
    fitness = 0
    fitness += eval_plus(D, E, Y, x1)
    fitness += eval_plus(N, R, E, x2)
    fitness += eval_plus(E, O, N, x3)
    fitness += eval_plus(S, M, O, M)
    for i in range(ind.size):
        if (not ind.chromosome[i] in ind.chromosome[0:i] and
            not ind.chromosome[i] in ind.chromosome[(i+1):(ind.size-1)]):
            fitness += 0.1
    ind.fit = fitness
def eval_plus(A, B, C, x):
    val1 = 1.0 - abs(A+B-C)/18.0 + 1-x/9.0
    val2 = 1.0 - abs(A+B-10-C)/19.0 + 1-abs(x-1)/9.0
    return max(val1, val2)
def selection(cdf):
    size = len(cdf)
    r = random() * cdf[size-1]
    i = 0
    while cdf[i] < r:
        i += 1
    return i
def new_generation(population, mutt=1, cp=0.8, cm=0.01, elite=0):
    try:
        pop_size = len(population)
    except:
        print(population)
    cdf = [population[0].fit]
    for i in range(1,pop_size):
        cdf.append(cdf[len(cdf)-1]+population[i].fit)
    new_gen = []
    for i in range(pop_size//2):
        p1 = selection(cdf)
        p2 = selection(cdf)
        c1 = population[p1].copy()
        c2 = population[p2].copy()
        r = random()
        if r <= cp:
            c1.crossover_1pt(c2)
        if mutt == 1:
            c1.mutation_opp(cm)
            c2.mutation_opp(cm)
        new_gen.append(c1)
        new_gen.append(c2)
    if elite:
        new_gen.append(population[pop_size-1].copy())
    for i in range(pop_size):
        eval_ind(new_gen[i])
    new_gen.sort()
    return new_gen
def random_population(ind_size, pop_size):
    population = []
    for i in range(pop_size):
        ind = Individual(ind_size)
        ind.init_random()
        eval_ind(ind)
        population.append(ind)
    population.sort()
    return population
def gen_algo(ind_size, pop_size, gen_number,
             mutt=1, cp=0.8, cm=0.01, elite=0):
    population = random_population(ind_size, pop_size)
    #print("Starting from the best individual:", population[pop_size-1])
    for i in range(gen_number):
        new_gen = new_generation(population, mutt, cp, cm, elite)
        del population[0:pop_size]
        population = new_gen
    #print("The best solution is:", population[pop_size-1])
    print(population[pop_size - 1])
    print(['D', 'E', 'M', 'N', 'O', 'R','S', 'Y', 'x1', 'x2', 'x3'])

def preset_gen_algo():
    return gen_algo(11, 20, 200, 1, 0.8, 0.01, 1)


