import cvxpy as cvx
import numpy as np
import math
from scipy.optimize import minimize
import sys

# Pick the right range to use (Python 3 range = Python 2 xrange)
range = range if sys.version_info >= (3, 0) else xrange

class Bandit:
    def __init__(self, regularization_param=0.1, num_items=3, num_positions=2, len_contexts=3):
        self.K = num_items
        self.M = num_positions
        self.S = num_positions
        self.C = len_contexts

        self.D = (num_items * num_positions) + num_positions + len_contexts

        self.m = np.zeros(self.D)
        self.q = np.full(self.D, regularization_param)

    def handle_user_action(self, context, item, pos, reward):
        (new_m, new_q) = self.update_posterior(self.gen_psi(self.C, self.K, self.M, context, item, pos),
                                               reward, self.m, self.q)

        # Update the m and q vectors
        self.m = new_m
        self.q = new_q

    def get_items(self, context):
        w = self.sample_posterior(self.m, self.q)
        e = self.expected_values(self.C, self.K, self.M, w, context)
        f = self.compute_superarm(e, self.S)

        items = []
        for m in range(self.M):
            for k in range(self.K):
                if f[k, m] >= 0.9:
                    items.append(k)
                    break

        return items

    @staticmethod
    def gen_psi(C, K, M, context, item, pos):
        # n-hot vector saying which item(s) were picked (currently just one)
        gamma = np.zeros(M)
        gamma[pos] = 1

        phi = np.zeros(K * M)
        phi[item + K * pos] = 1

        # Context vector
        psi = np.zeros(C)
        psi[0] = context["age"]
        psi[1] = 1 if context["gender"] == "male" else 0
        psi[2] = 1 if context["gender"] == "female" else 0

        return np.concatenate((phi, psi, gamma))

    @staticmethod
    def sample_posterior(m, q):
        """Return a sample of the parameters (a vector of independent Gaussians with means m and precisions q)"""
        return [np.random.normal(m[j], 1/math.sqrt(q[j])) for j in range(len(m))]

    @classmethod
    def expected_values(cls, C, K, M, w, context):
        ev = np.zeros((K, M))
        for k in range(K):
            for m in range(M):
                ev[k][m] = np.dot(w, cls.gen_psi(C, K, M, context, k, m))
        return ev

    @staticmethod
    def compute_superarm(expected_values, S):
        """Given a matrix of expected rewards, solve the LP to get the best super-arm with S items"""
        (K, M) = expected_values.shape

        # Variables
        f = cvx.Variable(K, M)

        # Objective
        objective = cvx.Maximize(cvx.sum_entries(cvx.mul_elemwise(expected_values, f)))

        # Constraints
        constraints = [f >= 0,
                       f <= 1,
                       cvx.sum_entries(f) == S,
                       cvx.sum_entries(f, axis=0) <= 1,
                       cvx.sum_entries(f, axis=1) <= 1]

        cvx.Problem(objective, constraints).solve()
        return f.value

    @staticmethod
    def update_posterior(event, reward, m, q):
        """Run a solver to find an updated m and q given the event and reward."""
        def objective(w):
            term1 = -0.5 * sum(q[i] * (w[i]-m[i]) * (w[i]-m[i]) for i in range(len(event)))
            term2 = -math.log(1+math.exp(-1 * reward * np.dot(w, event)))
            result = -(term1 + term2)
            return result

        x0 = np.zeros(len(event))
        res = minimize(objective, x0, method='nelder-mead', options={'xtol': 1e-6, 'disp': False})

        prob = 1 / (1 + math.exp(-np.dot(res.x, event)))
        new_q = q + prob * (1 - prob) * (event**2)

        return (res.x, new_q)


b = Bandit()

ctx = {"age": 55, "gender": "female"}

for i in range(20):
    item = 0
    pos = 1
    reward = 1

    b.handle_user_action(ctx, item, pos, reward)
    print("m: ", b.m)

    items = b.get_items(ctx)
    print("items: ", items)
