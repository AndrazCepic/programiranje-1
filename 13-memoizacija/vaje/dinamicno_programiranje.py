from functools import lru_cache

test_matrix =[
    [1, 2, 0], 
    [2, 4, 5],
    [7, 0, 1]
]

test_matrix_2 = [[i for i in range(100)] for j in range(100)]

def max_cheese(matrix):
    w = len(matrix[0])
    h = len(matrix)

    @lru_cache(maxsize=None)
    def miska(i, j):
        if i == w or j == h:
            return 0

        return matrix[j][i] + max(
            miska(i + 1, j),
            miska(i, j + 1)
        )

    return miska(0, 0)

print(max_cheese(test_matrix_2))