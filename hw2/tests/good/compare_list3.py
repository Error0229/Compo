# lexicographic order
# True
print([1, 2, 3] > [1, 2])        # True, first list is longer and the first two elements are the same
print([3, 2] == [3, 2])          # True, both lists are identical
print([1, 0, 0] <= [1, 0, 0, 1]) # True, first list is shorter
print([9, 1, 0] >= [9, 1, 0])    # True, both lists are identical
#False
print([2] != [2])                # False, the lists are identical
print([7, 8] > [7, 8, 0])        # False, the first two elements are the same but the second list is longer
print([4, 5, 6] < [4, 5])        # False, the first two elements are the same but the first list is longer
print([3] < [2])                 # False, 3 is greater than 2
