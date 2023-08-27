#Isaac Kim 
#CS 463
#Homework 1

# given a positive integer n, 
# return a list of its prime factors, 
# in increasing order. Include the correct number of occurrences
def prime_factors(n):
	p_factors = []
	factor = 2
	while(n > 1):
		if(n%factor == 0):
			n /= factor
			p_factors.append(factor)
		else:
			factor += 1
	return p_factors

# Given two positive integers a and b,
# return whether they are co-prime or not. 
# Coprime numbers don't share any divisors other than 1.
def coprime(a,b):
	a_factors = prime_factors(a)
	b_factors = prime_factors(b)
	for i in b_factors:
		for j in a_factors:
			if(i == j):
				return False
	return True

# Given a non-negative integer n, 
# calculate the nth tribonnaci number 
# (where values are the sums of the previous three items in the sequence, 
# or 1 when there aren't enough values ahead of it). 
# For our purposes, the sequence begins as 1,1,1,.... 
def trib(n):
	index = n
	seq = [1,1,1]
	i = 2
	#i represents the third element, but n = i + 1
	#e.g. if n = 3, i = 2
	while(n > i):
		next_term = seq[-1] + seq[-2] + seq[-3]
		seq.append(next_term)
		n -= 1
	return seq[index]

# Given a list of integers xs, 
# find the two largest values and return them in a list (largest first). 
# Largest duplicates should be preserved(included) in the output. 
# Do not modify the original list.
def max_two(xs):
	#base case if array has less than 2 elements, return the array itself
	if(len(xs) < 2):
		return xs

	prev_max = xs[0]
	new_max = xs[1]

	#sort initially
	if(prev_max > new_max):
		prev_max = xs[1]
		new_max = xs[0]

	for i in xs[2:]:
		if(i > new_max):
			prev_max = new_max
			new_max = i

		elif(i > prev_max):
			prev_max = i

	#largest first
	if(new_max > prev_max):
		return [new_max,prev_max]
	return [prev_max,new_max]

# Given a list of values, 
# create the list whose values are in the opposite order. 
# Do not modify the original list.
def reversed(xs):
	new_list = []
	for i in range(len(xs)):
		new_list.append(xs[len(xs)-i-1])

	return new_list

# Given a list of lists, 
# when it is rectangular (all rows have same length), 
# create and return the list of lists that contains the same values, but rotated clockwise. 
# If this 2D list is not rectangular, raise a ValueError with the message "not rectangular". 
# Do not modify the original 2D list.
def clockwise(grid):
	#base case for empty grid
	if(len(grid) == 0):
		return []
	#set variable for comparing length of elements in grid
	length = len(grid[0])
	for i in grid:
		if(len(i) != length):
			raise ValueError('not rectangular')

	results = []
	#rectangular clockwise shift
	for i in range(length):
		temp = []
		for j in range(len(grid)):
			temp.append(grid[len(grid)-j-1][i])

		results.append(temp)
	return results

# Given a list of bool values, 
# return True if any of them is True, 
# and return False if every single one of them is False. 
# Do not modify the original list.
def any(xs):
	for i in xs:
		if(i == True):
			return True
	return False

# Given a predicate function p (which accepts a single argument and returns a boolean), 
# as well as a list of values xs, 
# create a list of all items from xs that pass predicate function p. 
# Preserve ordering in your output, and do not modify the original list.
def select(p, xs):
	x_pass = []
	for i in xs:
		if(p(i)):
			x_pass.append(i)
	return x_pass

# Given a two-argument function and two lists of arguments to supply, 
# create a list of the results of applying the function to each same-indexed pair 
# of arguments from the two lists. (Zip up the two lists with the function). 
# The answer is as long as the shortest of the two input lists. 
# Do not modify the argument lists.
def zip_with(f, xs, ys):
	results = []
	length = len(xs)
	if(len(xs) > len(ys)):
		length = len(ys)

	for i in range(length):
		results.append(f(xs[i],ys[i]))

	return results

# Given positive ints r and c indicating number of rows and columns, 
# create a 2D list that represents the "augmented identity matrix" with that dimension: 
# It's the k x k identity matrix (where k = min(r,c)), 
# and augmented rightwards or downwards as needed with zeroes in order to be of size r x c. 
# Stated another way, it's an r x c matrix filled with zeroes that has ones along its main diagonal.
def augdentity(r,c):
	matrix = []
	for i in range(r):
		temp = []
		for j in range(c):
			if(i == j):
				temp.append(1)
			else:
				temp.append(0)
		matrix.append(temp)
	return matrix

def main():
    print(reversed([True, "poly", [1,2,3],{"five":5, "three":3},0]))

if __name__ == "__main__":
    main()