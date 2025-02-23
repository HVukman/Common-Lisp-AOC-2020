r = open('input.txt').read().strip('\n')
input = r.splitlines()

#Part 1
seats = [int(x.replace('F','0').replace('B','1').replace('L','0').replace('R','1'),2) for x in input]
seats.sort()
print(seats[-1])

#Part 2
for x in range(len(seats)):
    if seats[x+1] - seats[x] != 1:
        print(seats[x] + 1)
        break