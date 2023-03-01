import collections

def opgave1(mylist):
    j = 1
    for i in mylist:
        if j != i:
            return False
        j += 1
    return True

def opgave2(mylist):
    yield [j for j in range(1, len(mylist) +1) if j not in mylist]

def opgave3a(filename):
    list = []
    with open(filename) as f:
        for line in f:
            l = [int(i) for i in line.split()]
            list.append(l)
    return list


def opgave3b(mylist):
    for row in mylist:
        print(*row)

def opgave3(filename):
    opgave3b( opgave3a( filename ) )

def sum_nested_it(mylist):
    pass