
if __name__ == "__main__":
    with open('input.txt', 'r') as f:
        for line in f:
            print "---"
            print line
            d = {letter:line.count(letter) for letter in line}
            print d
            print d.values()
            print 2 in d.values()
