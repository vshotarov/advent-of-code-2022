with open("day20/input.txt","r") as f:
    data = [int(x) for x in f.read().splitlines()]

init = list(enumerate(data))

for a in list(enumerate(data)):
    i = init.index(a)
    (ii,xx) = init.pop(i)
    i_new = (i + xx) % len(init)
    if i_new != 0:
        init.insert(i_new, (ii,xx))
    else:
        init.append((ii,xx))
    #print(i_new,a,[x[1] for x in init])

ns = [x[1] for x in init]
i_0 = ns.index(0)
print(ns)
print(sum([ns[(i_0+i) % len(ns)] for i in [1000,2000,3000]]))

if False:
    for c in range(10):
        print(c)
        for a in list(enumerate(data)):
            i = init.index(a)
            (ii,xx) = init.pop(i)
            i_new = (i + xx*811589153) % len(init)
            if i_new != 0:
                init.insert(i_new, (ii,xx))
            else:
                init.append((ii,xx))
            #print(i_new,a,[x[1] for x in init])

    ns = [x[1] for x in init]
    i_0 = ns.index(0)
    print(sum([ns[(i_0+i) % len(ns)]*811589153 for i in [1000,2000,3000]]))


#init = [(x,i) for i,x in enumerate(data)]
#ns = {(x,i):i for (x,i) in init}
#n = len(data)
#
#def pprint(a):
#    print([k[0] for (k,v) in sorted([(k,v) for k,v in a.items()], key=lambda x: x[1])])
#
#for (x,i) in init:
#    print("#" * 10 + " processing", (x,i))
#    print("Start:")
#    pprint(ns)
#
#    i_current = ns[(x,i)]
#    i_new = (i_current + x) % len(ns)
#
#    if i_current == i_new:
#        continue
#
#    ns_new = {}
#    for (xx,ii),iii in ns.items():
#        if i_new < i_current:
#            if iii > i_new and iii <= i_current:
#                ns_new[(xx,ii)] = iii+1
#                continue
#        else:
#            if iii >= i_current and iii < i_new:
#                ns_new[(xx,ii)] = iii-1
#                continue
#        ns_new[(xx,ii)] = iii
#
#    ns = ns_new
#    ns[(x,i)] = i_new
#
#    print(ns)
#    print("End:")
#    pprint(ns)
