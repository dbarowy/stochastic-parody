cmu = open("cmu_pos_dict.txt","r")
noun = []
adj = []
verb = []
adv = []

lists = [noun, adj, verb, adv]

dictFiles = ["dict_noun.txt", "dict_adj.txt", "dict_verb.txt", "dict_adv.txt"]

for i in range(len(dictFiles)):
    print("Starting " + dictFiles[i])
    with open(dictFiles[i], "r") as cdict:
        for line in cdict:
            lists[i].append(line.strip().upper())
    

def remove_nums(w):
    for i in range(10):
        si = str(i)
        if si in w:
            w = w.replace(si, "")
    return w

def contains_num(w):
    # out = False
    for i in range(10):
        si = str(i)
        if si in w:
            return True
    return False

# out = ""
clear = open("new_cmu_pos_dict.txt", "w")
clear.write("")
clear.close()

f = open("new_cmu_pos_dict.txt", "a")

for l in cmu:
    line = l.split(" ")
    word = line[0]
    if contains_num(word):
        continue
    # line[0] = word
    if word in noun:
        line[1] = "noun"
        f.write(" ".join(line)) #out += "".join(line)
    elif word in adj:
        line[1] = "adj"
        f.write(" ".join(line)) #out += "".join(line)
    elif word in verb:
        line[1] = "verb"
        f.write(" ".join(line)) #out += "".join(line)
    elif word in adv:
        line[1] = "adv"
        f.write(" ".join(line)) #out += "".join(line)
    else:
        line[1] = "other"
        f.write(" ".join(line)) #out += "".join(line)

cmu.close()

# f.write(out)
f.close()
