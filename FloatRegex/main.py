import re
a = ['1', '5.5', '55.55', '2.78', '2.0', '2.', '0.78', '2.78e-1', '2e10', '2e+10', '2e-10', '0e0', '-545e-4345',
     '-e', '.78', 'abc', 'eee', 'e', '5e2.45', 'e2.45']
for word in a:
    if re.fullmatch(r"[-+]?\d+\.?\d*([Ee][-+]?\d+)?", word):
        print(word, 'Yes')
    else:
        print(word, 'No')