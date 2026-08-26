handsetList = [
  "Apple iPhone 6s (32GB Rose Gold)",
  "Apple iPhone X (64GB Space Grey Refurbished Grade A)",
  "Apple iPhone 11 (128GB Space Grey)",
  "Samsung Galaxy A50 (128GB Black)",
  "Fairphone 3 Dual Sim (64GB Black)",
  "Samsung Galaxy S10e (128GB Prism White)",
  "Samsung Galaxy S10e (128GB Prism Black)",
  "Samsung Galaxy A70 Dual Sim (128GB Black)",
  "Samsung Galaxy S10 5G (256GB Crown Silver)",
  "Huawei P30 Pro (128GB Aurora Black)",
  "Huawei P30 Pro (128GB Black)",
  "Huawei P30 Pro (128GB Breathing Crystal)",
  "Huawei P30 (128GB Black)",
  "Google Pixel 3A (64GB Just Black)",
  "Google Pixel 4 (64GB Clearly White)",
  "Google Pixel 4 XL (64GB Just Black)",
  "Google Pixel 4 XL (64GB Clearly White)",
  "Sony Xperia 1 (128GB Black)",
  "Sony Xperia 10 (64GB Black)",
  "Huawei Mate 20 Lite (64GB Black)"
]

from collections import namedtuple

Ranking = namedtuple('Ranking', 'handset rank')

def getRank(handset, searchTerms):
  handset = handset.replace(' ', '').lower()
  rank = 0
  for term in searchTerms:
    if term in handset:
      rank = rank + 1
  return rank

def assignRanks(handsets, searchTerms):
  for handset in handsets:
    yield Ranking(handset, getRank(handset, searchTerms))

while True:
  searchString = input("> ")
  searchTerms = list(map(lambda x : x.lower(), searchString.split(' ')))

  results = assignRanks(handsetList, searchTerms)
  results = filter(lambda x : x.rank > 0, results)
  results = sorted(results, key=lambda x: x.rank, reverse=True)
  results = map(lambda x : x.handset, results)

  for i in list(results):
    print(i)
