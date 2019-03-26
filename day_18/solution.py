#!/usr/bin/env python3

from sys import stdin

class lumber_collection_area:

  def __init__(self, src):
    self.state = [line.strip() for line in src]
    self.M, self.N = self.dimensions()

  def dimensions(self):
    return len(self.state[0]), len(self.state)

  def __eq__(self, other):
    return str(self) == str(other)

  def __getitem__(self, key):
    x, y = key
    return self.state[y][x]

  def neighbors(self, a, b):
#    print('M:', self.M, 'N:', self.N, 'a:', a, 'min(N, a + 1):', min(self.M, a + 1))
    xs = range(max(0, a - 1), min(self.M - 1, a + 1) + 1)
    ys = range(max(0, b - 1), min(self.N - 1, b + 1) + 1)
    return [(x, y) for x in xs for y in ys if (x, y) != (a, b)]

  def nextCell(self, x, y):
    around = [self[a, b] for a, b in self.neighbors(x, y)]
    nearby = lambda w: len([c for c in around if c == w])
    nearby_trees = nearby('|')
    nearby_lumberyard = nearby('#')

    if self[x, y] == '.':
      if nearby_trees >= 3:
        return '|'
      else:
        return '.'
    elif self[x, y] == '|':
      if nearby_lumberyard >= 3:
        return '#'
      else:
        return '|'
    elif self[x, y] == '#':
      if nearby_trees >= 1 and nearby_lumberyard >= 1:
        return '#'
      else:
        return '.'
    else:
      raise "uh oh"

  def all(self):
    game = self
    while True:
      yield game
      game = game.next()

  def next(self):
    return lumber_collection_area([''.join([self.nextCell(x, y) for x in range(0, self.M)]) for y in range(0, self.N)])

  def cells(self):
    return [cell for row in self.state for cell in row]

  def value(self):
    t = len(list(filter(lambda c : c == '|', self.cells())))
    l = len(list(filter(lambda c : c == '#', self.cells())))
    return t * l

  def __str__(self):
    return '\n'.join(self.state)

game = lumber_collection_area(stdin)

#print(game[5,1])
#print(game.neighbors(5, 1))
#print(game.nextCell(5, 1))

from sys import argv

n = int(argv[1]) if len(argv) > 1 else 10

previous = []

def foo(i, game):
  print('==== After', i, 'minutes ====')
  print(game)
  print('==== Value is', game.value(), '====')

for j in range(n + 1):
  if game in previous:
    print('whoop whoop we found a repeat')
    i = previous.index(game)
    cycle = previous[i:j]
    final_game = cycle[(n - i) % len(cycle)]
    foo(n, final_game)
    break
  foo(j, game)
  previous.append(game)
  game = game.next()

