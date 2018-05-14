
# A possible solution given in Python

# we construct non-empty lists as pairs of a head element and the remaining list

# we construct empty lists using `None`
nil = lambda : None

def head(xs):
  (hd, ignored) = xs()
  return hd

def tail(xs):
  (ignored, tl) = xs()
  return tl

def isNil(xs):
  return xs() == nil()
  
def append(xs,ys):
  if isNil(xs):
    return ys
  else:
    # We could use a lambda here as well to postpone the evaluation,
    # however, we already triggered one evaluation step by using `isNil`
    # anyway
    (hd,tl) = xs()
    
    return lambda : (hd, append(tl,ys))

def fromL(n):
  return lambda : (n, fromL(n+1))

def take(n,xs):
  if n <= 0 or isNil(xs):
    return nil
  else:
  	(hd,tl) = xs()
  	return lambda : (hd, take(n-1, tl))

def toList(xs):
  if isNil(xs):
  	return []
  else:
    (hd,tl) = xs()
    return [hd] + toList(tl)

# Listenkonstruktion per Bildungsregel
def iterate(f,x):
  return lambda : (x, iterate(f, f(x)))

# Sieb des Eratosthenes
def filter(p,xs):
  if isNil(xs):
    return nil
  else:
    (hd,tl) = xs()
    # We could make the test under the lambda exression, however,
    # we're are strict in all arguments of the list anyway, so we
    # could only benefit from this if the predicate fails.
    if p(hd):
      return lambda : (hd, filter(p, tl))
    else:
      return filter(p, tl)

def sieve(xs):
  (hd,tl) = xs()
  return lambda : (hd, sieve(filter(lambda y : y % hd > 0,tl)))

primes = sieve(fromL(2))

# >>> toList(take(10,primes))
# toList(take(10,primes))
# [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
# >>> toList (take(10,(append(fromL(10),fromL(20)))))
# toList (take(10,(append(fromL(10),fromL(20)))))
# [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
#
