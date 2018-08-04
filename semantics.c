#include <stdio.h>
#include <stdlib.h>
#include "pascal.h"

Constant *new_ordinal_constant(Type *t, Ordinal i) {
  Constant *cnst = new(Constant);

  cnst->type = t;
  cnst->i = i;
  return cnst;
}

Type *new_ordinal_type(Type *s, Ordinal low, Ordinal high) {
  Type *t = new(Type);

  t->class = ORDINAL_TYPE;
  if (s == 0)
    t->ordinal.base = t;
  else
    t->ordinal.base = s;
  t->ordinal.low = low;
  t->ordinal.high = high;
  return t;
}
