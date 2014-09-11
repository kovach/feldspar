Unroll 8
#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


void assign__de__mtx(uint32_t v0, struct array * v1, struct array * v2, struct array * * out)
{
  struct array * v20 = NULL;
  struct array * v19 = NULL;
  float v22;
  float v21;
  uint32_t len0;
  struct array * v24 = NULL;
  struct array * v23 = NULL;
  float v26;
  float v25;
  
  v19 = at(struct array *,v1,0);
  v20 = initArray(v20, sizeof(float), 3);
  for (uint32_t v13 = 0; v13 < 3; v13 += 1)
  {
    at(float,v20,v13) = (at(float,v19,v13) - at(float,v2,v13));
  }
  v22 = 0.0;
  for (uint32_t v15 = 0; v15 < 3; v15 += 1)
  {
    v21 = at(float,v20,v15);
    v22 = (v22 + (v21 * v21));
  }
  len0 = (v0 - 1);
  *out = initArray(*out, (0 - sizeof(struct array *)), len0);
  for (uint32_t v3 = 0; v3 < len0; v3 += 1)
  {
    v23 = at(struct array *,v1,(v3 + 1));
    v24 = initArray(v24, sizeof(float), 3);
    for (uint32_t v7 = 0; v7 < 3; v7 += 1)
    {
      at(float,v24,v7) = (at(float,v23,v7) - at(float,v2,v7));
    }
    v26 = 0.0;
    for (uint32_t v9 = 0; v9 < 3; v9 += 1)
    {
      v25 = at(float,v24,v9);
      v26 = (v26 + (v25 * v25));
    }
    at(struct array *,*out,v3) = initArray(at(struct array *,*out,v3), sizeof(float), 3);
    for (uint32_t v6 = 0; v6 < 3; v6 += 1)
    {
      at(float,at(struct array *,*out,v3),v6) = ((at(float,v24,v6) / v26) - (at(float,v20,v6) / v22));
    }
  }
  freeArray(v20);
  freeArray(v19);
  freeArray(v24);
  freeArray(v23);
}
