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


void assign__de__mtx(struct array * v0, struct array * v1, struct array * * out)
{
  uint32_t v18;
  uint32_t v19;
  uint32_t v21;
  struct array * v20 = NULL;
  struct array * e0 = NULL;
  uint32_t len1;
  struct array * v23 = NULL;
  struct array * v22 = NULL;
  uint32_t len2;
  float v25;
  float v24;
  uint32_t len3;
  uint32_t v27;
  struct array * v26 = NULL;
  struct array * e4 = NULL;
  uint32_t len5;
  struct array * v29 = NULL;
  struct array * v28 = NULL;
  uint32_t len6;
  float v31;
  float v30;
  uint32_t len7;
  
  v18 = getLength(v0);
  v19 = getLength(v1);
  v20 = at(struct array *,v0,0);
  len1 = min(getLength(v20), v19);
  e0 = initArray(e0, sizeof(float), len1);
  for (uint32_t v4 = 0; v4 < len1; v4 += 1)
  {
    at(float,e0,v4) = (at(float,v20,v4) - at(float,v1,v4));
  }
  v21 = getLength(e0);
  v22 = at(struct array *,v0,0);
  len2 = min(getLength(v22), v19);
  v23 = initArray(v23, sizeof(float), len2);
  for (uint32_t v12 = 0; v12 < len2; v12 += 1)
  {
    at(float,v23,v12) = (at(float,v22,v12) - at(float,v1,v12));
  }
  v25 = 0.0;
  for (uint32_t v14 = 0; v14 < v21; v14 += 1)
  {
    v24 = at(float,v23,v14);
    v25 = (v25 + (v24 * v24));
  }
  len3 = (v18 - min(v18, 1));
  *out = initArray(*out, (0 - sizeof(struct array *)), len3);
  for (uint32_t v2 = 0; v2 < len3; v2 += 1)
  {
    v26 = at(struct array *,v0,(v2 + 1));
    len5 = min(getLength(v26), v19);
    e4 = initArray(e4, sizeof(float), len5);
    for (uint32_t v3 = 0; v3 < len5; v3 += 1)
    {
      at(float,e4,v3) = (at(float,v26,v3) - at(float,v1,v3));
    }
    v27 = getLength(e4);
    v28 = at(struct array *,v0,(v2 + 1));
    len6 = min(getLength(v28), v19);
    v29 = initArray(v29, sizeof(float), len6);
    for (uint32_t v6 = 0; v6 < len6; v6 += 1)
    {
      at(float,v29,v6) = (at(float,v28,v6) - at(float,v1,v6));
    }
    v31 = 0.0;
    for (uint32_t v8 = 0; v8 < v27; v8 += 1)
    {
      v30 = at(float,v29,v8);
      v31 = (v31 + (v30 * v30));
    }
    len7 = min(v27, v21);
    at(struct array *,*out,v2) = initArray(at(struct array *,*out,v2), sizeof(float), len7);
    for (uint32_t v5 = 0; v5 < len7; v5 += 1)
    {
      at(float,at(struct array *,*out,v2),v5) = ((at(float,v29,v5) / v31) - (at(float,v23,v5) / v25));
    }
  }
  freeArray(v20);
  freeArray(e0);
  freeArray(v23);
  freeArray(v22);
  freeArray(v26);
  freeArray(e4);
  freeArray(v29);
  freeArray(v28);
}
