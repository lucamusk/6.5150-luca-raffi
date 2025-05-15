#include <assert.h>
#include <gc.h>
#include <gc/gc.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

typedef struct Shape {
  volatile size_t *indicies;
  size_t rank;
} Shape;

typedef struct Array Array;

#define for_each(var, low, high) for (size_t var = low; var < high; var++)

#define SHAPE(the_rank_x, ...)                                                 \
  ({                                                                           \
    Shape s;                                                                   \
    s.rank = the_rank_x;                                                      \
    size_t lengths[the_rank_x] = {__VA_ARGS__};                                \
    s.indicies = GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sizeof(size_t) * s.rank);     \
    memcpy((void*) s.indicies, lengths, sizeof(size_t) * the_rank_x);         \
    s;                                                                         \
  })

void shape_init(volatile Shape *shape, size_t rank, volatile size_t lengths[]) {
  shape->rank = rank;
  shape->indicies = GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sizeof(size_t) * rank);

  memcpy((void *)shape->indicies, (size_t *)lengths, sizeof(size_t) * rank);
}

size_t shape_size(Shape shape) {
  size_t count = 1;

  for_each(i, 0, shape.rank) count *= shape.indicies[i];

  return count;
}

typedef struct Array {
  union {
    volatile double *ptr;
    double singleton;
  } contents;
  Shape shape;
  size_t start;
} Array;

void array_init(Array *a, Shape shape) {
  a->shape = shape;
  if (shape.rank == 0) {
    a->contents.singleton = 0;
  } else {
    size_t sz = shape_size(shape);
    a->contents.ptr =
        GC_MALLOC(sizeof(size_t) * shape_size(shape));
  }
  a->start = 0;
}

Array *array_singleton(double v) {
  Array *new = GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sizeof(Array));
  new->contents.singleton = v;
  new->shape = (Shape){NULL, 0};
  new->start = 0;
  return new;
}

size_t array_rank(Array *a) { return a->shape.rank; }

Array *array_length(Array *a) {
  assert(array_rank(a) > 0);
  return array_singleton(a->shape.indicies[array_rank(a) - 1]);
}

Array *array_slice(Array *obj, Shape shape) {
  size_t start = 0;
  Array *a = obj;
  Array *subarray = GC_MALLOC_IGNORE_OFF_PAGE(sizeof(Array));

  for_each(index, 0, shape.rank) {
    size_t element_size = 1;

    for_each(j, index + 1, shape.rank) element_size *= a->shape.indicies[j];
    start +=
        element_size * (shape.indicies[index] % (a->shape.indicies[index]));
  }

  shape_init(&(subarray->shape), array_rank(a) - shape.rank,
             &(a->shape.indicies[shape.rank]));

  subarray->contents = a->contents;
  subarray->start = start;

  return subarray;
}

Array *array_ref(Array *obj, Shape s) { return array_slice(obj, s); }

double array_singleton_ref(Array *obj) {
  assert(array_rank(obj) == 0);
  return obj->contents.singleton;
}

void array_set_helper(Array *arr, size_t dim, size_t start, double val) {
  size_t element_size = 1;
  if (dim == array_rank(arr)) {
    arr->contents.ptr[start] = val;
  } else {
    for_each(idx, 0, arr->shape.indicies[dim]) {
      for_each(k, dim + 1, array_rank(arr)) { element_size *= k; }

      array_set_helper(arr, dim + 1, start + idx * element_size, val);
    }
  }
}

void array_set(Array *arr, Shape indicies, Array *value) {
  assert(array_rank(value) == 0);
  double val = value->contents.singleton;
  Array *subarray = array_slice(arr, indicies);

  Shape shape = subarray->shape;
  size_t start = subarray->start;


  if (array_rank(subarray)) {
    subarray->contents.singleton = val;
  } else {
    array_set_helper(subarray, 0, start, val);
  }
}

void print_array(Array *obj) {
  if (array_rank(obj) == 0) {
    printf("%f\n", obj->contents.singleton);
  } else {
    printf("(array (");
    for_each(idx, 0, array_rank(obj)) {
      printf("%lu ", obj->shape.indicies[idx]);
    }

    printf(") (");

    for_each(idx, 0, shape_size(obj->shape)) {
      printf("%f ", obj->contents.ptr[idx]);
    }

    puts("))");
  }
}

#define DEF_BINOP(name, op)                                                    \
  Array *name(Array *left, Array *right) {                              \
    assert(array_rank(left) == 0);                                             \
    assert(array_rank(right) == 0);                                            \
    double new = left->contents.singleton op right->contents.singleton;        \
    return array_singleton(new);                                               \
  }

DEF_BINOP(add, +);
DEF_BINOP(sub, -);
DEF_BINOP(mul, *);
DEF_BINOP(div, /);

Array *min(Array *left, Array *right) {
  assert(array_rank(left) == 0);
  assert(array_rank(right) == 0);
  double new = MIN(left->contents.singleton, right->contents.singleton);
  return array_singleton(new);
}

Array *max(Array *left, Array *right) {
  assert(array_rank(left) == 0);
  assert(array_rank(right) == 0);
  double new = MAX(left->contents.singleton, right->contents.singleton);
  return array_singleton(new);
}

Array *scary_program();

int main() {
  GC_INIT();
  print_array(scary_program());
}
