#ifndef THING_H
#define THING_H

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "thing.h"

typedef void *Item;

typedef struct _Thing *Thing;

bool
thing_is(Thing, Thing);

Item
thing_get(Thing);

Thing
thing_init(Item);

bool
thing_free(Thing, bool);

size_t
thing_show(Thing, char *, \
    size_t, size_t(*)(char *, size_t, Item));

#endif //THING_H