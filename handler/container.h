#ifndef CONTAINER_H
#define CONTAINER_H

#include <stdlib.h>

#include "thing.h"

typedef struct _Container *Container;

Container
container_init(Thing);

Thing
container_get_data(Container);

bool
container_set_next(Container, Container);

bool
container_set_prev(Container, Container);

Container
container_get_next(Container);

Container
container_get_prev(Container);

bool
container_break(Container);

bool
container_free(Container, bool);

bool
container_islocked(Container);

bool
container_lock(Container);

bool
container_unlock(Container);

#endif //CONTAINER_H