#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "commons.h"
#include "container.h"

typedef struct _Storage {
    Container   start;
    Container   mid;
    Container   cursor;
    Size        length;
    bool        lock;
    bool        move;
} _Storage;

#include "storage.h"

inline Storage
storage_init(void)
{
    Storage s = (Storage)malloc(sizeof(*s));
    assert(s != NULL);
    s->start = s->mid = s->cursor = NULL;
    s->length = 0;
    s->lock = false;
    s->move = false;
    return s;
}

inline bool
storage_decrement(Storage bucket)
{
    if (bucket->lock) {
        return false;
    }

    bucket->length -= 1;
    return true;
}

inline bool
storage_increment(Storage bucket)
{
    if (bucket->lock) {
        return false;
    }
    bucket->length += 1;
    return true;
}

inline Size
storage_size(Storage bucket)
{
    return bucket->length;
}

bool
storage_add(Storage bucket, Item item)
{
    if (bucket->lock) {
        return false;
    }

    return container_set_next(\
        bucket->cursor, container_init(thing_init(item))) && \
        storage_decrement(bucket);
}

Container
storage_find(Storage bucket, Pos index)
{
    unsigned int count = 0;
    for (Container c = bucket->start; c != NULL; c = container_get_next(c)) {
        if (index != count) {
            count += 1;
            continue;
        }
        return c;
    }
    return NULL;
}

bool
storage_remove(Storage bucket, Pos index, bool on)
{
    Container find = storage_find(bucket, index);
    if (!find || container_islocked(find)) {
        return false;
    }

    return container_break(find) && \
        container_free(find, on);
}

inline bool
storage_islocked(Storage s)
{
    return s->lock;
}

inline bool
storage_lock(Storage s)
{
    if (storage_islocked(s))
        return false;
    s->lock = true;
    return true;
}

inline bool
storage_unlock(Storage s)
{
    s->lock = false;
    return true;
}