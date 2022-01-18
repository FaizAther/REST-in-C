#include "commons.h"
#include "container.h"

typedef struct _Container {
    struct _Container   *prev;
    Thing               data;
    struct _Container   *next;
    bool                lock;
    Pos                 index;
} _Container;

inline Container
container_init(Thing t)
{
    Container c = (Container)malloc(sizeof(*c));
    if (c == NULL) {
        return c;
    }
    c->data = t;
    c->next = c->prev = NULL;
    c->lock = false;
    return c;
}

inline Thing
container_get_data(Container bucket)
{
    return bucket->data;
}

inline bool
container_set_next(Container to, Container bucket)
{
    if (to->lock)
        return false;
    to->next = bucket;
    bucket->prev = to;
    return true;
}

inline bool
container_set_prev(Container to, Container bucket)
{
    if (to->lock)
        return false;
    to->prev = bucket;
    bucket->next = to;
    return true;
}

inline Container
container_get_next(Container bucket)
{
    return bucket->next;
}

inline Container
container_get_prev(Container bucket)
{
    return bucket->prev;
}

inline bool
container_break(Container bucket)
{
    if (bucket->lock)
        return false;
    bucket->prev->next = bucket->next;
    bucket->next->prev = bucket->prev;
    return true;
}

inline bool
container_free(Container bucket, bool on)
{
    if (!thing_free(bucket->data, on))
        return false;
    free(bucket);
    return true;
}

inline bool
container_islocked(Container bucket)
{
    return bucket->lock;
}

inline bool
container_lock(Container bucket)
{
    if (container_islocked(bucket)) {
        return false;
    }
    bucket->lock = !bucket->lock;
    return true;
}

inline bool
container_unlock(Container c)
{
    c->lock = false;
    return true;
}