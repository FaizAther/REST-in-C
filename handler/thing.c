#include "thing.h"

#include <stdio.h>

typedef struct _Thing {
    Item    data;
    bool    lock;
} _Thing;

bool
thing_is(Thing t0, Thing t1)
{
    return t0 != NULL && t1 != NULL && \
        t0->data == t1->data;
}

Item
thing_get(Thing t)
{
    if (t != NULL)
        return t->data;
    return NULL;
}

Thing
thing_init(Item i)
{
    Thing t = (Thing)malloc(sizeof(*t));
    assert(t != NULL);
    t->data = i;
    t->lock = false;
    return t;
}

inline bool
thing_free(Thing t, bool on)
{
    if (t->lock)
        return false;
    if (on) {
        free(t->data);
    }
    t->data = NULL;
    free(t);
    return true;
}

size_t
thing_show(Thing t, char *buf, size_t len, \
    size_t (data_show(char *, size_t, Item)))
{
    int wrote = 0;
    wrote += snprintf(buf, len, "Thing{lock=%d, data=", t->lock);
    wrote += data_show(buf + wrote, len - wrote, t->data);
    wrote += snprintf(buf + wrote, len - wrote, "}");
    return wrote;
}