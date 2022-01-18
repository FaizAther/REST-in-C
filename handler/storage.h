#ifndef STORAGE_H
#define STORAGE_H

#include "commons.h"
#include "thing.h"

typedef struct _Storage *Storage;

bool
storage_decrement(Storage);

bool
storage_increment(Storage);

Storage
storage_init(void);

bool
storage_add(Storage, Item);

bool
storage_remove(Storage, Pos, bool);

Size
storage_size(Storage);

bool
storage_islocked(Storage);

bool
storage_lock(Storage);

bool
storage_unlock(Storage);

#endif //STORAGE_H