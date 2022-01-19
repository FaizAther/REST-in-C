#include <stdio.h>

#include "commons.h"
#include "thing.h"
#include "container.h"
#include "storage.h"
#include "connection.h"

#include <string.h>

Handler Konnect;

int
main(int argc, char **argv)
{
    (void)argc;
    unsigned int i = 0;
    // printf("%s\n", connection_version());
    while (argv[i]) {
        printf("argv[i] => {%s}\n", argv[i]);
        i += 1;
    }

    char random[] = "{n}, Hello, World\n";

    Storage s = storage_init();

    char d0[50] = {0};
    random[1] = '0';
    strncpy(d0, random, 19);

    storage_add(s, d0);

    Container ret = storage_find(s, 0);

    Thing ret0 = container_get_data(ret);

    Item ret1 = thing_get(ret0);

    printf("%s", (char *)ret1);

    // int ret = connection_init(DEFAULT_PORT, DEFAULT_ADDRESS, DEFAULT_BACKLOG);

    // ret = printf("connection_init %d\n", ret);

    // storage_destroy();

    return (0);
}