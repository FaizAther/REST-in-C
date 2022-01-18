#include <stdio.h>

#include "commons.h"
#include "thing.h"
#include "container.h"
#include "storage.h"
#include "connection.h"

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

    int ret = connection_init(DEFAULT_PORT, DEFAULT_ADDRESS, DEFAULT_BACKLOG);

    ret = printf("connection_init %d\n", ret);

    return (0);
}