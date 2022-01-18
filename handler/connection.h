#ifndef CONNECTION_H
#define CONNECTION_H

#define DEFAULT_PORT	5000
#define DEFAULT_ADDRESS "0.0.0.0"
#define DEFAULT_BACKLOG 1024

typedef struct _Handler *Handler;

int
connection_init(unsigned int, const char *, unsigned int);

#endif //CONNECTION_H