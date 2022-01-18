#include "connection.h"

#include <sys/select.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <arpa/inet.h>

#include <signal.h>

#include <sys/ioctl.h>
#include <sys/poll.h>

#define BUF_SIZ	1024 * 2

#define NKONNS	256

typedef enum {
	ALIVE	= 0,
	DIE		= (1 << 0),
	DEAD	= (1 << 1),
	BAD		= ~0
} KONNSTATUS;

typedef struct _PollHolder {
	struct pollfd 	*fds;
	unsigned int 	count;
	unsigned int 	curr;
	KONNSTATUS 		status;
	unsigned int	timeout;
} _PollHolder;

typedef struct _PollHolder *PollHolder;

typedef struct _Handler {
    int 				io;
	struct _PollHolder 	holders[NKONNS];
} _Handler;

#define READER(x) x.io

struct _Handler Konnection;

extern Handler Konnect;

void
connection_pre(void)
{
    Konnect = &Konnection;
}

void
exit_func ( int signo )
{
    fprintf(stderr, "\nGot signo %d, bye.\n\n", signo);

    close(READER(Konnection));

    exit(EXIT_FAILURE);
}

int
connection_init(unsigned int portnum, const char *address, \
	unsigned int backlog)
{
	int client = ~0,
		ret = ~0, on = 1;
	uint32_t count = 0;
	struct sockaddr_in ssock = {0}, 
		csock = {0};
	char buf[BUF_SIZ] = {0};
	socklen_t clen = sizeof(client); 

	bzero(Konnection.holders, NKONNS * sizeof(struct _PollHolder));
	Konnection.holders[0].curr = 0;
	Konnection.holders[0].count = 1;
	Konnection.holders[0].timeout = (3 * 60 * 1000);
	Konnection.holders[0].fds = \
		(struct pollfd *)malloc(sizeof(struct pollfd) * NKONNS);

	// hs_init(&argc, &argv);

	signal( SIGKILL, exit_func );
	signal( SIGTERM, exit_func );
	signal( SIGINT,  exit_func );

	READER(Konnection) = socket(AF_INET, SOCK_STREAM, 0);
	if (READER(Konnection) < 0) {
		fprintf(stderr, "socket: %d %s\n", READER(Konnection), strerror(errno));
		exit(EXIT_FAILURE);
	}

	ret = setsockopt(READER(Konnection), SOL_SOCKET,  SO_REUSEADDR, \
		(char *)&on, sizeof(on));
	if (ret < 0)
	{
		fprintf(stderr, "setsockopt: %d %s", ret, strerror(errno));
		goto BCLOSE;
	}

	ret = ioctl(READER(Konnection), FIONBIO, (char *)&on);
	if (ret < 0)
	{
		perror("ioctl() failed");
		goto BCLOSE;
	}

	bzero(&ssock, sizeof(ssock));
	ssock.sin_family = AF_INET;
	ssock.sin_port = htons(portnum);
	ssock.sin_addr.s_addr = inet_addr(address);

	ret = bind(READER(Konnection), (struct sockaddr *)&ssock, sizeof(ssock));
	if (ret < 0) {
		fprintf(stderr, "bind: %d %s\n", ret, strerror(errno));
		goto BCLOSE;
	}

	ret = listen(READER(Konnection), backlog);
	if (ret < 0) {
		fprintf(stderr, "listen: %d %s\n", ret, strerror(errno));
		goto BCLOSE;
	}

	Konnection.holders->fds[0].fd = READER(Konnection);
  	Konnection.holders->fds[0].events = POLLIN;

	while (1) {
		bzero(&csock, sizeof(csock));

		ret = poll(Konnection.holders->fds, Konnection.holders->count, Konnection.holders->timeout);
		if (ret < 0) {
			fprintf(stderr, "poll: %d %s\n", ret, strerror(errno));
			break;
		}

		client = accept(READER(Konnection), \
			(struct sockaddr *)&csock, &clen);
		if (client < 0) {
			fprintf(stderr, "accept: %d %s\n", client, strerror(errno));
			continue;
		}
// CRECV:
		bzero(buf, BUF_SIZ);
		ret = recv(client, buf, BUF_SIZ, 0);
		if (ret == -1) {
			switch (errno) {
				case EAGAIN:
					break;
				/*case EWOULDBLOCK:
					break;*/
				default:
					fprintf(stderr, "recv: %d %s\n", ret, strerror(errno));
					goto CCLOSE;
			}
		}
		printf("_____start{%u}\n", count);
		printf("%s\n", buf);
		fflush(stdout);
		clen = sizeof(client);
		// strncpy(buf, canvasStr(), BUF_SIZ);
// CSEND:
		ret = send(client, "ABCD", strlen("ABCDE") - 1, 0);
		if (ret == -1) {
			switch (errno) {
				case EAGAIN:
					break;
				/*case EWOULDBLOCK:
					break;*/
				default:
					fprintf(stderr, "send: %d %s\n", ret, strerror(errno));
					goto CCLOSE;
			}
		}
		printf("_____end{%uu}__wroten{%d}\n", count, ret);
		fflush(stdout);
CCLOSE:
		close(client);
		count++;
	}
// SCLOSE:
	close(READER(Konnection));

	return (EXIT_SUCCESS);
BCLOSE:
	close(READER(Konnection));

	return (EXIT_FAILURE);
}