#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/epoll.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_EVENTS	10
#define BUF_SIZ		50

typedef struct msgg {
	char buf[BUF_SIZ];
	unsigned int pos;
} msgg_t;

int glob_sock = ~0;

void
exit_func ( int signo )
{
    fprintf(stderr, "\nGot signo %d, bye.\n\n", signo);

    close(glob_sock);

    exit(EXIT_FAILURE);
}

int
main(void)
{
	struct epoll_event ev = {0}, \
		events[MAX_EVENTS] = {0};
	int listen_sock = -1, conn_sock = -1, \
		nfds = -1, epollfd = -1, ret = -1, on = 1;

	struct sockaddr_in my_addr = {0};
	struct sockaddr addr = {0};
	socklen_t addrlen;

	signal( SIGKILL, exit_func );
	signal( SIGTERM, exit_func );
	signal( SIGINT,  exit_func );

	glob_sock = listen_sock = socket(AF_INET, SOCK_STREAM, 0);	
	if (listen_sock < 0) {
		fprintf(stderr, "socket: %d %s", errno, strerror(errno));
		goto bad;
	}

	ret = setsockopt(listen_sock, SOL_SOCKET, SO_REUSEADDR, \
		&on, sizeof(on));
	if (ret < 0) {
		fprintf(stderr, "setsockopt: %d %s", errno, strerror(errno));
		goto bad;
	}

	ret = ioctl(listen_sock, FIONBIO, (char *)&on);
	if (ret < 0) {
		fprintf(stderr, "ioctl: %d %s", errno, strerror(errno));
		goto bad;
	}

	my_addr.sin_family = AF_INET;
	my_addr.sin_port = htons(5000);

	ret = bind(listen_sock, (struct sockaddr *)&my_addr, \
		sizeof(my_addr));
	if (ret < 0) {
		fprintf(stderr, "bind: %d %s", errno, strerror(errno));
		goto bad;
	}

	ret = listen(listen_sock, 50);
	if (ret < 0) {
		fprintf(stderr, "listen: %d %s", errno, strerror(errno));
		goto bad;
	}
	
	epollfd = epoll_create1(0);
	if (epollfd == -1) {
		fprintf(stderr, "epoll_create: %d %s", errno, strerror(errno));
		goto worse;
	}

	ev.events = EPOLLIN;
	ev.data.fd = listen_sock;

	if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1) {
		fprintf(stderr, "epoll_ctl: %d %s", errno, strerror(errno));
		goto bad;
	}

	msgg_t msggs[MAX_EVENTS];
	bzero(msggs, sizeof(msgg_t) * MAX_EVENTS);
	for (;;) {
		nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
		if (nfds == -1) {
			fprintf(stderr, "epoll_wait: %d %s", errno, strerror(errno));
			goto bad;
		}
		for (int n = 0; n < nfds; ++n) {
			addrlen = sizeof(struct sockaddr);
			if (events[n].data.fd == listen_sock) {
				conn_sock = accept(listen_sock, \
					(struct sockaddr *)&addr, &addrlen);
				if (conn_sock < 0) {
					if (errno == EWOULDBLOCK) {
						continue;
					}
					fprintf(stderr, "accept: %d %s", errno, strerror(errno));
					goto bad;
				}
				int flags = fcntl(conn_sock, F_GETFL, 0);
				if (flags < 0) {
					close(conn_sock);
					continue;
				}
				ret = fcntl(conn_sock, F_SETFL, flags | O_NONBLOCK);
				if (ret < 0) {
					fprintf(stderr, "fcntl: %d %s", errno, strerror(errno));
					close(conn_sock);
					goto bad;
				}
				ev.events = EPOLLIN/* | EPOLLET */;
				ev.data.fd = conn_sock;
				if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_sock, \
					&ev) == -1) {
					fprintf(stderr, "epoll_ctl: %d %s", errno, strerror(errno));
					close(conn_sock);
					goto bad;
				}
			} else {
				switch (events[n].events) {
					case EPOLLIN:
						if (msggs[n].pos == 0) {
							bzero(msggs[n].buf, BUF_SIZ);
						}
						ret = recv(events[n].data.fd, \
							msggs[n].buf + msggs[n].pos, BUF_SIZ - 1, MSG_DONTWAIT);
						if (ret < 0) {
							switch (errno) {
								case EWOULDBLOCK:
									continue;
									break;
								default:
									goto remove_it;
							}
						}
						msggs[n].pos += ret;
						if (msggs[n].pos < BUF_SIZ - 1) {
							continue;
						} else {
							msggs[n].pos = 0;
						}
						ev.events = EPOLLOUT;
						ev.data.fd = events[n].data.fd;
						epoll_ctl(epollfd, EPOLL_CTL_MOD, events[n].data.fd,
								&ev);
						break;
					case EPOLLOUT:
						ret = send(events[n].data.fd, \
							msggs[n].buf + msggs[n].pos, BUF_SIZ - 1, MSG_DONTWAIT);
						if (ret < 0) {
							switch (errno) {
								case EWOULDBLOCK:
									continue;
									break;
								default:
									goto remove_it;
							}
						}
						msggs[n].pos += ret;
						if (msggs[n].pos < BUF_SIZ - 1) {
							continue;
						} else {
							msggs[n].pos = 0;
						}
						ev.events = EPOLLIN;
						ev.data.fd = events[n].data.fd;
						epoll_ctl(epollfd, EPOLL_CTL_MOD, events[n].data.fd,
								&ev);
						break;
					case EPOLLET:
						break;
					default:
remove_it:
						close(events[n].data.fd);
						epoll_ctl(epollfd, EPOLL_CTL_DEL, events[n].data.fd, \
							&events[n]);
				}
			}
		}
	}

	close(listen_sock);
	return (EXIT_SUCCESS);

worse:
	close(listen_sock);
bad:
	return (EXIT_FAILURE);
}
