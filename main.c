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

#include "Thing_stub.h"

char sample_response[] = \
	"HTTP/1.1 200 OK\r\n"
	"Date: Mon, 27 Jul 2009 12:28:53 GMT\r\n"
	"Server: Apache\r\n"
	"Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r\n"
	"Vary: Accept-Encoding\r\n"
	"Content-Type: application/vnd.api+json\r\n"
	"\r\n"
	"{"
	"\"data\": [{"
	"\"type\": \"articles\","
	"\"id\": \"1\","
	"\"attributes\": {"
	"\"title\": \"JSON:API paints my bikeshed!\","
	"\"body\": \"The shortest article. Ever.\","
	"\"created\": \"2015-05-22T14:56:29.000Z\","
	"\"updated\": \"2015-05-22T14:56:28.000Z\""
	"},"
	"\"relationships\": {"
	"\"author\": {"
	"\"data\": {\"id\": \"42\", \"type\": \"people\"}"
	"}"
	"}"
	"}],"
	"\"included\": ["
	"{"
	"\"type\": \"people\","
	"\"id\": \"42\","
	"\"attributes\": {"
	"\"name\": \"John\","
	"\"age\": 80,"
	"\"gender\": \"male\""
	"}"
	"}"
	"]"
	"}"
	"\r\n";

#define BUF_SIZ	1024 * 2

#define PORTNUM	5001

int sock = ~0;

void
exit_func ( int signo )
{
    fprintf( stderr, "\n got signo %d, bye.\n\n", signo );

    close(sock);

    exit( 1 );
}

int
main(int argc, char *argv[])
{
	int client = ~0,
		ret = ~0;
	struct sockaddr_in ssock = {0}, 
		csock = {0};
	char buf[BUF_SIZ] = {0};
	socklen_t clen = sizeof(client); 

	hs_init(&argc, &argv);

	signal( SIGKILL, exit_func );
	signal( SIGTERM, exit_func );
	signal( SIGINT,  exit_func );

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0) {
		fprintf(stderr, "socket: %d %s\n", sock, strerror(errno));
		exit(EXIT_FAILURE);
	}

	bzero(&ssock, sizeof(ssock));
	ssock.sin_family = AF_INET;
	ssock.sin_port = htons(PORTNUM);
	ssock.sin_addr.s_addr = inet_addr("0.0.0.0");

	ret = bind(sock, (struct sockaddr *)&ssock, sizeof(ssock));
	if (ret < 0) {
		fprintf(stderr, "connect: %d %s\n", connect, strerror(errno));
		exit(EXIT_FAILURE);
	}

	ret = listen(sock, 50);
	if (ret < 0) {
		fprintf(stderr, "listen: %d %s\n", connect, strerror(errno));
		exit(EXIT_FAILURE);
	}

	while (1) {
		bzero(&csock, sizeof(csock));

		client = accept(sock, \
			(struct sockaddr *)&csock, &clen);
		if (client < 0) {
			fprintf(stderr, "accept: %d %s\n", client, strerror(errno));
			continue;
		}
		bzero(buf, BUF_SIZ);
		recv(client, buf, BUF_SIZ, 0);
		printf("%s\n", buf);
		fflush(stdout);
		printf("Got 1\n");
		clen = sizeof(client);
		strncpy(buf, canvasStr(), BUF_SIZ);
		ret = send(client, buf, strlen(buf) - 1, 0);
		printf("wrote: %d\n", ret);
		fflush(stdout);
		close(client);
	}

	close(sock);

	return (0);
}