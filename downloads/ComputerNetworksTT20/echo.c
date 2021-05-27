#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <string.h>
  
/*****************************************************************************/
/* MAINTAINABILITY                                                           */
/* How can we enable the user to modify the port number (and if you're keen, */
/* the buffer size) without having to recompile the program?                 */
/*****************************************************************************/

#define PORT    56789           /* default port number for the echo service */
#define BSIZE   11              /* artificially short buffer size           */

#define PROMPT  "Danlei Zhu - Networks Practical Echo Server\n"
#define QUIT    ".\r\n"
#define ENDLN   '\n'

#define TRUE    (0==0)

/*****************************************************************************/
/* MAINTAINABILITY                                                           */
/* How will the user know what this function does?                           */
/*****************************************************************************/

int main(int argc, char *argv[]) {

    int sock;                   /* file descriptor for the server socket */
    struct sockaddr_in server;

    char buf[BSIZE];
    
    /* 1. Create socket*/

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
                /*************************************************************/
                /* ARGUMENTS look at "man 2 socket" for information          */
                /*************************************************************/
        perror("cannot open a socket");
        exit(EXIT_FAILURE);
    };

    /* 2. Bind an address at the socket*/

    server.sin_family = AF_INET;                  /********************************/
    server.sin_addr.s_addr = INADDR_ANY;             /* ARGUMENT: see "man 2 bind"   */
    server.sin_port = htons(PORT);                    /********************************/

    if (bind(sock, (struct sockaddr *) &server, sizeof(server)) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    };

    /* 3. Accept connections*/
                                             /********************************/
    if (listen(sock, BSIZE) <0) {               /* ARGUMENT: see "man 2 listen" */
                                             /********************************/
        perror("listen");
        exit(EXIT_FAILURE);
    };

    int count = 1;

    while (TRUE) {

    /* 4. Wait for client requests*/
       	
        struct sockaddr_in client;
        socklen_t client_len = sizeof(client);
        int stream = accept(sock, (struct sockaddr *) &client, &client_len);

	int size = 100;

        /*********************************************************************/
        /* ERROR HANDLING                                                    */
        /*      is "stream" a valid socket descriptor here?                  */
        /*      can anything go wrong in an of the code that follows?        */
        /*********************************************************************/

        send(stream, PROMPT, sizeof(PROMPT), 0);

        while (size != 0) { //recv is 0 if the client quits so  we stop the while loop when quit
		size = recv(stream, buf, BSIZE, 0);
        	buf[size] = '\0';                    /* null-termination for strings */
        	fprintf(stderr, "[%d] Text received: %s\n", count, buf);
        	send(stream, buf, BSIZE, 0);

	}; //End inner while

	count = count + 1;                             /********************************/
        close(stream);
    
    }; /* while(TRUE) */

}; /* main */
