#include <iostream>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <linux/icmp.h>
#include <linux/ip.h>
#include <unistd.h>


#include "util.h"

uint32_t get_time() {
    timeval tv;
    uint32_t res = -1;

    if (gettimeofday(&tv, NULL) == 0) {
        res = ((tv.tv_sec % 86400) * 1000 + tv.tv_usec / 1000);
    }
    else {
        perror("gettimeofday");
        exit(EXIT_FAILURE);
    }

    return res;
}

struct icmp_timestamp_mgs {
    icmphdr hdr;
    __be32 originate;
    __be32 receive;
    __be32 transmit;
};

int main(int argc, char** argv) {
    if(argc < 2) {
        std::cerr << "Specify destination ip address" << std::endl;
        exit(EXIT_FAILURE);
    }
    
    std::string dest_addr = argv[1];

    uint32_t my_time = get_time();

    icmp_timestamp_mgs msg;
    msg.hdr.type = ICMP_TIMESTAMP;
    msg.hdr.code = 0;
    msg.hdr.checksum = 0;
    msg.hdr.un.echo.id = 0;
    msg.hdr.un.echo.sequence = 0;
    msg.originate = htonl(my_time);
    msg.receive = 0;
    msg.transmit = 0;
    msg.hdr.checksum = in_checksum((uint16_t*)&msg, sizeof(msg)); 

    int sockfd;
    if ((sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP)) == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    sockaddr_in dest;
    dest.sin_family = AF_INET;
    dest.sin_addr.s_addr = inet_addr(dest_addr.c_str());

    int send_cnt = sendto(sockfd, &msg, sizeof(msg), 0, (sockaddr*)&dest, sizeof(sockaddr));
    if(send_cnt == -1) {
        perror("send");
        exit(EXIT_FAILURE);
    }

    if(send_cnt != sizeof(msg)) {
        fprintf(stderr, "failed to send full packet\n");
        exit(EXIT_FAILURE);
    }

    // set timeout
    timeval tv;
    tv.tv_sec = 20;
    tv.tv_usec = 0;
    setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    
    char buffer[sizeof(iphdr) + sizeof(icmp_timestamp_mgs)];
    socklen_t addrlen = sizeof(dest);

    int recv_cnt = recvfrom(sockfd, &buffer, sizeof(buffer), 0, (sockaddr*)&dest, &addrlen);

    if(recv_cnt == -1) {
        perror("recv");
        exit(EXIT_FAILURE);
    }

    if(recv_cnt != sizeof(buffer)) {
        fprintf(stderr, "failed to receive full packet\n");
        exit(EXIT_FAILURE);
    }

    icmp_timestamp_mgs &answer = *(icmp_timestamp_mgs*)(buffer + sizeof(iphdr)); 

    if(answer.hdr.type != ICMP_TIMESTAMPREPLY) {
        fprintf(stderr, "received wrong message\n");
        exit(EXIT_FAILURE);
    }

    std::cout << "My time: " << my_time << std::endl;
    std::cout << "Dest time: " << ntohl(answer.receive) << std::endl;

    int64_t delta = (int64_t) my_time - ntohl(answer.receive);
    std::cout << "Time delta: " << delta / 1000 << "s " << abs(delta) % 1000 << "ms" << std::endl;

    close(sockfd);
    return 0;
}
