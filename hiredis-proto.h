struct timeval {
  long tv_sec;
  long tv_usec;
};

___abstract struct redisContext {
    int err;
    char errstr[128];
};

___abstract struct redisReply {
    int type;
    long integer;
    int len;
    char *str;
    size_t elements;
    struct redisReply **element;
};

redisContext *redisConnect(const char *ip, int port);
void redisFree(redisContext *c);

int redisGetReply(redisContext *c, void **reply);
void freeReplyObject(void *reply);
