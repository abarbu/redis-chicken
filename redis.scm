(module redis (redis-connect redis-command)
(import chicken scheme extras foreign)
(use traversal bind easyffi lolevel)

#>
#include "hiredis/hiredis.h"
<#

(define redis-err-io (foreign-value "REDIS_ERR_IO" int))
(define redis-err-eof (foreign-value "REDIS_ERR_EOF" int))
(define redis-err-protocol (foreign-value "REDIS_ERR_PROTOCOL" int))
(define redis-err-oom (foreign-value "REDIS_ERR_OOM" int))
(define redis-err-other (foreign-value "REDIS_ERR_OTHER" int))

(define redis-reply-string (foreign-value "REDIS_REPLY_STRING" int))
(define redis-reply-array (foreign-value "REDIS_REPLY_ARRAY" int))
(define redis-reply-integer (foreign-value "REDIS_REPLY_INTEGER" int))
(define redis-reply-nil (foreign-value "REDIS_REPLY_NIL" int))
(define redis-reply-status (foreign-value "REDIS_REPLY_STATUS" int))
(define redis-reply-error (foreign-value "REDIS_REPLY_ERROR" int))

(bind "#include \"hiredis-proto.h\"")

(define (process-hiredis-reply context reply)
 (when reply
  (cond ((equal? redis-reply-string (redisReply-type reply))
         (redisReply-str reply))
        ((equal? redis-reply-array (redisReply-type reply))
         (map-n
           (lambda (i)
            (process-hiredis-reply
             context
             ((foreign-lambda* (c-pointer "redisReply")
                               (((c-pointer "redisReply") reply)
                                (integer i))
                               "C_return(reply->element[i]);")
              reply i)))
          (redisReply-elements reply)))
        ((equal? redis-reply-integer (redisReply-type reply))
         (redisReply-integer reply))
        ((equal? redis-reply-nil (redisReply-type reply)) #f)
        ((equal? redis-reply-status (redisReply-type reply))
         (error "redis status unimplemented"))
        ((equal? redis-reply-error (redisReply-type reply))
         (cond
          ((equal? redis-err-io (redisContext-err context))
           (error "redis I/O error"))
          ((equal? redis-err-eof (redisContext-err context))
           (error "redis unexpected end of file"))
          ((equal? redis-err-protocol (redisContext-err context))
           (error "redis protocol error"))
          ((equal? redis-err-oom (redisContext-err context))
           (error "redis out of memory"))
          ((equal? redis-err-other (redisContext-err context))
           (error (string-append "redis " (redisContext-errstr context))))
          (else (error "redis unknown error" (redisReply-type reply)))))
        (else (error "unexpected redis reply" (redisReply-type reply))))))

(define (redis-command context command)
 (process-hiredis-reply
  context
  (set-finalizer! ((foreign-lambda (c-pointer "redisReply")
                                   "redisCommand"
                                   (c-pointer "redisContext")
                                   c-string)
                   context
                   command) 
                  freeReplyObject)))

(define (redis-connect hostname port)
 (let ((context (redisConnect hostname port)))
  (unless context (error "redis couldn't connect"))
  (set-finalizer! context redisFree)
  (when (> (redisContext-err context) 0)
   (error (string-append "redis " (redisContext-errstr context))))
  context))
)
