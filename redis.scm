(module redis (redis-connect redis-command redis-command*)
(import chicken scheme extras foreign)
(use bind easyffi lolevel defstruct)

#>
#include "hiredis/hiredis.h"
<#

(defstruct redis-context ptr)

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

(define (process-hiredis-reply context-ptr reply)
 (define (map-n f n)
  (let loop ((i 0) (c '()))
   (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))
 (when reply
  (cond ((equal? redis-reply-string (redisReply-type reply))
         (redisReply-str reply))
        ((equal? redis-reply-array (redisReply-type reply))
         (map-n
           (lambda (i)
            (process-hiredis-reply
             context-ptr
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
         (unless (equal? (redisReply-str reply) "OK")
          (error "redis status" (redisReply-str reply))))
        ((equal? redis-reply-error (redisReply-type reply))
         (cond
          ((equal? redis-err-io (redisContext-err context-ptr))
           (error "redis I/O error"))
          ((equal? redis-err-eof (redisContext-err context-ptr))
           (error "redis unexpected end of file"))
          ((equal? redis-err-protocol (redisContext-err context-ptr))
           (error "redis protocol error"))
          ((equal? redis-err-oom (redisContext-err context-ptr))
           (error "redis out of memory"))
          ((equal? redis-err-other (redisContext-err context-ptr))
           (error (string-append "redis " (redisContext-errstr context-ptr))))
          (else (error "redis error" (redisReply-type reply)
                       (redisReply-str reply)))))
        (else (error "unexpected redis reply" (redisReply-type reply))))))

(define (redis-command context command)
 (unless (and (redis-context? context)
            (redis-context-ptr context))
  (error "invalid redis context"))
 (process-hiredis-reply
  (redis-context-ptr context)
  (set-finalizer! ((foreign-lambda (c-pointer "redisReply")
                                   "redisCommand"
                                   (c-pointer "redisContext")
                                   c-string)
                   (redis-context-ptr context)
                   command)
                  freeReplyObject)))

(define (redis-command* context command . arguments)
 (unless (and (redis-context? context)
            (redis-context-ptr context))
  (error "invalid redis context"))
 (process-hiredis-reply
  (redis-context-ptr context)
  (set-finalizer!
   ((foreign-lambda* (c-pointer "redisReply")
                     (((c-pointer "struct redisContext") redisContext)
                      (scheme-object strings)
                      (integer nr_of_strings))
                     "const char *arguments[nr_of_strings];"
                     "size_t sizes[nr_of_strings];"
                     "C_word head = strings;"
                     "int index = 0;"
                     "while(head != C_SCHEME_END_OF_LIST) {"
                     " C_word obj = C_u_i_car(head);"
                     " arguments[index] = C_c_string(obj);"
                     " sizes[index] = C_unfix(C_i_string_length(obj));"
                     " head = C_u_i_cdr(head);"
                     " index++;"
                     "}"
                     "C_return(redisCommandArgv(redisContext, nr_of_strings, arguments, sizes));")
    (redis-context-ptr context)
    (cons command arguments)
    (+ (length arguments) 1))
   freeReplyObject)))

(define (redis-connect hostname port)
 (let ((context (redisConnect hostname port)))
  (unless context (error "redis couldn't connect"))
  (set-finalizer! context redisFree)
  (when (> (redisContext-err context) 0)
   (error (string-append "redis " (redisContext-errstr context))))
  (make-redis-context ptr: context)))
)
