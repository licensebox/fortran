/* Handle ownership and serialization only; all licensing behavior is native. */
#define _POSIX_C_SOURCE 200809L
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <licensebox_json.h>
typedef struct {
 pthread_mutex_t mutex;
 size_t references;
 int busy;
 lb_client *client;
} owner;
static void err(lb_result r, lb_client *c, char **e) {
 const char *code=lb_result_code(r);
 if(c && r!=LB_INVALID_ARGUMENT && strcmp(lb_error_code(c),"ok")) code=lb_error_code(c);
 *e=strdup(code);
}
int lbf_create(const char *options,const lb_callbacks *callbacks,void **output,char **error) {
 *output=NULL; *error=NULL;
 owner *o=calloc(1,sizeof(*o)); if(!o) return LB_IO_ERROR;
 pthread_mutexattr_t a; pthread_mutexattr_init(&a);
 pthread_mutexattr_settype(&a,PTHREAD_MUTEX_RECURSIVE);
 pthread_mutex_init(&o->mutex,&a); pthread_mutexattr_destroy(&a);
 o->references=1;
 int r=lb_client_create_json(options,callbacks,&o->client);
 if(r) {err(r,NULL,error);pthread_mutex_destroy(&o->mutex);free(o);return r;}
 *output=o;return 0;
}
void lbf_retain(owner *o) {if(!o)return;pthread_mutex_lock(&o->mutex);o->references++;pthread_mutex_unlock(&o->mutex);}
void lbf_release(owner *o) {
 if(!o)return;pthread_mutex_lock(&o->mutex);
 if(--o->references) {pthread_mutex_unlock(&o->mutex);return;}
 lb_client_destroy(o->client);pthread_mutex_unlock(&o->mutex);pthread_mutex_destroy(&o->mutex);free(o);
}
int lbf_close(owner *o) {
 if(!o)return 0;pthread_mutex_lock(&o->mutex);
 if(o->busy){pthread_mutex_unlock(&o->mutex);return LB_INVALID_ARGUMENT;}
 lb_client_destroy(o->client);o->client=NULL;pthread_mutex_unlock(&o->mutex);return 0;
}
int lbf_call(owner *o,const char *operation,const char *args,char **output,char **error) {
 *output=NULL;*error=NULL;
 if(!o){err(LB_INVALID_ARGUMENT,NULL,error);return LB_INVALID_ARGUMENT;}
 pthread_mutex_lock(&o->mutex);
 if(o->busy||!o->client){err(LB_INVALID_ARGUMENT,NULL,error);pthread_mutex_unlock(&o->mutex);return LB_INVALID_ARGUMENT;}
 o->busy=1;int r=lb_client_call_json(o->client,operation,args,output);
 if(r)err(r,o->client,error);
 o->busy=0;pthread_mutex_unlock(&o->mutex);return r;
}
