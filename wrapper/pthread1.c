
#include <assert.h>
#include <stdlib.h>

#include <pthread.h>
#include <semaphore.h>

int _pthread_create(pthread_t *thread, pthread_attr_t *attr, typeof(void *(void *)) *start_routine, void *arg) {
    return pthread_create(thread, attr, start_routine, arg);
}
int _pthread_join(pthread_t thread, void **retval) {
    return pthread_join(thread, retval);
}

size_t _pthread_mutex_init(pthread_mutexattr_t *attr) {
    pthread_mutex_t *mutex = malloc(sizeof(*mutex));
    int s = pthread_mutex_init(mutex, attr);
    assert(s == 0);
    return (size_t)mutex;
}
int _pthread_mutex_lock(size_t mutex) {
    return pthread_mutex_lock((pthread_mutex_t*)mutex);
}
int _pthread_mutex_unlock(size_t mutex) {
    return pthread_mutex_unlock((pthread_mutex_t*)mutex);
}
void _pthread_mutex_destroy(size_t mutex) {
    free((pthread_mutex_t*)mutex);
}

size_t _pthread_sem_init(int pshared, int value) {
    sem_t *sem = malloc(sizeof(*sem));
    int s = sem_init(sem, pshared, value);
    assert(s == 0);
    return (size_t)sem;
}
int _pthread_sem_wait(size_t sem) {
    return sem_wait((sem_t*)sem);
}
int _pthread_sem_post(size_t sem) {
    return sem_post((sem_t*)sem);
}
void _pthread_sem_destroy(size_t sem) {
    free((sem_t*)sem);
}

size_t _pthread_barrier_init(pthread_barrierattr_t *attr, unsigned count) {
    pthread_barrier_t *barrier = malloc(sizeof(*barrier));
    int s = pthread_barrier_init(barrier, attr, count);
    assert(s == 0);
    return (size_t)barrier;
}
int _pthread_barrier_wait(size_t barrier) {
    return pthread_barrier_wait((pthread_barrier_t*)barrier);
}
int _pthread_barrier_destroy(size_t barrier) {
    int s = pthread_barrier_destroy((pthread_barrier_t*)barrier);
    free((pthread_barrier_t*)barrier);
    return s;
}
