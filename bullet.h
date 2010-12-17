#ifndef BULLET_H_
#define BULLET_H_

void
bullets_initialize(void);

void
bullets_tear_down(void);

void
bullets_draw(int lerp_tics);

void
bullets_update(void);

void
bullet_spawn(float x, float y, const char *name);

#endif /* BULLET_H */
