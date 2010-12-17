#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <SDL.h>
#include <GL/gl.h>

#include "panic.h"
#include "game.h"
#include "bullet.h"

static const char *WINDOW_CAPTION = "BULLET TEST";

static void
initialize_sdl(void)
{
	if (SDL_Init(SDL_INIT_VIDEO) < 0)
		panic("SDL_Init: %s", SDL_GetError());

	if (SDL_SetVideoMode(WINDOW_WIDTH, WINDOW_HEIGHT, 0, SDL_OPENGL) == NULL)
		panic("SDL_SetVideoMode: %s", SDL_GetError());

	SDL_WM_SetCaption(WINDOW_CAPTION, NULL);
}

static void
initialize_gl_state(void)
{	
	glViewport(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0.f, WINDOW_WIDTH, 0.f, WINDOW_HEIGHT, -1.f, 1.f);

	glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
}

static void
terminate_sdl(void)
{
	SDL_Quit();
}

static int running;

static void
handle_events(void)
{
	SDL_Event event;

	while (SDL_PollEvent(&event)) {
		switch (event.type) {
			case SDL_QUIT:
				running = 0;
				break;

			default:
				break;
		}
	}
}

static void
redraw(int lerp_tics)
{
	glClear(GL_COLOR_BUFFER_BIT);

	bullets_draw(lerp_tics);

	SDL_GL_SwapBuffers();
}

static void
update(void)
{
	bullets_update();
}

static void
event_loop(void)
{
	int last_update;
	const int update_interval = 1000/FPS;

	running = 1;
	last_update = SDL_GetTicks();

	while (running) {
		while (SDL_GetTicks() - last_update >= update_interval) {
			handle_events();
			update();
			last_update += update_interval;
		}

		redraw(SDL_GetTicks() - last_update);
	}
}

static void
initialize(void)
{
	initialize_sdl();
	initialize_gl_state();
	bullets_initialize();

	bullet_spawn(300, 200, "test");
}

static void
terminate(void)
{
	bullets_tear_down();
	terminate_sdl();
}

int
main(int argc, char *argv[])
{
	initialize();
	event_loop();
	terminate();

	return 0;
}
