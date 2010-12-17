#include <cassert>
#include <cmath>
#include <map>

extern "C" {
#include <GL/gl.h>

#include "game.h"
#include "panic.h"
#include "gl_util.h"
#include "bullet.h"
}
#include "icode.h"

enum {
	BULLET_TEXTURE_WIDTH = 128,
	BULLET_TEXTURE_HEIGHT = 64,

	MAX_BULLETS = 1200,
	NUM_BULLET_TYPES = 4,
};

static struct bullet_info {
	int texture_width, texture_height;
	int width, height;
	float u, v;
	float du, dv;
} bullet_infos[NUM_BULLET_TYPES] = {
	{ 16, 48, 12, 30 },
	{ 16, 48, 12, 30 },
	{ 30, 30, 40, 40 },
	{ 30, 30, 30, 30 },
};

using namespace bc;

static ProgramCode *bullet_code;

struct gl_vertex {
	GLfloat position[2];
	GLfloat texuv[2];
};

static gl_vertex gl_vertex_array[MAX_BULLETS*4];
static unsigned num_gl_vertices;

static int bullet_texture_id;

struct vector2 {
	float x, y;

	vector2(float x_ = 0, float y_ = 0)
	: x(x_), y(y_)
	{ }

	vector2
	operator+(const vector2& v) const
	{
		return vector2(x + v.x, y + v.y);
	}

	vector2&
	operator+=(const vector2& v)
	{
		x += v.x;
		y += v.y;
		return *this;
	}

	vector2
	operator-(const vector2& v) const
	{
		return vector2(x - v.x, y - v.y);
	}

	vector2&
	operator-=(const vector2& v)
	{
		x -= v.x;
		y -= v.y;
		return *this;
	}

	vector2
	operator*(float s)
	{
		return vector2(x*s, y*s);
	}

	vector2&
	operator*=(float s)
	{
		x *= s;
		y *= s;
		return *this;
	}
};

struct bullet {
	bool used;
	vector2 position;
	vector2 direction;
	float speed;
	ExecuteCtx *script_ctx;
	int type;

	void initialize(float x, float y, const char *class_name);
	void initialize(ExecuteCtx *parent_ctx, ExecuteCtx *ctx);
	void release();
	void update();
	void update_direction();
	void draw(float dt) const;
};

static bullet bullets[MAX_BULLETS];

static std::map<ExecuteCtx *, bullet *> ctx_bullet_map;

static bullet *
find_unused_bullet()
{
	for (bullet *p = bullets; p != &bullets[MAX_BULLETS]; p++) {
		if (!p->used)
			return p;
	}

	return 0;
}

struct bullet_listener : SpawnListener {
	void operator()(ExecuteCtx *parent, ExecuteCtx *child) {
		bullet *p = find_unused_bullet();

		if (!p) {
			delete child;
		} else {
			p->initialize(parent, child);
		}
	}
};

static bullet_listener bullet_spawn_listener;

void
bullet::initialize(float x, float y, const char *class_name)
{
	assert(!used);

	used = true;

	position.x = x;
	position.y = y;

	script_ctx = bullet_code->get_execute_ctx(class_name, bullet_spawn_listener);
	ctx_bullet_map[script_ctx] = this;

	script_ctx->prepare("INIT");
	if (script_ctx->execute())
		panic("invalid state after %s::initialize call", class_name);
	update_direction();

	script_ctx->prepare("run");
}

void
bullet::initialize(ExecuteCtx *parent_ctx, ExecuteCtx *ctx)
{
	assert(!used);

	used = true;

	script_ctx = ctx;
	ctx_bullet_map[script_ctx] = this;

	position = ctx_bullet_map[parent_ctx]->position;
	update_direction();

	script_ctx->prepare("run");
}

void
bullet::release()
{
	assert(used);

	ctx_bullet_map.erase(script_ctx);
	delete script_ctx;

	used = false;
}

void
bullet::update_direction()
{
	assert(used);

	const Data *angle_ptr = script_ctx->get_field("angle");
	if (angle_ptr == 0)
		panic("`angle' field not defined in class");

	const Data *speed_ptr = script_ctx->get_field("speed");
	if (speed_ptr == 0)
		panic("`speed' field not defined in class");

	const Data *type_ptr = script_ctx->get_field("type");
	if (type_ptr == 0)
		panic("`type' field not defined in class");

	float angle_rads = M_PI*angle_ptr->float_val/180.;

	direction.x = cos(angle_rads);
	direction.y = sin(angle_rads);

	speed = speed_ptr->float_val;

	type = type_ptr->int_val;
}

void
bullet::update()
{
	assert(used);

	position += direction*speed;

	if (position.x < 0 || position.x >= WINDOW_WIDTH ||
	  position.y < 0 || position.y >= WINDOW_HEIGHT) {
		release();
		return;
	}

	if (!script_ctx->execute()) {
		release();
		return;
	}

	update_direction();
}

void
bullet::draw(float dt) const
{
	vector2 v = direction;
	vector2 u(-direction.y, direction.x);

	const bullet_info& bi = bullet_infos[type];

	vector2 p0 = position - u*.5*bi.width + v*.5*bi.height;
	vector2 p1 = position + u*.5*bi.width + v*.5*bi.height;
	vector2 p2 = position + u*.5*bi.width - v*.5*bi.height;
	vector2 p3 = position - u*.5*bi.width - v*.5*bi.height;

#define EMIT_VERTEX(pos, u, v) \
	{ \
	assert(num_gl_vertices < sizeof gl_vertex_array/sizeof *gl_vertex_array); \
	gl_vertex *p = &gl_vertex_array[num_gl_vertices++]; \
	p->position[0] = pos.x; \
	p->position[1] = pos.y; \
	p->texuv[0] = u; \
	p->texuv[1] = v; \
	}

	EMIT_VERTEX(p0, bi.u, bi.v);
	EMIT_VERTEX(p1, bi.u + bi.du, bi.v);
	EMIT_VERTEX(p2, bi.u + bi.du, bi.v + bi.dv);
	EMIT_VERTEX(p3, bi.u, bi.v + bi.dv);
}

void
bullets_initialize()
{
	extern ProgramCode *parse_script(const char *filename);

	bullet_code = parse_script("bullets.bc");

	bullet_texture_id = png_to_texture("bullet.png");

	float u = 0;

	for (bullet_info *p = bullet_infos; p != &bullet_infos[NUM_BULLET_TYPES]; p++) {
		float du = static_cast<float>(p->texture_width)/BULLET_TEXTURE_WIDTH;
		float dv = static_cast<float>(p->texture_height)/BULLET_TEXTURE_HEIGHT;

		p->u = u;
		p->v = 0;

		p->du = du;
		p->dv = dv;

		u += du;
	}
}

void
bullets_tear_down()
{
	for (bullet *p = bullets; p != &bullets[MAX_BULLETS]; ++p) {
		if (p->used)
			p->release();
	}

	delete bullet_code;
}

void
bullets_draw(int lerp_tics)
{
	float dt = (float)lerp_tics/(1000/FPS);

	num_gl_vertices = 0;

	for (const bullet *p = bullets; p != &bullets[MAX_BULLETS]; ++p) {
		if (p->used)
			p->draw(dt);
	}

	glColor3f(1, 1, 1);

	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, gl_texture_id(bullet_texture_id));

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);

	glVertexPointer(2, GL_FLOAT, sizeof(struct gl_vertex), &gl_vertex_array[0].position);
	glTexCoordPointer(2, GL_FLOAT, sizeof(struct gl_vertex), &gl_vertex_array[0].texuv);
	glDrawArrays(GL_QUADS, 0, num_gl_vertices);

	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
}

void
bullets_update()
{
	for (bullet *p = bullets; p != &bullets[MAX_BULLETS]; p++) {
		if (p->used)
			p->update();
	}
}

void
bullet_spawn(float x, float y, const char *class_name)
{
	bullet *p = find_unused_bullet();

	if (p)
		p->initialize(x, y, class_name);
}
