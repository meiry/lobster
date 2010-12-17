#ifndef GL_UTIL_H_
#define GL_UTIL_H_

#include <GL/gl.h>

struct image;
struct material;

int
image_to_texture(struct image *image);

int
png_to_texture(const char *file_name);

GLuint
gl_texture_id(int texture_id);

void
delete_textures(void);

void
reload_textures(void);

void
destroy_texture_pool(void);

#endif /* GL_UTIL_H_ */
