'''
Переписать на шейдерах лабораторную работу №6 
'''

import glfw
from OpenGL.GL import *
import numpy as np
from PIL import Image
import glm
import ctypes

angle_x, angle_y = 0.0, 0.0
scale = 0.7
wireframe = False
use_texture = True
light_position_toggle = False
light_diffuse_toggle = False
light_specular_toggle = False

a, b, c = 1.0, 0.6, 0.8
num_lat = 40
num_lon = 40

gravity = -9.8
y_pos = 1.0
y_velocity = 2.5
last_time = 0.0

def parametric_ellipsoid(theta, phi):
    x = a * np.cos(theta) * np.cos(phi)
    y = b * np.sin(theta)
    z = c * np.cos(theta) * np.sin(phi)
    return x, y, z

def generate_ellipsoid():
    vertices = []
    normals = []
    texcoords = []
    indices = []

    for i in range(num_lat + 1):
        theta = -np.pi / 2 + np.pi * i / num_lat
        for j in range(num_lon + 1):
            phi = 2 * np.pi * j / num_lon
            x, y, z = parametric_ellipsoid(theta, phi)
            vertices.extend([x, y, z])
            nx = x / a
            ny = y / b
            nz = z / c
            length = np.sqrt(nx**2 + ny**2 + nz**2)
            normals.extend([nx/length, ny/length, nz/length])
            texcoords.extend([j / num_lon, i / num_lat])

    for i in range(num_lat):
        for j in range(num_lon):
            first = i * (num_lon + 1) + j
            second = first + num_lon + 1
            indices.extend([first, second, first + 1])
            indices.extend([second, second + 1, first + 1])

    return np.array(vertices, dtype=np.float32), np.array(normals, dtype=np.float32), \
           np.array(texcoords, dtype=np.float32), np.array(indices, dtype=np.uint32)

def load_texture(path):
    image = Image.open(path)
    image = image.transpose(Image.FLIP_TOP_BOTTOM)
    img_data = image.convert("RGB").tobytes()
    width, height = image.size

    texture_id = glGenTextures(1)
    glBindTexture(GL_TEXTURE_2D, texture_id)

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height,
                 0, GL_RGB, GL_UNSIGNED_BYTE, img_data)

    glGenerateMipmap(GL_TEXTURE_2D)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

    return texture_id

def compile_shader(source, shader_type):
    shader = glCreateShader(shader_type)
    glShaderSource(shader, source)
    glCompileShader(shader)
    result = glGetShaderiv(shader, GL_COMPILE_STATUS)
    if not(result):
        raise RuntimeError(glGetShaderInfoLog(shader).decode())
    return shader

def create_shader_program(vertex_source, fragment_source):
    vertex_shader = compile_shader(vertex_source, GL_VERTEX_SHADER)
    fragment_shader = compile_shader(fragment_source, GL_FRAGMENT_SHADER)
    program = glCreateProgram()
    glAttachShader(program, vertex_shader)
    glAttachShader(program, fragment_shader)
    glLinkProgram(program)
    result = glGetProgramiv(program, GL_LINK_STATUS)
    if not(result):
        raise RuntimeError(glGetProgramInfoLog(program).decode())
    glDeleteShader(vertex_shader)
    glDeleteShader(fragment_shader)
    return program

vertex_shader_source = """
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out vec3 FragPos;
out vec3 Normal;
out vec2 TexCoords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
    FragPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    TexCoords = aTexCoords;
    gl_Position = projection * view * vec4(FragPos, 1.0);
}
"""

fragment_shader_source = """
#version 330 core
out vec4 FragColor;

in vec3 FragPos;
in vec3 Normal;
in vec2 TexCoords;

uniform vec3 lightPos;
uniform vec3 viewPos;
uniform sampler2D texture1;

void main()
{
    float ambientStrength = 0.2;
    vec3 ambient = ambientStrength * vec3(1.0);

    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = diff * vec3(1.0);

    float specularStrength = 0.5;
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec3 specular = specularStrength * spec * vec3(1.0);

    vec3 lighting = (ambient + diffuse + specular);
    vec3 textureColor = texture(texture1, TexCoords).rgb;
    FragColor = vec4(lighting * textureColor, 1.0);
}
"""

def key_callback(window, key, scancode, action, mods):
    global angle_x, angle_y, scale, wireframe, use_texture
    global light_position_toggle, light_diffuse_toggle, light_specular_toggle

    if action == glfw.PRESS or action == glfw.REPEAT:
        if key == glfw.KEY_UP:
            angle_x -= 5
        elif key == glfw.KEY_DOWN:
            angle_x += 5
        elif key == glfw.KEY_LEFT:
            angle_y -= 5
        elif key == glfw.KEY_RIGHT:
            angle_y += 5
        elif key == glfw.KEY_EQUAL:
            scale += 0.1
        elif key == glfw.KEY_MINUS:
            scale -= 0.1
        elif key == glfw.KEY_W:
            wireframe = not wireframe
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE if wireframe else GL_FILL)
        elif key == glfw.KEY_T:
            use_texture = not use_texture
        elif key == glfw.KEY_1:
            light_position_toggle = not light_position_toggle
        elif key == glfw.KEY_2:
            light_diffuse_toggle = not light_diffuse_toggle
        elif key == glfw.KEY_3:
            light_specular_toggle = not light_specular_toggle

def scroll_callback(window, xoffset, yoffset):
    global scale
    scale += 0.1 if yoffset > 0 else -0.1

def update_motion():
    global y_pos, y_velocity, last_time

    current_time = glfw.get_time()
    delta_time = current_time - last_time
    last_time = current_time

    y_velocity += gravity * delta_time
    y_pos += y_velocity * delta_time

    if y_pos <= -1.0:
        y_pos = -1.0
        y_velocity = -y_velocity

def main():
    global last_time

    if not glfw.init():
        raise Exception("GLFW can't be initialized")

    window = glfw.create_window(800, 600, "Ellipsoid with Shaders", None, None)
    if not window:
        glfw.terminate()
        raise Exception("GLFW window can't be created")

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    glfw.set_scroll_callback(window, scroll_callback)

    glEnable(GL_DEPTH_TEST)

    vertices, normals, texcoords, indices = generate_ellipsoid()

    VAO = glGenVertexArrays(1)
    VBO = glGenBuffers(3)
    EBO = glGenBuffers(1)

    glBindVertexArray(VAO)

    glBindBuffer(GL_ARRAY_BUFFER, VBO[0])
    glBufferData(GL_ARRAY_BUFFER, vertices.nbytes, vertices, GL_STATIC_DRAW)
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(0))
    glEnableVertexAttribArray(0)

    glBindBuffer(GL_ARRAY_BUFFER, VBO[1])
    glBufferData(GL_ARRAY_BUFFER, normals.nbytes, normals, GL_STATIC_DRAW)
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(0))
    glEnableVertexAttribArray(1)

    glBindBuffer(GL_ARRAY_BUFFER, VBO[2])
    glBufferData(GL_ARRAY_BUFFER, texcoords.nbytes, texcoords, GL_STATIC_DRAW)
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(0))
    glEnableVertexAttribArray(2)

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.nbytes, indices, GL_STATIC_DRAW)

    glBindVertexArray(0)

    shader_program = create_shader_program(vertex_shader_source, fragment_shader_source)
    texture = load_texture(r"C:\Go\projects\Bmstu-projects\Grafic\lab6\texture.bmp")

    last_time = glfw.get_time()

    while not glfw.window_should_close(window):
        glfw.poll_events()
        update_motion()

        glClearColor(0.1, 0.1, 0.1, 1.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        glUseProgram(shader_program)

        projection = glm.perspective(glm.radians(45.0), 800/600, 0.1, 100.0)
        view = glm.translate(glm.mat4(1.0), glm.vec3(0.0, 0.0, -4.0))
        model = glm.mat4(1.0)
        model = glm.rotate(model, glm.radians(angle_x), glm.vec3(1.0, 0.0, 0.0))
        model = glm.rotate(model, glm.radians(angle_y), glm.vec3(0.0, 1.0, 0.0))
        model = glm.translate(model, glm.vec3(0.0, y_pos, 0.0))
        model = glm.scale(model, glm.vec3(scale, scale, scale))

        light_pos = glm.vec3(2.0, 2.0, 2.0)
        view_pos = glm.vec3(0.0, 0.0, 4.0)

        glUniformMatrix4fv(glGetUniformLocation(shader_program, "model"), 1, GL_FALSE, glm.value_ptr(model))
        glUniformMatrix4fv(glGetUniformLocation(shader_program, "view"), 1, GL_FALSE, glm.value_ptr(view))
        glUniformMatrix4fv(glGetUniformLocation(shader_program, "projection"), 1, GL_FALSE, glm.value_ptr(projection))
        glUniform3fv(glGetUniformLocation(shader_program, "lightPos"), 1, glm.value_ptr(light_pos))
        glUniform3fv(glGetUniformLocation(shader_program, "viewPos"), 1, glm.value_ptr(view_pos))

        if use_texture:
            glActiveTexture(GL_TEXTURE0)
            glBindTexture(GL_TEXTURE_2D, texture)
        glUniform1i(glGetUniformLocation(shader_program, "texture1"), 0)

        glBindVertexArray(VAO)
        glDrawElements(GL_TRIANGLES, len(indices), GL_UNSIGNED_INT, None)
        glBindVertexArray(0)

        glfw.swap_buffers(window)

    glDeleteVertexArrays(1, [VAO])
    glDeleteBuffers(3, VBO)
    glDeleteBuffers(1, [EBO])
    glDeleteProgram(shader_program)
    glfw.terminate()

main()
