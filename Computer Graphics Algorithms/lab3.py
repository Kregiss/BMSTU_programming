'''
1. Определить параметризованную модель объекта сцены 
(персональный вариант: эллипсоид)
'''

import glfw
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import numpy as np

angle_x, angle_y = 0, 0
scale = 0.7
wireframe = False

#  Эллипсоид
a, b, c = 1.0, 0.6, 0.8
num_lat = 10  # Широта
num_lon = 20  # Долгота

def parametric_ellipsoid(theta, phi):
    x = a * np.cos(theta) * np.cos(phi)
    y = b * np.sin(theta)
    z = c * np.cos(theta) * np.sin(phi)
    return x, y, z

def draw_parametric_ellipsoid():
    glBegin(GL_TRIANGLES)

    for i in range(num_lat):
        theta1 = -np.pi / 2 + np.pi * i / num_lat
        theta2 = -np.pi / 2 + np.pi * (i + 1) / num_lat

        for j in range(num_lon):
            phi1 = 2 * np.pi * j / num_lon
            phi2 = 2 * np.pi * (j + 1) / num_lon

            # Вершины
            v1 = parametric_ellipsoid(theta1, phi1)
            v2 = parametric_ellipsoid(theta2, phi1)
            v3 = parametric_ellipsoid(theta2, phi2)
            v4 = parametric_ellipsoid(theta1, phi2)

            # Первый треугольник
            glVertex3fv(v1)
            glVertex3fv(v2)
            glVertex3fv(v3)

            # Второй треугольник
            glVertex3fv(v1)
            glVertex3fv(v3)
            glVertex3fv(v4)

    glEnd()

def draw_moving_ellipsoid():
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glRotatef(angle_x, 1, 0, 0)
    glRotatef(angle_y, 0, 1, 0)
    
    glPushMatrix()
    glScalef(scale, scale, scale)
    glColor3f(75/255, 0, 130/255)
    draw_parametric_ellipsoid()
    glPopMatrix()

def display():
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    draw_moving_ellipsoid()
    glfw.swap_buffers(window)

def key_callback(window, key, scancode, action, mods):
    global angle_x, angle_y, wireframe, scale
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

def scroll_callback(window, xoffset, yoffset):
    global scale
    scale += 0.1 if yoffset > 0 else -0.1

def init():
    glEnable(GL_DEPTH_TEST)

if not glfw.init():
    raise Exception("GLFW can't be initialized")

window = glfw.create_window(800, 600, "lab3", None, None)
if not window:
    glfw.terminate()
    raise Exception("GLFW window can't be created")

glfw.make_context_current(window)
glfw.set_key_callback(window, key_callback)
glfw.set_scroll_callback(window, scroll_callback)
init()

while not glfw.window_should_close(window):
    display()
    glfw.poll_events()

glfw.terminate()
