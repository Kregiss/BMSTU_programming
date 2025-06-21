'''
• 1.Определить куб в качестве модели объекта сцены.
• 2.Определить преобразования, позволяющие получить заданный вид проекции (в соответствии с вариантом). Для демонстрации проекции добавить в
сцену куб (в стандартной ориентации, не изменяемой при модельно-видовых
преобразованиях основного объекта).
• 3.Реализовать изменение ориентации и размеров объекта (навигацию камеры) с помощью модельно-видовых преобразований (без gluLookAt). Управление
производится интерактивно с помощью клавиатуры и/или мыши.
• 4.Предусмотреть возможность переключения между каркасным и твердотельным отображением модели (glFrontFace/ glPolygonMode).

Персональный вариант: двухточечная перспектива.
'''
import glfw
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import numpy as np

# двухточечная перспектива

angle_x, angle_y = 0, 0
scale = 0.5
wireframe = False
vertices = [(-1,-1,-1), (1,-1,-1), (1,1,-1), (-1,1,-1),
            (-1,-1,1), (1,-1,1), (1,1,1), (-1,1,1)]
faces = [(0,1,2,3), (4,5,6,7), (0,1,5,4), (2,3,7,6), (0,3,7,4), (1,2,6,5)]

projection_matrix = np.array([
    0.87, 0, 0.5, 0,
    0, 1, 0, 0,
    1, 0, -1.73, 1,
    0.5, 0, -0.87, 2
], dtype=np.float32)

def draw_static_cube():
    glPushMatrix()
    glViewport(0, 0, 400, 600)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glLoadMatrixf(projection_matrix.T)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glScalef(0.5, 0.5, 0.5)
    glColor3f(1, 0, 0.5)  
    glBegin(GL_QUADS)
    for face in faces:
        for vertex in face:
            glVertex3fv(vertices[vertex])
    glEnd()
    glPopMatrix()

def draw_moving_cube():
    glViewport(400, 0, 400, 600)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glLoadMatrixf(projection_matrix.T)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glRotatef(angle_x, 1, 0, 0)
    glRotatef(angle_y, 0, 1, 0)
    glPushMatrix()
    glScalef(scale, scale, scale)
    glBegin(GL_QUADS)
    glColor3f(0, 1, 0.5)  
    for face in faces:
        for vertex in face:
            glVertex3fv(vertices[vertex])
    glEnd()
    glPopMatrix()

def display():
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    draw_static_cube()  
    draw_moving_cube()  
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
    if yoffset > 0:
        scale += 0.1
    else:
        scale -= 0.1

def init():
    glEnable(GL_DEPTH_TEST)

if not glfw.init():
    raise Exception("GLFW can't be initialized")

window = glfw.create_window(800, 600, "Lab2", None, None)
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
    #print(f"Scale: {scale}, Angle X: {angle_x}, Angle Y: {angle_y}")

glfw.terminate()
