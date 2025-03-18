import glfw
from OpenGL.GL import *

delta = 0
angle = 0
posx = 0
posy = 0
size = 0.2

def main():
    if not glfw.init():
        return

    window = glfw.create_window(640, 640, "Lab1", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    glfw.set_scroll_callback(window, scroll_callback)
    glfw.swap_interval(1)  

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-1, 1, -1, 1, -1, 1)
    glMatrixMode(GL_MODELVIEW)

    while not glfw.window_should_close(window):
        display(window)
        glfw.poll_events()
        glfw.swap_buffers(window)

    glfw.destroy_window(window)
    glfw.terminate()

def display(window):
    global angle

    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity()
    glClearColor(1.0, 1.0, 1.0, 1.0)

    glPushMatrix()
    glTranslatef(posx, posy, 0)
    glRotatef(angle, 0, 0, 1)

    glBegin(GL_POLYGON)
    glColor3f(0.8, 0.2, 0.1)
    glVertex2f(size * 0.8 + 0.4, size * 1.2 + 0.5)  # Смещенная вершина
    glColor3f(0.3, 0.7, 0.2)
    glVertex2f(-size * 1.5 + -0.6, size * 0.8 + 0.5)  # Другая смещенная вершина
    glColor3f(0.1, 0.5, 0.9)
    glVertex2f(-size * 1.0 + -0.4, -size * 1.3 + -0.6)  # Другая сторона
    glColor3f(0.9, 0.3, 0.7)
    glVertex2f(size * 1.3 + 0.5, -size * 0.7 + -0.5)  # вершина
    glEnd()

    glPopMatrix()
    
    angle += delta

def key_callback(window, key, scancode, action, mods):
    global delta
    global size
    if action == glfw.PRESS:
        if key == glfw.KEY_RIGHT:
            delta = -3
        elif key == glfw.KEY_LEFT:
            delta = 3
        elif key == glfw.KEY_UP:
            size += 0.1
        elif key == glfw.KEY_DOWN:
            size -= 0.1

def scroll_callback(window, xoffset, yoffset):
    global size
    size += yoffset / 10

main()
