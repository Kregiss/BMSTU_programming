'''
Лабораторная работа №5 – алгоритмы отсечения
а.  Реализовать один из алгоритмов отсечения определенного 
    типа в пространстве заданной размерности 
    (в соответствии с вариантом).
б.  Ввод исходных данных каждого из алгоритмов производится 
    интерактивно с помощью клавиатуры и/или мыши.

Алгоритм отсечения:	                    
        отрезка произвольным многоугольником
Размерность пространства отсечения:	    
        Трехмерное
Тип отсечения:                           
        Внутреннее
'''
import glfw
from OpenGL.GL import *
from OpenGL.GLU import *
import numpy as np

cube_scale = 1.0 
rotation_x = 0.0
rotation_y = 0.0
line_start = [-2, 0, 0]
line_end = [2, 0, 0]

def get_cube_faces(scale):
    s = scale
    faces = [
        ([ s, 0, 0], [-1, 0, 0]),  # X+
        ([-s, 0, 0], [ 1, 0, 0]),  # X-
        ([0,  s, 0], [ 0, -1, 0]), # Y+
        ([0, -s, 0], [ 0, 1, 0]),  # Y-
        ([0, 0,  s], [ 0, 0, -1]), # Z+
        ([0, 0, -s], [ 0, 0, 1]),  # Z-
    ]
    return faces




def intersect_line_plane(p1, p2, plane_point, plane_normal):
    #Находим точку пересечения прямой с плоскостью
    p1 = np.array(p1, dtype=np.float64)
    p2 = np.array(p2, dtype=np.float64)
    plane_point = np.array(plane_point, dtype=np.float64)
    plane_normal = np.array(plane_normal, dtype=np.float64)

    d = p2 - p1
    #print(d)
    denom = int(np.dot(plane_normal, d))
    #print(denom, plane_normal)

    if abs(denom) < 1e-8:
        return None  #Прямая || плоскости

    t = int(np.dot(plane_normal, plane_point - p1)) / denom
    #print(t)

    if 0 <= t <= 1:
        return p1 + t * d
    else:
        return None  #Точка вне отрезка

def point_inside_polyhedron(point, faces):
    #Проверяем, лежит ли точка внутри куба
    point = np.array(point, dtype=np.float64)
    for plane_point, normal in faces:
        plane_point = np.array(plane_point, dtype=np.float64)
        normal = np.array(normal, dtype=np.float64)
        if np.dot(normal, point - plane_point) < -1e-8:
            return False
    return True

def clip_line_simple(p1, p2, faces):
    p1 = np.array(p1, dtype=np.float64)
    p2 = np.array(p2, dtype=np.float64)
    points = []

    #1. Начальная и конечная точки
    if point_inside_polyhedron(p1, faces):
        points.append(p1)
    if point_inside_polyhedron(p2, faces):
        points.append(p2)

    #2. Проверяем пересечения с каждой гранью
    for plane_point, normal in faces:
        pt = intersect_line_plane(p1, p2, plane_point, normal)
        if pt is not None and point_inside_polyhedron(pt, faces):
            if not any(np.allclose(pt, existing, atol=1e-6) for existing in points):
                points.append(pt)

    if len(points) < 2:
        return None

    #3. Сортировка по расстоянию от p1
    points = sorted(points, key=lambda pt: np.linalg.norm(pt - p1))
    return points[0].tolist(), points[1].tolist()




def draw_cube(scale):
    s = scale
    glBegin(GL_LINES)
    glColor3f(0.5, 0.5, 0.5)
    edges = [
        # рёбра
        [-s,-s,-s], [ s,-s,-s],
        [-s, s,-s], [ s, s,-s],
        [-s,-s, s], [ s,-s, s],
        [-s, s, s], [ s, s, s],
        [-s,-s,-s], [-s, s,-s],
        [ s,-s,-s], [ s, s,-s],
        [-s,-s, s], [-s, s, s],
        [ s,-s, s], [ s, s, s],
        [-s,-s,-s], [-s,-s, s],
        [ s,-s,-s], [ s,-s, s],
        [-s, s,-s], [-s, s, s],
        [ s, s,-s], [ s, s, s],
    ]
    for i in range(0, len(edges), 2):
        glVertex3fv(edges[i])
        glVertex3fv(edges[i + 1])
    glEnd()

def draw_line(p0, p1, color, width = 1.0):
    glLineWidth(width)
    glColor3f(*color)
    glBegin(GL_LINES)
    glVertex3fv(p0)
    glVertex3fv(p1)
    glEnd()
    glLineWidth(1.0)

def input_line():
    try:
        print("Введите координаты начала отрезка (x y z):")
        x1, y1, z1 = map(float, input(">>> ").split())
        print("Введите координаты конца отрезка (x y z):")
        x2, y2, z2 = map(float, input(">>> ").split())
        return [x1, y1, z1], [x2, y2, z2]
    except Exception as e:
        print(f"Ошибка ввода: {e}")
        return None, None

def key_callback(window, key, scancode, action, mods):
    global cube_scale, rotation_x, rotation_y
    if action == glfw.PRESS or action == glfw.REPEAT:
        if key == glfw.KEY_EQUAL or key == glfw.KEY_KP_ADD:
            cube_scale += 0.1
        elif key == glfw.KEY_MINUS or key == glfw.KEY_KP_SUBTRACT:
            cube_scale = max(0.1, cube_scale - 0.1)
        elif key == glfw.KEY_S:
            rotation_x -= 5.0
        elif key == glfw.KEY_E:
            rotation_x += 5.0
        elif key == glfw.KEY_F:
            rotation_y -= 5.0
        elif key == glfw.KEY_D:
            rotation_y += 5.0
        elif key == glfw.KEY_L:
            global line_start, line_end
            glfw.iconify_window(window)  #свернуть окно
            p0, p1 = input_line()
            glfw.restore_window(window)
            if p0 and p1:
                line_start, line_end = p0, p1

def main():
    global rotation_x, rotation_y

    if not glfw.init():
        return

    window = glfw.create_window(1000, 750, "Lab5", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)

    glEnable(GL_DEPTH_TEST)

    while not glfw.window_should_close(window):
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(45, 800 / 600, 0.1, 100)
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()

        gluLookAt(0, 0, 5, 0, 0, 0, 0, 1, 0)

        glRotatef(rotation_x, 1, 0, 0)
        glRotatef(rotation_y, 0, 1, 0)

        draw_cube(cube_scale)

        draw_line(line_start, line_end, (0.6, 0.6, 0.6), width=3.0)

        clipped = clip_line_simple(np.array(line_start), np.array(line_end), get_cube_faces(cube_scale))
        #print(clipped)
        if clipped:
            glDisable(GL_DEPTH_TEST)  
            draw_line(clipped[0], clipped[1], (1, 0, 0), width=5.0)
            glEnable(GL_DEPTH_TEST)

        glfw.swap_buffers(window)
        glfw.poll_events()

    glfw.terminate()

main()