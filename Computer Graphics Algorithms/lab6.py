'''
Лабораторная работа №6 – построение реалистичных изображений
а.  Базовой лабораторной работой является лабораторная работа №3 
    (модельно-видовые преобразования и преобразования проецирования).

б.  Определить параметры модели освещения OpenGL (свойства источника 
    света, свойства материалов (поверхностей), характеристики 
    глобальной модели освещения).

в.  Исследовать один из методов повышения реалистичности получаемых 
    изображений сцены (в соответствии с вариантом):
    А3. влияние параметров (компонентов модели освещения) 
        источника света;

г.  Реализовать один из алгоритмов анимации (в соответствии с вариантом):
    Б1. моделирование равноускоренного падения (с заданной 
        начальной скоростью) при условии абсолютно упругого соударения 
        с горизонтальной поверхностью;

д.  Реализовать наложение текстуры (загрузка из файла *.bmp или 
    процедурная генерация) с возможностью отключения (в соответствии 
    с вариантом):
    В1. использование текстуры для определения интенсивности поверхности;
'''
import glfw
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import numpy as np
from PIL import Image

'''
MEASURE_DURATION = 5.0  # секунды
measure_start_time = None
frame_count = 0
total_render_time = 0.0
'''
angle_x, angle_y = 0, 0
scale = 0.7
wireframe = False
light_diffuse = [0.2, 0.2, 0.2, 1.0]
light_position_toggle = False
light_diffuse_toggle = False
light_specular_toggle = False

gravity = -9.8
y_pos = 1.0
y_velocity = 2.5
last_time = glfw.get_time()
use_texture = True

#  Эллипсоид
a, b, c = 1.0, 0.6, 0.8
num_lat = 10
num_lon = 20

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

            if use_texture:
                glTexCoord2f(j / num_lon, i / num_lat)
            glVertex3fv(v1)

            if use_texture:
                glTexCoord2f(j / num_lon, (i + 1) / num_lat)
            glVertex3fv(v2)

            if use_texture:
                glTexCoord2f((j + 1) / num_lon, (i + 1) / num_lat)
            glVertex3fv(v3)

            if use_texture:
                glTexCoord2f(j / num_lon, i / num_lat)
            glVertex3fv(v1)

            if use_texture:
                glTexCoord2f((j + 1) / num_lon, (i + 1) / num_lat)
            glVertex3fv(v3)

            if use_texture:
                glTexCoord2f((j + 1) / num_lon, i / num_lat)
            glVertex3fv(v4)
    glEnd()

def draw_moving_ellipsoid():
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glRotatef(angle_x, 1, 0, 0)
    glRotatef(angle_y, 0, 1, 0)
    
    glPushMatrix()
    glTranslatef(0, y_pos, 0)
    glScalef(scale, scale, scale)
    glColor3f(75/255, 0, 130/255)
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, [0.2, 0.2, 0.4, 1.0])
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, [0.3, 0.3, 0.8, 1.0])
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [1.0, 1.0, 1.0, 1.0])
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 50.0)
    draw_parametric_ellipsoid()
    glPopMatrix()

def setup_lighting():
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glShadeModel(GL_SMOOTH)

    # глобальное освещение
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, [0.5, 0.5, 0.5, 1.0])

    # параметры источника света 0
    glLightfv(GL_LIGHT0, GL_POSITION, [2.0, 2.0, 2.0, 1.0])
    glLightfv(GL_LIGHT0, GL_DIFFUSE, [1.0, 1.0, 1.0, 1.0])
    glLightfv(GL_LIGHT0, GL_SPECULAR, [1.0, 1.0, 1.0, 1.0])

    # свойства материалов
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, [0.2, 0.2, 0.4, 1.0])
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, [0.3, 0.3, 0.8, 1.0])
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [1.0, 1.0, 1.0, 1.0])
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 50.0)

def update_light():
    global light_position_toggle, light_diffuse_toggle, light_specular_toggle

    # положение источника света
    if light_position_toggle:
        glLightfv(GL_LIGHT0, GL_POSITION, [-2.0, -2.0, 2.0, 1.0])
    else:
        glLightfv(GL_LIGHT0, GL_POSITION, [2.0, 2.0, 2.0, 1.0])

    # диффузная компонента
    if light_diffuse_toggle:
        glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.2, 1.0, 0.2, 1.0])
    else:
        glLightfv(GL_LIGHT0, GL_DIFFUSE, [1.0, 1.0, 1.0, 1.0])

    # зеркальная компонента
    if light_specular_toggle:
        glLightfv(GL_LIGHT0, GL_SPECULAR, [0.0, 0.0, 0.0, 1.0])
    else:
        glLightfv(GL_LIGHT0, GL_SPECULAR, [1.0, 1.0, 1.0, 1.0])

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

def load_texture(path):
    image = Image.open(path)
    image = image.transpose(Image.FLIP_TOP_BOTTOM)
    img_data = image.convert("RGB").tobytes()

    texture_id = glGenTextures(1)
    glBindTexture(GL_TEXTURE_2D, texture_id)

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, image.width, image.height,
                 0, GL_RGB, GL_UNSIGNED_BYTE, img_data)

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    return texture_id

def display():
    '''
    global measure_start_time, frame_count, total_render_time
    if measure_start_time is None:
        measure_start_time = glfw.get_time()
    frame_start = glfw.get_time()
    '''
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    draw_moving_ellipsoid()
    glfw.swap_buffers(window)

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    draw_moving_ellipsoid()
    glfw.swap_buffers(window)
    '''
    frame_end = glfw.get_time()
    render_time = frame_end - frame_start
    total_render_time += render_time
    frame_count += 1
    '''

def key_callback(window, key, scancode, action, mods):
    global angle_x, angle_y, wireframe, scale, use_texture, light_position_toggle, light_diffuse_toggle, light_specular_toggle
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
            if use_texture:
                glEnable(GL_TEXTURE_2D)
            else:
                glDisable(GL_TEXTURE_2D)
        elif key == glfw.KEY_1:
            light_position_toggle = not light_position_toggle
            update_light()
        elif key == glfw.KEY_2:
            light_diffuse_toggle = not light_diffuse_toggle
            update_light()
        elif key == glfw.KEY_3:
            light_specular_toggle = not light_specular_toggle
            update_light()

def scroll_callback(window, xoffset, yoffset):
    global scale
    scale += 0.1 if yoffset > 0 else -0.1

def init():
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_TEXTURE_2D)
    texture_id = load_texture(r"C:\Go\projects\Bmstu-projects\Grafic\lab6\texture.bmp")
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    setup_lighting()

if not glfw.init():
    raise Exception("GLFW can't be initialized")

window = glfw.create_window(800, 600, "lab6", None, None)
if not window:
    glfw.terminate()
    raise Exception("GLFW window can't be created")

glfw.make_context_current(window)

glfw.set_key_callback(window, key_callback)
glfw.set_scroll_callback(window, scroll_callback)
init()

while not glfw.window_should_close(window):
    '''
    current_time = glfw.get_time()
    if measure_start_time is not None and current_time - measure_start_time >= MEASURE_DURATION:
        break
    '''

    update_motion()
    display()
    glfw.poll_events()

glfw.terminate()

'''
if frame_count > 0:
    avg_frame_time = (total_render_time / frame_count) * 1000  # в мс
    fps = frame_count / total_render_time
    print(f"Замер длился: {MEASURE_DURATION:.1f} секунд")
    print(f"Общее число кадров: {frame_count}")
    print(f"Среднее время кадра: {avg_frame_time:.3f} мс")
    print(f"FPS: {fps:.2f}")
'''
