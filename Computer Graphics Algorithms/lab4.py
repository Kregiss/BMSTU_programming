'''
Алгоритм А:         А2	
    построчного сканирования многоугольника со списком активных ребер
Алгоритм Б:         Б2, N=3	
    постфильтрация с взвешенным усреднением области NхN
Вспомогательный:    целочисленный алгоритм Брезенхема
'''
import glfw
from OpenGL.GL import *
import numpy as np

WIDTH, HEIGHT = 800, 600

polygon_points = []
edges = []
scanline_buffer = np.zeros((HEIGHT, WIDTH, 3), dtype=np.uint8)
drawing = True

def bresenham_line(x0, y0, x1, y1):
    """Целочисленный алгоритм Брезенхема."""
    points = []
    dx, dy = abs(x1 - x0), abs(y1 - y0)
    sx = 1 if x0 < x1 else -1
    sy = 1 if y0 < y1 else -1
    err = dx - dy

    while True:
        points.append((x0, y0))
        if x0 == x1 and y0 == y1:
            break
        e2 = 2 * err
        if e2 > -dy:
            err -= dy
            x0 += sx
        if e2 < dx:
            err += dx
            y0 += sy
    return points

def scanline_fill():
    """Построчное сканирование многоугольника со списком активных рёбер."""
    global scanline_buffer
    scanline_buffer.fill(0)

    edge_table = []
    for i in range(len(polygon_points)):
        x0, y0 = polygon_points[i]
        x1, y1 = polygon_points[(i + 1) % len(polygon_points)]

        # Пропускаем горизонтальные рёбра
        if y0 == y1:
            continue

        # Упорядочиваем вершины
        if y0 > y1:
            x0, y0, x1, y1 = x1, y1, x0, y0

        # инкремент X
        dx = (x1 - x0) / (y1 - y0)
        edge_table.append([y0, y1, x0, dx])

    edge_table.sort(key=lambda e: e[0])

    active_edges = []
    y = min(pt[1] for pt in polygon_points) 

    while edge_table or active_edges:
        # Рёбра, начинающиеся на этой строке
        active_edges.extend(edge for edge in edge_table if edge[0] == y)
        edge_table = [edge for edge in edge_table if edge[0] != y]

        # Удаление рёбер, заканчивающихся на этой строке
        active_edges = [edge for edge in active_edges if edge[1] > y]

        active_edges.sort(key=lambda e: e[2])

        for i in range(0, len(active_edges) - 1, 2):
            x_start = int(active_edges[i][2])
            x_end = int(active_edges[i + 1][2])
            scanline_buffer[y, x_start:x_end + 1] = (255, 255, 255)

        y += 1
        for edge in active_edges:
            edge[2] += edge[3]  # x += dx

def apply_filter():
    """Фильтрация с взвешенным усреднением 3×3."""
    global scanline_buffer
    kernel = np.array([[1, 2, 1],
                       [2, 4, 2],
                       [1, 2, 1]]) / 16.0
    height, width, _ = scanline_buffer.shape
    new_buffer = np.copy(scanline_buffer)

    for y in range(1, height - 1):
        for x in range(1, width - 1):
            region = scanline_buffer[y - 1:y + 2, x - 1:x + 2, 0]
            new_buffer[y, x] = np.sum(region * kernel)

    scanline_buffer = new_buffer

def draw_buffer():
    glDrawPixels(WIDTH, HEIGHT, GL_RGB, GL_UNSIGNED_BYTE, scanline_buffer)

def mouse_button_callback(window, button, action, mods):
    global drawing
    if action == glfw.PRESS and button == glfw.MOUSE_BUTTON_LEFT:
        
        width, height = glfw.get_framebuffer_size(window)
    
        x, y = glfw.get_cursor_pos(window)

        x = int(x * WIDTH / width)
        y = int(y * HEIGHT / height)
        polygon_points.append((x, y))
        print("[%d, %d]" % (x, y))

def framebuffer_size_callback(window, width, height):
    global WIDTH, HEIGHT, scanline_buffer

    WIDTH, HEIGHT = width, height

    glViewport(0, 0, WIDTH, HEIGHT)

    scanline_buffer = np.zeros((HEIGHT, WIDTH, 3), dtype=np.uint8)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, WIDTH, 0, HEIGHT, -1, 1)
    glMatrixMode(GL_MODELVIEW)

def key_callback(window, key, scancode, action, mods):
    global drawing, scanline_buffer
    if action == glfw.PRESS:
        if key == glfw.KEY_ENTER:  # завершить ввод
            drawing = False
            scanline_fill()
        elif key == glfw.KEY_F:  # применить фильтр
            apply_filter()
        elif key == glfw.KEY_C:  # очистить экран
            polygon_points.clear()
            edges.clear()
            scanline_buffer.fill(0)
            drawing = True

def main():
    if not glfw.init():
        return

    window = glfw.create_window(WIDTH, HEIGHT, "Lab4", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_mouse_button_callback(window, mouse_button_callback)
    glfw.set_key_callback(window, key_callback)
    glfw.set_framebuffer_size_callback(window, framebuffer_size_callback)

    glPixelZoom(1, -1)  # Инверсия Y-координаты
    glRasterPos2f(-1, 1)

    while not glfw.window_should_close(window):
        glClear(GL_COLOR_BUFFER_BIT)

        if drawing:
            glBegin(GL_LINE_LOOP)
            glColor3f(1, 1, 1)
            for x, y in polygon_points:
                glVertex2f(2 * x / WIDTH - 1, 2 * y / HEIGHT - 1)
            glEnd()

        draw_buffer()
        glfw.swap_buffers(window)
        glfw.poll_events()

    glfw.terminate()

if __name__ == "__main__":
    main()
