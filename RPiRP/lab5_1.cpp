#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <iostream>
#include <mutex>
#include <random>
#include <thread>
#include <vector>
#include <io.h>
#include <fcntl.h>

using Clock = std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::microseconds;
using namespace std::chrono_literals;

class Barrier {
public:
    explicit Barrier(unsigned count) : thread_count(count), counter(0), waiting(0) {}
    void wait() {
        std::unique_lock<std::mutex> lk(mtx);
        unsigned gen = counter;
        if (++waiting == thread_count) {
            // последняя пришедшая
            waiting = 0;
            ++counter; // увеличиваем поколение
            cv.notify_all();
        }
        else {
            cv.wait(lk, [&] { return counter != gen; });
        }
    }

private:
    std::mutex mtx;
    std::condition_variable cv;
    const unsigned thread_count;
    unsigned counter; // поколение
    unsigned waiting;
};

// Вспомогательные типы
using Grid = std::vector<std::vector<uint8_t>>; // 0 или 1

// Подсчет живых соседей для клетки (r, c) в сетке.
// Параметр 'rows' и 'cols' — размеры полной матрицы.
// Функция читает из grid, который доступен целиком для чтения.
inline int count_neighbors_global(const Grid& grid, int rows, int cols, int r, int c) {
    int cnt = 0;
    for (int dr = -1; dr <= 1; ++dr) {
        int rr = (r + dr + rows) % rows;
        for (int dc = -1; dc <= 1; ++dc) {
            if (dr == 0 && dc == 0) continue;
            int cc = (c + dc + cols) % cols;
            cnt += grid[rr][cc];
        }
    }
    return cnt;
}

// Подсчет живых соседей для клетки, когда у потока есть:
// - доступ к своей полосе full_local (rows_local x cols)
// - доступ к top_halo (cols) — строка над своей полосой
// - доступ к bottom_halo (cols) — строка под своей полосой
// local_r — индекс строки внутри полосы (0..rows_local-1)
inline int count_neighbors_local(const std::vector<std::vector<uint8_t>>& local,
    const std::vector<uint8_t>& top_halo,
    const std::vector<uint8_t>& bottom_halo,
    int rows_local, int cols, int local_r, int c) {
    int cnt = 0;
    for (int dr = -1; dr <= 1; ++dr) {
        int rr = local_r + dr;
        for (int dc = -1; dc <= 1; ++dc) {
            if (dr == 0 && dc == 0) continue;
            int cc = (c + dc + cols) % cols;
            if (rr < 0) {
                // берем из top_halo
                cnt += top_halo[cc];
            }
            else if (rr >= rows_local) {
                // берем из bottom_halo
                cnt += bottom_halo[cc];
            }
            else {
                cnt += local[rr][cc];
            }
        }
    }
    return cnt;
}

// Инициализация матрицы случайным образом (0/1)
void random_init(Grid& grid, int rows, int cols, double alive_prob = 0.5) {
    std::mt19937_64 rng(std::random_device{}());
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            grid[r][c] = (dist(rng) < alive_prob) ? 1 : 0;
        }
    }
}

// Выполнение одного шага однопоточно (для сравнения)
// Из current в next
void step_single_thread(const Grid& current, Grid& next, int rows, int cols) {
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            int n = count_neighbors_global(current, rows, cols, r, c);
            if (current[r][c]) {
                next[r][c] = (n == 2 || n == 3) ? 1 : 0;
            }
            else {
                next[r][c] = (n == 3) ? 1 : 0;
            }
        }
    }
}


// Разбиение строк на полосы: возвращает пары (start_row, end_row_inclusive) для каждого потока.
std::vector<std::pair<int, int>> partition_stripes(int rows, int nthreads) {
    std::vector<std::pair<int, int>> parts;
    parts.reserve(nthreads);
    int base = rows / nthreads;
    int rem = rows % nthreads;
    int cur = 0;
    for (int i = 0; i < nthreads; ++i) {
        int size = base + (i < rem ? 1 : 0);
        int s = cur;
        int e = cur + size - 1;
        if (size == 0) { s = 0; e = -1; } // пустая полоса
        parts.emplace_back(s, e);
        cur += size;
    }
    return parts;
}

// Многопоточная реализация с барьерной синхронизацией и обменом строк.
// Возвращает среднее время одного шага в микросекундах.
double run_multithread(Grid grid_current, Grid grid_next, int rows, int cols, int steps, int nthreads) {
    if (nthreads <= 0) nthreads = 1;
    // Разбиение полос
    auto parts = partition_stripes(rows, nthreads);

    // Буферы для обмена граничными строками. Для каждого потока два буфера: top и bottom
    std::vector<std::vector<uint8_t>> top_rows(nthreads, std::vector<uint8_t>(cols));
    std::vector<std::vector<uint8_t>> bottom_rows(nthreads, std::vector<uint8_t>(cols));
    std::vector<std::mutex> top_mutex(nthreads), bottom_mutex(nthreads);

    // Три барьера на итерацию
    Barrier barrier_send(nthreads);
    Barrier barrier_compute(nthreads);
    Barrier barrier_swap(nthreads);

    std::atomic<bool> stop_flag{ false };

    auto worker = [&](int tid) {
        auto [srow, erow] = parts[tid];
        if (srow > erow) {
            // пустая полоса: просто участвует в барьерах
            for (int step = 0; step < steps; ++step) {
                // пустые отправки (ничего не делаем)
                barrier_send.wait();
                barrier_compute.wait();
                barrier_swap.wait();
            }
            return;
        }
        int rows_local = erow - srow + 1;
        // Локальная копия полосы из текущей матрицы
        std::vector<std::vector<uint8_t>> local(rows_local, std::vector<uint8_t>(cols));
        // Буферы для гало-строк
        std::vector<uint8_t> top_halo(cols), bottom_halo(cols);

        for (int step = 0; step < steps; ++step) {
            // Подготовка и отправка своих граничных строк
            // Копируем верхнюю и нижнюю строки из grid_current в общие буферы
            {
                std::lock_guard<std::mutex> lk(top_mutex[tid]);
                for (int c = 0; c < cols; ++c) top_rows[tid][c] = grid_current[srow][c];
            }
            {
                std::lock_guard<std::mutex> lk(bottom_mutex[tid]);
                for (int c = 0; c < cols; ++c) bottom_rows[tid][c] = grid_current[erow][c];
            }

            // Синхронизация: все отправили свои граничные строки
            barrier_send.wait();


            // Теперь читаем соседние гало-строки.
            // Предположим, полосы упорядочены по индексам 0..nthreads-1, поэтому сосед по вертикали —
            // это либо предыдущий по индексу поток, либо следующий (с учётом wrap-around).
            int prev_tid = tid - 1;
            while (prev_tid >= 0 && parts[prev_tid].first > parts[tid].first - 1) --prev_tid;
            // Упрощение: вычислим владельца строки srow-1 и erow+1.
            auto owner_of_row = [&](int row)->int {
                if (row < 0) row += rows;
                if (row >= rows) row -= rows;
                
                for (int i = 0; i < nthreads; ++i) {
                    if (parts[i].first <= row && row <= parts[i].second) return i;
                }
                return -1;
                };
            int owner_top = owner_of_row(srow - 1);
            int owner_bottom = owner_of_row(erow + 1);
            if (owner_top == -1 || owner_bottom == -1) {
                std::cerr << "Ошибка: не найден владелец строки при вычислении гало.\n";
                return;
            }
            // Читаем bottom_rows[owner_top] -> это строка над нашей полосой
            {
                std::lock_guard<std::mutex> lk(bottom_mutex[owner_top]);
                for (int c = 0; c < cols; ++c) top_halo[c] = bottom_rows[owner_top][c];
            }
            // Читаем top_rows[owner_bottom] -> это строка под нашей полосой
            {
                std::lock_guard<std::mutex> lk(top_mutex[owner_bottom]);
                for (int c = 0; c < cols; ++c) bottom_halo[c] = top_rows[owner_bottom][c];
            }

            // Подготовим локальную копию своей полосы
            for (int i = 0; i < rows_local; ++i)
                for (int c = 0; c < cols; ++c)
                    local[i][c] = grid_current[srow + i][c];

            // Синхронизация: убедились что все прочитали гало
            // Теперь вычисляем новую полосу в grid_next
            for (int local_r = 0; local_r < rows_local; ++local_r) {
                int global_r = srow + local_r;
                for (int c = 0; c < cols; ++c) {
                    int n = count_neighbors_local(local, top_halo, bottom_halo, rows_local, cols, local_r, c);
                    uint8_t cur = local[local_r][c];
                    uint8_t out = 0;
                    if (cur) {
                        out = (n == 2 || n == 3) ? 1 : 0;
                    }
                    else {
                        out = (n == 3) ? 1 : 0;
                    }
                    grid_next[global_r][c] = out; 
                }
            }

            // Синхронизируемся: дождаться, пока все завершили вычисление
            barrier_compute.wait();

            if (tid == 0) {
                for (int r = 0; r < rows; ++r)
                    for (int c = 0; c < cols; ++c)
                        grid_current[r][c] = grid_next[r][c];
            }

            barrier_swap.wait();
        } 
        };

    std::vector<std::thread> threads;
    threads.reserve(nthreads);

    auto t0 = Clock::now();

    for (int i = 0; i < nthreads; ++i) threads.emplace_back(worker, i);

    for (auto& th : threads) th.join();

    auto t1 = Clock::now();
    auto total_us = duration_cast<microseconds>(t1 - t0).count();
    double avg_per_step = double(total_us) / steps;


    return avg_per_step; 
}

int main() {
    int rows = 10000;
    int cols = 10000;
    int steps = 10;
    int nthreads = 8;

    std::cout << "Size: " << rows << "x" << cols << ", steps: " << steps << ", threads: " << nthreads << "\n";

    Grid grid_current(rows, std::vector<uint8_t>(cols));
    Grid grid_next(rows, std::vector<uint8_t>(cols));
    random_init(grid_current, rows, cols, 0.3);

    // Запуск многопоточной версии (копии матрицы передаем по значению в run_multithread,
    // чтобы однопоточная проверка начиналась с того же начального состояния)
    double avg_mt_us = run_multithread(grid_current, grid_next, rows, cols, steps, nthreads);
    std::cout << "Multithreaded: average time per step = " << avg_mt_us / pow(10, 6) << " s\n";

    // Однопоточная версия (начинаем с той же начальной сетки)
    // Скопируем начальную конфигурацию
    Grid grid_single_current = grid_current;
    Grid grid_single_next(rows, std::vector<uint8_t>(cols));

    auto t0 = Clock::now();
    for (int step = 0; step < steps; ++step) {
        step_single_thread(grid_single_current, grid_single_next, rows, cols);
        // копируем next->current
        for (int r = 0; r < rows; ++r)
            for (int c = 0; c < cols; ++c)
                grid_single_current[r][c] = grid_single_next[r][c];
    }
    auto t1 = Clock::now();
    auto total_us = duration_cast<microseconds>(t1 - t0).count();
    double avg_st_us = double(total_us) / steps;
    std::cout << "Single-threaded: average time per step = " << avg_st_us / pow(10,6) << " s\n";

    return 0;
}
