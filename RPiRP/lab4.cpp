#include <iostream>
#include <vector>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <random>
#include <atomic>
#include <fstream>
#include <sstream>
#include <iomanip>

enum class State {
    THINKING,
    TAKING_LEFT,
    TAKING_RIGHT,
    EATING,
    PUTTING_DOWN
};

static const char* stateToString(State s) {
    switch (s) {
    case State::THINKING: return "THINKING";
    case State::TAKING_LEFT: return "TAKING_LEFT";
    case State::TAKING_RIGHT: return "TAKING_RIGHT";
    case State::EATING: return "EATING";
    case State::PUTTING_DOWN: return "PUTTING_DOWN";
    default: return "UNKNOWN";
    }
}

class Waiter {
public:
    explicit Waiter(size_t limit) : limit_(limit), counter_(0) {}

    void acquire() {
        std::unique_lock<std::mutex> lk(mtx_);
        cv_.wait(lk, [this]() { return counter_ < limit_; });
        ++counter_;
    }

    void release() {
        {
            std::lock_guard<std::mutex> lk(mtx_);
            if (counter_ > 0) --counter_;
        }
        cv_.notify_one();
    }

private:
    size_t limit_;
    size_t counter_;
    std::mutex mtx_;
    std::condition_variable cv_;
};

struct LogEntry {
    long long time_ms;
    size_t philosopher;
    State state;
};

class Logger {
public:
    void push(long long t, size_t id, State s) {
        std::lock_guard<std::mutex> lk(mtx_);
        log_.push_back({ t, id, s });
    }

    void toCSV(const std::string& filename) {
        std::lock_guard<std::mutex> lk(mtx_);
        std::ofstream ofs(filename);
        ofs << "time_ms,philosopher,state\n";
        for (auto& e : log_) {
            ofs << e.time_ms << "," << e.philosopher << "," << stateToString(e.state) << "\n";
        }
    }

    std::vector<size_t> countEats(size_t N) {
        std::vector<size_t> cnt(N, 0);
        std::lock_guard<std::mutex> lk(mtx_);
        for (size_t i = 0; i + 1 < log_.size(); ++i) {
            if (log_[i].state == State::EATING) ++cnt[log_[i].philosopher];
        }
        return cnt;
    }

private:
    std::vector<LogEntry> log_;
    std::mutex mtx_;
};

long long now_ms(const std::chrono::steady_clock::time_point& start) {
    auto d = std::chrono::steady_clock::now() - start;
    return std::chrono::duration_cast<std::chrono::milliseconds>(d).count();
}

class PhilosophersMonitor {
private:
    std::vector<State> states;
    std::mutex mtx;
    const std::chrono::steady_clock::time_point& start_time;
    Logger& logger;

    long long getCurrentTime() {
        return now_ms(start_time);
    }

public:
    PhilosophersMonitor(size_t N, const std::chrono::steady_clock::time_point& start, Logger& log_ref)
        : states(N, State::THINKING), start_time(start), logger(log_ref) {
    }

    void updateState(size_t id, State new_state) {
        std::lock_guard<std::mutex> lock(mtx);

        long long current_time = getCurrentTime();
        logger.push(current_time, id, new_state);
        states[id] = new_state;

        printStates(current_time);
    }

    void printStates(long long current_time) {
        std::cout << "\nTime: " << current_time << "ms\n";
        std::cout << std::string(60, '-') << "\n";
        std::cout << "| " << std::setw(10) << "Philosopher" << " | "
            << std::setw(15) << "State" << " | "
            << std::setw(25) << "Description" << " |\n";
        std::cout << std::string(60, '-') << "\n";

        for (size_t i = 0; i < states.size(); ++i) {
            std::string description;
            switch (states[i]) {
            case State::THINKING:
                description = "Thinking deeply";
                break;
            case State::TAKING_LEFT:
                description = "Taking left fork " + std::to_string(i);
                break;
            case State::TAKING_RIGHT:
                description = "Taking right fork " + std::to_string((i + 1) % states.size());
                break;
            case State::EATING:
                description = "Eating with forks " + std::to_string(i) + " and " + std::to_string((i + 1) % states.size());
                break;
            case State::PUTTING_DOWN:
                description = "Putting down both forks";
                break;
            }

            std::cout << "| " << std::setw(10) << i << " | "
                << std::setw(15) << stateToString(states[i]) << " | "
                << std::setw(25) << description << " |\n";
        }
        std::cout << std::string(60, '-') << std::endl;
    }

    std::vector<State> getCurrentStates() {
        std::lock_guard<std::mutex> lock(mtx);
        return states;
    }
};

void philosopher_routine(size_t id,
    size_t N,
    std::vector<std::mutex>& forks,
    Waiter& waiter,
    PhilosophersMonitor& monitor,
    std::atomic<bool>& stop_flag,
    const std::chrono::steady_clock::time_point& start_time,
    int think_min_ms, int think_max_ms,
    int eat_min_ms, int eat_max_ms,
    std::mt19937& rng_for_thread)
{
    size_t left = id;
    size_t right = (id + 1) % N;

    std::uniform_int_distribution<int> think_dist(think_min_ms, think_max_ms);
    std::uniform_int_distribution<int> eat_dist(eat_min_ms, eat_max_ms);

    while (!stop_flag.load(std::memory_order_relaxed)) {
        monitor.updateState(id, State::THINKING);
        std::this_thread::sleep_for(std::chrono::milliseconds(think_dist(rng_for_thread)));
        if (stop_flag.load(std::memory_order_relaxed)) break;

        waiter.acquire();

        monitor.updateState(id, State::TAKING_LEFT);
        forks[left].lock();

        monitor.updateState(id, State::TAKING_RIGHT);
        forks[right].lock();

        monitor.updateState(id, State::EATING);
        std::this_thread::sleep_for(std::chrono::milliseconds(eat_dist(rng_for_thread)));

        monitor.updateState(id, State::PUTTING_DOWN);
        forks[right].unlock();
        forks[left].unlock();

        waiter.release();
    }
}

int main(int argc, char* argv[]) {
    size_t N = 5;
    int think_min_ms = 50;
    int think_max_ms = 200;
    int eat_min_ms = 50;
    int eat_max_ms = 150;
    int simulation_seconds = 10;
    unsigned seed = (unsigned)std::chrono::steady_clock::now().time_since_epoch().count();

    std::cout << "Dining philosophers simulation\n";
    std::cout << "N = " << N << ", simulation_seconds = " << simulation_seconds << ", seed = " << seed << "\n";
    std::cout << "Initializing philosophers...\n\n";

    std::vector<std::mutex> forks(N);
    Waiter waiter((N > 1) ? (N / 2) : 1);
    Logger logger;
    std::atomic<bool> stop_flag(false);

    auto start_time = std::chrono::steady_clock::now();
    PhilosophersMonitor monitor(N, start_time, logger);

    monitor.printStates(0);

    std::random_device rd;
    std::mt19937 seed_gen(seed);
    std::vector<unsigned> thread_seeds(N);
    for (size_t i = 0; i < N; ++i) thread_seeds[i] = seed_gen();

    std::vector<std::thread> threads;
    threads.reserve(N);
    for (size_t i = 0; i < N; ++i) {
        std::mt19937 rng(thread_seeds[i]);
        threads.emplace_back(philosopher_routine,
            i, N, std::ref(forks), std::ref(waiter), std::ref(monitor),
            std::ref(stop_flag), std::cref(start_time),
            think_min_ms, think_max_ms, eat_min_ms, eat_max_ms,
            std::ref(rng));
    }

    std::this_thread::sleep_for(std::chrono::seconds(simulation_seconds));
    stop_flag.store(true, std::memory_order_relaxed);

    for (auto& t : threads) {
        if (t.joinable()) t.join();
    }

    std::string csvname = "philosophers_log.csv";
    logger.toCSV(csvname);

    auto counts = logger.countEats(N);
    std::cout << "\n\n=== SIMULATION COMPLETE ===\n";
    std::cout << "Summary: number of 'EATING' events per philosopher:\n";
    for (size_t i = 0; i < N; ++i) {
        std::cout << "Philosopher " << std::setw(2) << i << ": " << counts[i] << " times\n";
    }

    std::cout << "Detailed log exported to " << csvname << "\n";
    return 0;
}