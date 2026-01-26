#include <iostream>
#include <thread>
#include <vector>
#include <mutex>
#include <shared_mutex>
#include <random>
#include <unordered_set>

struct Node {
    int value;
    Node* next;
    Node(int v) : value(v), next(nullptr) {}
};

Node* head = nullptr;

std::mutex read_lock;     
std::mutex write_lock;    

bool contains_read(int x) {
    std::lock_guard<std::mutex> lock(read_lock);

    Node* cur = head;
    while (cur) {
        if (cur->value == x) {
            return true;
        }
        cur = cur->next;
    }

    return false;
}

void insert_if_not_exists(int x) {
    std::lock_guard<std::mutex> w_lock(write_lock);   
   
    {
        Node* cur = head;
        while (cur) {
            if (cur->value == x) return;   
            cur = cur->next;
        }
    }
    Node* n = new Node(x);
    if (!head) {
        head = n;
        return;
    }
    Node* cur = head;
    while (cur->next) cur = cur->next;
    cur->next = n;
}

void worker(int count_per_thread) {
    std::mt19937 rng(std::random_device{}());
    std::uniform_int_distribution<int> dist(0, 1000);

    for (int i = 0; i < count_per_thread; i++) {
        int x = dist(rng);

        if (!contains_read(x)) {
            insert_if_not_exists(x);
        }
    }
}

bool check_duplicates() {
    std::lock_guard<std::mutex> lock(read_lock);
    std::unordered_set<int> seen;
    Node* cur = head;
    while (cur) {
        if (!seen.insert(cur->value).second)
            return false;
        cur = cur->next;
    }
    return true;
}

void cleanup() {
    std::lock_guard<std::mutex> w_lock(write_lock);
    std::lock_guard<std::mutex> r_lock(read_lock);

    Node* cur = head;
    while (cur) {
        Node* temp = cur;
        cur = cur->next;
        delete temp;
    }
    head = nullptr;
}

int main() {
    int nthreads = 8;
    int per_thread = 10000;

    std::vector<std::thread> threads;
    threads.reserve(nthreads);

    for (int i = 0; i < nthreads; i++)
        threads.emplace_back(worker, per_thread);

    for (auto& t : threads) t.join();

    //print_list();

    if (check_duplicates())
        std::cout << "There are no duplicate numbers\n";
    else
        std::cout << "Duplicates found\n";

    cleanup(); 
    return 0;
}
