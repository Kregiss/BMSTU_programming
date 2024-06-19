#include <limits>
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <queue>
using namespace std;

vector<vector<int>> g, gr;
vector<int> used, top, color, in_component;
set<pair<int,int>> s;

void dfs1(int v) {
    used[v] = 1;
    for(int to : g[v]) {
        if (!used[to]) dfs1(to);
    }
    top.push_back(v);
}

void dfs2(int v, int c, vector<vector<int>>& components) {
    color[v] = c;
    components[c].push_back(v);
    for(int to : gr[v]) {
        if (color[to] == -1) dfs2(to, c, components);
    }
}

void bfs(int start, vector<vector<int>>& condensedGraph, vector<int>& reachable) {
    queue<int> q;
    q.push(start);
    reachable[start] = 1;

    while (!q.empty()) {
        int v = q.front();
        q.pop();

        for (int to : condensedGraph[v]) {
            if (!reachable[to]) {
                reachable[to] = 1;
                q.push(to);
            }
        }
    }
}

int main() {
    int n, m, a, b;
    cin >> n >> m;

    g.resize(n);
    gr.resize(n);
    in_component.assign(n, -1);

    for(int i = 0; i < m; i++) {
        cin >> a >> b;
        g[a].push_back(b);
        gr[b].push_back(a);
    }

    used.assign(n, 0);
    for(int i = 0; i < n; i++)
        if (!used[i]) dfs1(i);

    color.assign(n, -1);
    int c = 0;
    vector<vector<int>> components(n);
    for(int i = 0; i < n; i++) {
        int v = top[n-1-i];
        if (color[v] == -1) {
            dfs2(v, c++, components);
        }
    }

    vector<vector<int>> condensedGraph(c); 
    set<pair<int, int>> condensedEdges; 

    for(int i = 0; i < n; i++) {
        for(int to : g[i]) {
            if (color[i] != color[to]) {
                condensedEdges.insert(make_pair(color[i], color[to]));
            }
        }
    }

    for(auto edge : condensedEdges) {
        condensedGraph[edge.first].push_back(edge.second);
    }

    cout << "Strongly Connected Components:\n";
    for(int i = 0; i < c; i++) {
        cout << "Component " << i << ": ";
        for(int v : components[i]) {
            cout << v << " ";
            in_component[v] = i;
        }
        cout << "\n";
    }

    
    cout << "\nCondensed Graph:\n";
    for(int i = 0; i < c; i++) {
        cout << "Component " << i << " -> ";
        for(int v : condensedGraph[i]) {
            cout << "Component " << v << " ";
        }
        cout << "\n";
    }
    

    cout << "\nБаза конденсации:\n";
    vector<int> condensationBasis;
    for (int i = 0; i < c; i++) {
        bool isBaseVertex = true;
        int minVertex = numeric_limits<int>::max();
        for (int vertex : components[i]) {
            minVertex = min(minVertex, vertex);
        }
        for (int j = 0; j < c; j++) {
            if (i != j) {
                for (int v : condensedGraph[j]) {
                    if (v == i) {
                        isBaseVertex = false;
                        break;
                    }
                }
            }
            if (!isBaseVertex) break;
        }
        if (isBaseVertex) {
            condensationBasis.push_back(minVertex);
        }
    }
    
    sort(condensationBasis.begin(), condensationBasis.end());
    
    for (int vertex : condensationBasis) {
        cout << vertex << " ";
    }
    cout << "\n";

    return 0;
}



/*
5
5
1 0    0 4    3 0    4 1    2 4

2 3
*/

/*
https://site.ada.edu.az/~medv/acm/Docs%20e-olimp/Volume%2020/1947.htm

1. найти все компоненты сильной связности орграфа;
2. построить его конденсацию;
3. найти базу конденсации;
4. из каждой компоненты сильной связности, образующей вершину базы конденсации, взять по одной вершине.
*/

/*
22
33
0  8
1  3
1 10
2 11
2 13
3 14
4  6
4 16
5 17
6 19
8  1
8  9
9  0
9  2
10  1
10  4
11 12
12  2
12  4
13  5
13 12
14 15
15  3
15  6
16  4
16  7
17  7
17 18
18  5
19  6
20 21
21 18
21 20
*/
