#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <random>
#include <algorithm>
#include <iomanip>

using namespace std;
using bit = int;
using span = pair<int, int>;
using matrix = vector<vector<bit>>;


struct edge {
    int next;
    int value;
};

struct vertex {
    vector<edge> edges;
    int indexes_mask;
    int values_mask;
    long double dist = -1e9;
    int path;
    int pred;
};

int hamming_weight(int n) {
    return __builtin_popcount(n);
}

span get_span(vector<bit> &l) {
    int start = -1;
    int end = -1;
    int n = l.size();

    for (int i = 0; i < n; i++) {
        if (l[i] == 1) {
            start = start == -1 ? i : start;
            end = i;
        }
    }

    return make_pair(start, end);
}

pair<matrix, vector<span>> get_span_form(matrix &g) {
    matrix gspan = g;
    int k = g.size();

    vector<span> spans;

    for (int i = 0; i < k; i++) {
        spans.push_back(get_span(gspan[i]));
    }

    for (;;) {
        bool changed = false;
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < k; j++) {
                span *a = &spans[i];
                span *b = &spans[j];
                if (i != j &&
                    (a->first == b->first && a->second <= b->second || a->first <= b->first && a->second == b->second)) {

                    for (int z = 0; z < gspan[i].size(); z++) {
                        gspan[i][z] ^= gspan[j][z];
                    }
                    spans[i] = get_span(gspan[i]);
                    changed = true;
                }
            }
        }
        if (!changed) {
            return {gspan, spans};
        }
    }
}

int get_value(int active, int state) {
    int value = 0;
    int offset = 0;
    while (active != 0) {
        if ((active & 1) == 1) {
            value |= (state & 1) << offset;
            state >>= 1;
        }
        offset++;
        active >>= 1;
    }
    return value;
}

pair<vector<vertex>, vector<vector<int>>> get_grid(matrix &matrix, vector<span> &spans) {

    vector<int> previous_level;
    vector<int> current_level;

    vector<vertex> grid;
    vector<vector<int>> layers;

    grid.push_back(vertex{.dist=0});
    previous_level.push_back(0);
    layers.push_back(previous_level);

    int k = matrix.size();
    int n = matrix[0].size();

    for (int i = 0; i < n; i++) {
        int active = 0;

        for (int j = 0; j < k; j++) {
            int start = spans[j].first;
            int end = spans[j].second;
            if (i >= start && i < end || i == start && j == end) {
                active |= 1 << j;
            }
        }

        int s = 1 << hamming_weight(active);

        for (int j = 0; j < s; j++) {
            current_level.push_back(grid.size());
            grid.push_back(vertex{.edges=vector<edge>(), .indexes_mask=active, .values_mask=get_value(active, j)});
        }

        int column = 0;
        int shared = grid[previous_level[0]].indexes_mask | grid[current_level[0]].indexes_mask;
        int shift = 0;
        for (int j = 0; j < k; j++) {
            if ((shared & 1) == 1) {
                column |= matrix[j][i] << shift;
            }
            shift++;
            shared >>= 1;
        }

        for (int j: previous_level) {
            int common = grid[j].indexes_mask & active;

            int state = grid[j].values_mask;
            int prev_value = common & state;

            int j_value = grid[j].values_mask;

            for (int q: current_level) {
                int curr_value = common & grid[q].values_mask;
                if (prev_value == curr_value) {
                    int shared_value = grid[q].values_mask | j_value;

                    int bit = hamming_weight(shared_value & column) % 2;
                    grid[j].edges.push_back(edge{.next=q, .value=bit});
                }
            }
        }
        layers.push_back(current_level);

        previous_level = current_level;
        current_level.clear();
    }

    return make_pair(grid, layers);
}

vector<int> decode(vector<long double> &word, vector<vertex> &grid, vector<vector<int>> &layers) {
    for (int i = 0; i < layers.size() - 1; i++) {
        long double a = word[i];
        for (int ind: layers[i]) {
            vertex *u = &grid[ind];
            for (int p = 0; p < u->edges.size(); p++) {
                auto *e = &u->edges[p];
                long double value = e->value == 1 ? -a : a;
                long double dist = value + u->dist;
                if (dist > grid[e->next].dist) {
                    grid[e->next].dist = dist;
                    grid[e->next].path = p;
                    grid[e->next].pred = ind;
                }
            }
        }
    }

    int min_index = -1;
    long double m_dist = -1e9;
    for (int ind: layers.back()) {
        if (grid[ind].dist > m_dist) {
            m_dist = grid[ind].dist;
            min_index = ind;
        }
    }

    vector<int> ans;

    int index = min_index;
    while (index != 0) {
        vertex *pred = &grid[grid[index].pred];
        ans.push_back(pred->edges[grid[index].path].value);
        index = grid[index].pred;
    }

    for (int i = 1; i < grid.size(); i++) {
        grid[i].dist = -1e9;
    }

    std::reverse(ans.begin(), ans.end());
    return ans;
}

vector<int> encode(vector<int> &word, vector<vector<int>> &matrix) {
    vector<int> ans;
    for (int i = 0; i < matrix[0].size(); i++) {
        int a = 0;
        for (int j = 0; j < matrix.size(); j++) {
            a ^= matrix[j][i] * word[j];
        }
        ans.push_back(a);
    }
    return ans;
}

double simulate(
        vector<vector<int>> &matrix,
        vector<vertex> &grid,
        vector<vector<int>> &layers,
        int noise,
        int max_iterations,
        int max_errors
) {
    int k = matrix.size();
    int n = matrix[0].size();
    random_device rd{};
    mt19937 gen{rd()};
    uniform_int_distribution<mt19937::result_type> uniform(0, 1);
    normal_distribution<> normal(
            0,
            sqrt(0.5 * pow(10, ((double) -noise) / 10) * ((double) n / k))
    );

    int mistakes = 0;
    int iterations = 0;
    for (int i = 0; i < max_iterations; i++) {
        iterations++;

        vector<int> simulated;

        for (int j = 0; j < k; j++) {
            simulated.push_back(uniform(gen));
        }
        vector<int> encoded = encode(simulated, matrix);
        vector<long double> encoded_noise;

        for (int j = 0; j < n; j++) {
            encoded_noise.push_back((1 - 2 * encoded[j]) + normal(gen));
        }

        vector<int> decoded = decode(encoded_noise, grid, layers);

        if (decoded != encoded) {
            mistakes++;
        }

        if (mistakes == max_errors) {
            break;
        }
    }

    return (double) mistakes / iterations;
}


int main() {
    ifstream input("input.txt");
    ofstream output("output.txt");

    ios_base::sync_with_stdio(false);
    input.tie(NULL);

    int n, k;

    input >> n >> k;

    matrix g = vector(k, vector<bit>(n));

    for (int i = 0; i < k; i++) {
        for (int j = 0; j < n; j++) {
            input >> g[i][j];
        }
    }

    vector<vector<int>> m = g;

    auto span_form = get_span_form(g);


    auto grid_layers = get_grid(span_form.first, span_form.second);

    for (vector<int> &layers: grid_layers.second) {
        output << layers.size() << " ";
    }
    output << "\n";

    string command;

    for (int i = 0; i < k; i++) {
        for (int j = 0; j < n; j++) {
            cout << g[i][j] << " ";
        }
        cout << endl;
    }

    for (pair<int, int> &v: span_form.second) {
        cout << v.first << " " << v.second << endl;
    }


    while (input >> command) {
        if (command == "Encode") {
            vector<int> in(k);
            for (int i = 0; i < k; i++) {
                input >> in[i];
            }

            for (int i: encode(in, m)) {
                output << i << " ";
            }
            output << "\n";
        }
        if (command == "Decode") {
            vector<long double> in(n);
            for (int i = 0; i < n; i++) {
                input >> in[i];
            }

            for (int i: decode(in, grid_layers.first, grid_layers.second)) {
                output << i << " ";
            }
            output << "\n";
        }

        if (command == "Simulate") {
            int noise, iterations, max_errors;
            input >> noise >> iterations >> max_errors;
            output << fixed << simulate(g, grid_layers.first, grid_layers.second, noise, iterations, max_errors)
                   << '\n';
        }
    }

    input.close();
    output.close();
    return 0;
}