#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <algorithm>
#include <iomanip>
#include <set>
#include <map>

using namespace std;

vector<int> degrees;
vector<int> alphas;
int n, p, d, r, k;

int deg(int x) {
    int ans = 0;
    while (x > 0) {
        x >>= 1;
        ans++;
    }
    return ans;
}

int mul(int a, int b) {
    return a * b == 0 ? 0 : alphas[(degrees[a] + degrees[b]) % n];
}

void trim(vector<int> &v) {
    while (v.back() == 0 && !v.empty()) {
        v.pop_back();
    }
}

vector<int> mul(vector<int> &a, vector<int> &b) {
    vector<int> c(a.size() + b.size() - 1);
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < b.size(); j++) {
            c[i + j] ^= mul(a[i], b[j]);
        }
    }
    trim(c);
    return c;
}

vector<int> mod(vector<int> a, vector<int> &b) {
    trim(a);
    while (true) {
        int delta = a.size() - b.size();
        if (delta < 0) {
            return a;
        }
        for (int i = 0; i < b.size(); i++) {
            a[i + delta] ^= b[i];
        }
        trim(a);
    }
}

int inv(int x) {
    if (x == 1) {
        return 1;
    }

    return alphas[n - degrees[x]];
}

// c(x) = x^r * m(x) + (x^r * m(x) mod g(x))
vector<int> encode(vector<int> &m, vector<int> &g) {
    vector<int> c(n);
    for (int i = 0; i < m.size(); i++) {
        c[i + r] = m[i];
    }
    vector<int> t = mod(c, g);
    for (int i = 0; i < r; i++) {
        c[i] = t[i];
    }
    return c;
}

int pow(int alpha, int x) {
    int i = degrees[alpha];
    return alphas[(i * x) % n];
}

int calculate(vector<int> &m, int alpha) {
    int acc = 0;
    for (int j = 0; j < m.size(); j++) {
        acc ^= mul(pow(alpha, j), m[j]);
    }
    return acc;
}

vector<int> decode(vector<int> &m) {
    vector<int> syndrome(1, 0);
    for (int i = 1; i <= d - 1; i++) {
        syndrome.push_back(calculate(m, alphas[i]));
    }
    vector<int> lambda(1, 1);
    vector<int> b(1, 1);
    int L = 0;

    for (int i = 1; i <= d - 1; i++) {
        int delta = 0;
        for (int j = 0; j <= L && j < lambda.size(); j++) {
            delta ^= mul(lambda[j], syndrome[i - j]);
        }
        b.push_back(0);
        for (int j = b.size() - 1; j > 0; j--) {
            b[j] = b[j - 1];
        }
        b[0] = 0;
        if (delta != 0) {
            vector<int> t(max(lambda.size(), b.size()));
            for (int j = 0; j < b.size(); j++) {
                t[j] ^= mul(b[j], delta);
            }
            for (int j = 0; j < lambda.size(); j++) {
                t[j] ^= lambda[j];
            }
            if (2 * L <= i - 1) {
                delta = inv(delta);
                b = lambda;
                for (int j = 0; j < b.size(); j++) {
                    b[j] = mul(b[j], delta);
                }
                L = i - L;
            }
            lambda = t;
            trim(lambda);
        }
    }

    for (int i = 0; i < n; i++) {
        if (calculate(lambda, inv(alphas[i])) == 0) {
            m[i] ^= 1;
        }
    }

    return m;
}

double simulate(double noise, int iterations, int max_errors, vector<int> &g) {
    random_device rd{};
    mt19937 gen{rd()};
    uniform_int_distribution<mt19937::result_type> uniform(0, 1);
    uniform_real_distribution<> dist(0, 1);
    int mistakes = 0;
    int i = 0;
    for (; i < iterations; i++) {
        vector<int> simulated(k);
        for (int j = 0; j < k; j++) {
            simulated[j] = uniform(gen);
        }
        vector<int> encoded = encode(simulated, g);
        vector<int> encoded_noise = encoded;
        for (int j = 0; j < encoded_noise.size(); j++) {
            if (dist(gen) <= noise) {
                encoded_noise[j] ^= 1;
            }
        }
        vector<int> decoded = decode(encoded_noise);

        if (decoded != encoded) {
            mistakes++;
        }

        if (mistakes == max_errors) {
            break;
        }
    }

    return (double) mistakes / i;
}

int main() {
    ifstream input("input.txt");
    ofstream output("output.txt");

    ios_base::sync_with_stdio(false);
    input.tie(NULL);

    input >> n >> p >> d;
    int dp = deg(p);

    degrees.resize(n + 1, 0);
    alphas.resize(n, 0);

    for (int i = 0, alpha = 1; i < n; i++) {
        degrees[alpha] = i;
        alphas[i] = alpha;

        alpha <<= 1;
        for (int da = deg(alpha); da >= dp; da = deg(alpha)) {
            alpha ^= p << (da - dp);
        }
    }

    set<int> used;
    map<int, set<int>> classes;

    for (int i = 0; i < n - 1; i++) {
        int m = i;
        set<int> c;
        int j = i;
        bool add = false;
        while (used.find(j) == used.end()) {
            m = min(j, m);
            used.insert(j);
            c.insert(j);
            j = (2 * j) % n;
            add = true;
        }

        if (add) {
            classes[m] = c;
        }
    }

    vector<int> g(1, 1);

    for (auto const &[key, val]: classes) {
        if (key >= 1 && key <= d - 1) {
            for (int j: val) {
                vector<int> m(2, 1);
                m[0] = alphas[j];
                g = mul(g, m);
            }
        }
    }

    r = g.size() - 1;
    k = n - r;
    output << k << endl;

    for (int i: g) {
        output << i << " ";
    }
    output << endl;

    string command;

    while (input >> command) {
        if (command == "Encode") {
            vector<int> m(k);
            for (int i = 0; i < k; i++) {
                input >> m[i];
            }
            for (int i: encode(m, g)) {
                output << i << " ";
            }
            output << endl;
        }

        if (command == "Decode") {
            vector<int> m(n);
            for (int i = 0; i < n; i++) {
                input >> m[i];
            }
            for (int i: decode(m)) {
                output << i << " ";
            }
            output << endl;
        }

        if (command == "Simulate") {
            double noise;
            int iterations, max_errors;
            input >> noise >> iterations >> max_errors;
            output << fixed << simulate(noise, iterations, max_errors, g)
                   << '\n';
        }
    }
    input.close();
    output.close();
    return 0;
}
