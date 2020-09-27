#include <algorithm>
#include <boost/optional.hpp>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = std::vector<int>;
using VI2D = std::vector<vector<int>>;
using VLL = std::vector<long long>;
using VLL2D = std::vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T, std::size_t N>
struct make_vector_type {
	using type =
		typename std::vector<typename make_vector_type<T, (N - 1)>::type>;
};

template <typename T>
struct make_vector_type<T, 0> {
	using type = typename std::vector<T>;
};

template <typename T, size_t N>
auto make_vector_impl(const std::vector<std::size_t>& ls, T init_value) {
	if constexpr(N == 0) {
		return std::vector<T>(ls[N], init_value);
	} else {
		return typename make_vector_type<T, N>::type(
			ls[N], make_vector_impl<T, (N - 1)>(ls, init_value));
	}
}

template <typename T, std::size_t N>
auto make_vector(const std::size_t (&ls)[N], T init_value) {
	std::vector<std::size_t> dimensions(N);
	for(int i = 0; i < N; i++) {
		dimensions[N - i - 1] = ls[i];
	}
	return make_vector_impl<T, N - 1>(dimensions, init_value);
}

template <typename T>
std::vector<T> make_vector(std::size_t size, T init_value) {
	return std::vector<T>(size, init_value);
}

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T, typename U>
constexpr void chmax(T& m, U x) {
	m = max<T>(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min<T>(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

int id, n, k;

inline int index(int y, int x) {
	return x + y * n;
}

constexpr TIME_LIMIT_ms = 2950;

struct State {
	VI board;
	int score;
	int operation_count;
};

void initialize(State& s) {
	vector<string> s(n);
	REP(i, n) { cin >> s[i]; }
	s.board.resize(n * n);
	REP(i, n) {
		REP(j, n) {
			int color = s[i][j] - '0';
			s.board[index(i, j)] = color;
		}
	}
}

void operate(State& s) {}

int eval(State& s) {}

pair<VI, int> touch(VI board, int y, int x, int c, int point) {
	int color_pre = board[index(y, x)];
	if(c == color_pre) {
		return {board, point - 1};
	}
	queue<pair<int, int>> q;
	int count = 0;
	q.push({y, x});
	while(!q.empty()) {
		auto p = q.front();
		q.pop();
		if(board[index(p.first, p.second)] != color_pre) {
			continue;
		}
		board[index(p.first, p.second)] = c;
		count++;
		REP(i, 4) {
			if((p.first + dy[i] < 0 || n <= p.first + dy[i]) ||
			   (p.second + dx[i] < 0 || n <= p.second + dx[i])) {
				continue;
			}
			q.push({p.first + dy[i], p.second + dx[i]});
		}
	}
	return {board, point + count * 100 - 1};
}

constexpr q_max = 100000;
int q;
int y[q_max], x[q_max], c[q_max];

int main() {
	cin >> id >> n >> k;

	State state;
	initialize(state);
	double start_temp, end_temp;
	chrono::system_clock::time_point start = chrono::system_clock::now();

	random_device seed_gen;
	mt19937_64 engine(seed_gen());

	while(q <= op_max) {
		chrono::system_clock::time_point end = chrono::system_clock::now();
		double time = static_cast<double>(
			std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
				.count());
		if(time > TIME_LIMIT_ms) {
			break;
		}

		State new_state = state;
		operate(new_state);
		int pre_score = eval(state);
		int new_score = eval(new_state);

		double temp =
			start_temp + (end_temp - start_temp) * time / TIME_LIMIT_ms;
		double prob = exp((new_score - prev_score) / temp);

		if(prob > (engine() % INF / static_cast<double>(INF))) {
			state = new_state;
		}
	}

	cout << q << endl;
	REP(i, q) { cout << y[i] << " " << x[i] << " " << c[i] << endl; }
	return 0;
}
