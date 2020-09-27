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

unsigned int xor128() {
	static unsigned int x = 123456789;
	static unsigned int y = 362436069;
	static unsigned int z = 521288629;
	static unsigned int w = 88675123;
	unsigned int t;
	t = x ^ (x << 11);
	x = y;
	y = z;
	z = w;
	return w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));
}

int id, n, k;

inline int index(int y, int x) {
	return x + y * n;
}

constexpr int TIME_LIMIT_ms = 2800;
constexpr int q_max = 300;
int q;
int y[q_max], x[q_max], c[q_max];

struct State {
	VI x;
	VI y;
	VI c;
	int q;
};

void initialize(State& s) {
	s.x.resize(q_max);
	s.y.resize(q_max);
	s.c.resize(q_max);
	fill(ALL(s.x), n / 2);
	fill(ALL(s.y), n / 2);
	EACH(c, s.c) { c = xor128() % k + 1; }
}

void operate(State& s) {
	int op_q = xor128() % q_max;
	// int op_x = xor128() % n;
	// int op_y = xor128() % n;
	int op_c = xor128() % k + 1;
	// s.x[op_q] = op_x;
	// s.y[op_q] = op_y;
	s.c[op_q] = op_c;
}

void touch(VI& board, int op_y, int op_x, int op_c) {
	int pre_c = board[index(op_y, op_x)];
	if(op_c == pre_c) {
		return;
	}
	queue<pair<int, int>> q;
	q.push({op_y, op_x});
	while(!q.empty()) {
		auto p = q.front();
		q.pop();
		if(board[index(p.first, p.second)] != pre_c) {
			continue;
		}
		board[index(p.first, p.second)] = op_c;
		REP(i, 4) {
			if((p.first + dy[i] < 0 || n <= p.first + dy[i]) ||
			   (p.second + dx[i] < 0 || n <= p.second + dx[i])) {
				continue;
			}
			q.push({p.first + dy[i], p.second + dx[i]});
		}
	}
}

int eval(State& s, VI board) {
	int result = -INF;

	REP(i, q_max) {
		touch(board, s.y[i], s.x[i], s.c[i]);
		VI count(k + 1, 0);
		REP(i, n) {
			REP(j, n) { count[board[index(i, j)]]++; }
		}
		int score = *max_element(ALL(count)) * 100 - (i + 1);
		if(score > result) {
			result = score;
			s.q = i;
		}
	}
	return result;
}

void sa(VI& board) {
	State state;
	initialize(state);
	int pre_score = eval(state, board);

	double start_temp, end_temp;
	start_temp = 5;
	end_temp = 5;

	int best_score = pre_score;
	State best_state = state;

	chrono::system_clock::time_point start = chrono::system_clock::now();

	while(true) {
		chrono::system_clock::time_point end = chrono::system_clock::now();
		double time = static_cast<double>(
			std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
				.count());
		if(time > TIME_LIMIT_ms) {
			break;
		}

		State new_state = state;
		operate(new_state);
		int new_score = eval(new_state, board);

		double temp =
			start_temp + (end_temp - start_temp) * time / TIME_LIMIT_ms;
		double prob = exp((new_score - pre_score) / temp);

		if(prob > (xor128() % INF / static_cast<double>(INF))) {
			// cout << "score: " << pre_score << " => " << new_score << '\n';
			state = new_state;
			pre_score = new_score;
			if(new_score > best_score) {
				best_score = new_score;
				best_state = new_state;
			}
		}
	}

	cout << best_state.q << '\n';
	REP(i, best_state.q) {
		cout << best_state.x[i] << " " << best_state.y[i] << " "
			 << best_state.c[i] << '\n';
	}
	// cout << "best score: " << best_score << '\n';
}

int main() {
	cin >> id >> n >> k;

	vector<string> s(n);
	REP(i, n) { cin >> s[i]; }
	VI board(n * n);
	REP(i, n) {
		REP(j, n) {
			int color = s[i][j] - '0';
			board[index(i, j)] = color;
		}
	}

	sa(board);
	return 0;
}
