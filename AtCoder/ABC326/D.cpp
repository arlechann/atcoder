#include <algorithm>
#include <cassert>
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
#define RREP(i, n) for(int i = (n)-1; i >= 0; i--)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define RRANGE(i, a, b) for(int i = (b)-1, i##_MACRO = (a); i >= i##_MACRO; i--)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define CAST(x, t) (static_cast<t>(x))
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;
using VLL = vector<long long>;
using VLL2D = vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr long long INFLL = 2e18;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

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

template <typename T>
constexpr bool chmax(T& m, T x) {
	if(m >= x) {
		return false;
	}
	m = x;
	return true;
}

template <typename T>
constexpr bool chmin(T& m, T x) {
	if(m <= x) {
		return false;
	}
	m = x;
	return true;
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T pow(T a, int n) {
	T ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
		}
		a *= a;
		n /= 2;
	}
	return ret;
}

template <typename T>
constexpr T div_ceil(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return (a + b - 1) / b;
	}
	return a / b;
}

template <typename T>
constexpr T div_floor(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return a / b;
	}
	assert(false);
}

template <typename T>
constexpr bool is_power_of_two(T n) {
	if constexpr(n == std::numeric_limits<T>::min()) {
		return true;
	}
	return (n & (n - 1)) == 0;
}

constexpr std::size_t next_power_of_two(std::size_t n) {
	if((n & (n - 1)) == 0) {
		return n;
	}
	std::size_t ret = 1;
	while(n != 0) {
		ret <<= 1;
		n >>= 1;
	}
	return ret;
}

template <typename T>
constexpr T next_multiple_of(T a, T b) {
	return div_ceil(a, b) * b;
}

template <typename T>
constexpr bool is_mul_overflow(T a, T b) {
	if(a >= 0 && b >= 0) {
		return a > std::numeric_limits<T>::max() / b;
	}
	if(a <= 0 && b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
	if(a < 0) {
		return a > std::numeric_limits<T>::min() / b;
	}
	if(b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
}

template <typename T>
constexpr T diff(T a, T b) {
	return max(a, b) - min(a, b);
}

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

int n;
string r, c;

void show_board(const vector<string>& board) {
	REP(i, n) {
		cout << board[i] << endl;
	}
	cout << endl;
}

inline bool
is_valid(const vector<string>& board, const VI& row_used, const VI& col_used) {
	// show_board(board);
	REP(i, n) {
		VI abc(n, 0);
		REP(j, n) {
			if(board[i][j] != '.') {
				abc[board[i][j] - 'A']++;
			}
		}
		if(abc[0] != 1 || abc[1] != 1 || abc[2] != 1) {
			return false;
		}
	}
	REP(j, n) {
		VI abc(n, 0);
		REP(i, n) {
			if(board[i][j] != '.') {
				abc[board[i][j] - 'A']++;
			}
		}
		if(abc[0] != 1 || abc[1] != 1 || abc[2] != 1) {
			return false;
		}
	}
	return true;
}

bool dfs(vector<string>& board, int row, int col, VI& row_used, VI& col_used) {
	// cout << row << "," << col << endl;
	if(row == n) {
		return is_valid(board, row_used, col_used);
	}

	int next_col = (col + 1) % n;
	int next_row = row + (next_col == 0 ? 1 : 0);

	bool found = dfs(board, next_row, next_col, row_used, col_used);
	if(found) {
		return true;
	}

	bool is_row_all_empty = row_used[row] == 0;
	bool is_col_all_empty = col_used[col] == 0;

	char row_first_char = r[row];
	char col_first_char = c[col];
	int row_first_char_i = r[row] - 'A';
	int col_first_char_i = c[col] - 'A';

	if(is_row_all_empty && is_col_all_empty) {
		if(row_first_char_i != col_first_char_i) {
			return false;
		}
		board[row][col] = row_first_char;
		row_used[row] |= (1 << row_first_char_i);
		col_used[col] |= (1 << row_first_char_i);
		bool found = dfs(board, next_row, next_col, row_used, col_used);
		if(found) {
			return true;
		}
		board[row][col] = '.';
		row_used[row] ^= (1 << row_first_char_i);
		col_used[col] ^= (1 << row_first_char_i);
	} else if(is_row_all_empty) {
		if(((col_used[col] >> row_first_char_i) & 1) != 0) {
			return false;
		}
		board[row][col] = row_first_char;
		row_used[row] |= (1 << row_first_char_i);
		col_used[col] |= (1 << row_first_char_i);
		bool found = dfs(board, next_row, next_col, row_used, col_used);
		if(found) {
			return true;
		}
		board[row][col] = '.';
		row_used[row] ^= (1 << row_first_char_i);
		col_used[col] ^= (1 << row_first_char_i);
	} else if(is_col_all_empty) {
		if(((row_used[row] >> col_first_char_i) & 1) != 0) {
			return false;
		}
		board[row][col] = col_first_char;
		row_used[row] |= (1 << col_first_char_i);
		col_used[col] |= (1 << col_first_char_i);
		bool found = dfs(board, next_row, next_col, row_used, col_used);
		if(found) {
			return true;
		}
		board[row][col] = '.';
		row_used[row] ^= (1 << col_first_char_i);
		col_used[col] ^= (1 << col_first_char_i);
	} else {
		REP(i, 3) {
			char ch = 'A' + i;
			if(((row_used[row] >> i) & 1) != 0 ||
			   ((col_used[col] >> i) & 1) != 0) {
				continue;
			}
			board[row][col] = ch;
			row_used[row] |= (1 << i);
			col_used[col] |= (1 << i);
			bool found = dfs(board, next_row, next_col, row_used, col_used);
			if(found) {
				return true;
			}
			board[row][col] = '.';
			row_used[row] ^= (1 << i);
			col_used[col] ^= (1 << i);
		}
	}
	return false;
}

int main() {
	cin >> n;
	cin >> r;
	cin >> c;

	vector<string> board(n, string(n, '.'));
	VI row_used(n, 0), col_used(n, 0);
	bool result = dfs(board, 0, 0, row_used, col_used);

	if(result) {
		cout << "Yes" << endl;
		show_board(board);
	} else {
		cout << "No" << endl;
	}
	return 0;
}
