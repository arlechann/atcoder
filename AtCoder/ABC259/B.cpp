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

constexpr double deg_to_rad(double deg) {
	return PI * deg / 180.0;
}

constexpr double rad_to_deg(double rad) {
	return rad * 180 / PI;
}

// 二次元ベクトルクラス
class Vec2d {
	double _x;
	double _y;

	public:
	// 原点の位置ベクトル
	static constexpr Vec2d origin() { return Vec2d(0.0, 0.0); }

	// ゼロベクトル
	static constexpr Vec2d zero() { return Vec2d::origin(); }

	constexpr Vec2d(double a, double b) : _x(a), _y(b) {}

	constexpr double x() const { return this->_x; }
	constexpr double y() const { return this->_y; }

	// 外積
	constexpr double det(const Vec2d& rhs) const {
		return this->x() * rhs.y() - this->y() * rhs.x();
	}

	// 内積
	constexpr double dot(const Vec2d& rhs) const {
		return this->x() * rhs.x() + this->y() * rhs.y();
	}

	// 長さ
	constexpr double length() const { return this->distance(Vec2d::origin()); }

	// 2つの位置ベクトル間のユークリッド距離
	constexpr double distance(const Vec2d& rhs) const {
		return std::sqrt(square(this->x() - rhs.x()) +
						 square(this->y() - rhs.y()));
	}

	// 2つの位置ベクトル間のマンハッタン距離
	constexpr double manhattan_distance(const Vec2d& rhs) const {
		return std::abs(this->x() - rhs.x()) + std::abs(this->y() - rhs.y());
	}

	// ベクトルとx軸のなす角
	constexpr double argument() const {
		return std::atan2(this->y(), this->x());
	}

	// 反時計回りに回転したベクトル
	constexpr Vec2d rotate(double rad) const {
		double xx = this->x();
		double yy = this->y();
		return Vec2d(xx * cos(rad) - yy * sin(rad),
					 xx * sin(rad) + yy * cos(rad));
	}

	// 単位ベクトル
	constexpr Vec2d unit() const {
		double len = this->length();
		return Vec2d(this->x() / len, this->y() / len);
	}

	// 法線ベクトル
	constexpr Vec2d normal() const {
		double len = this->length();
		return Vec2d(this->y() / len, -this->x() / len);
	}

	// 平行判定
	constexpr bool is_parallel(const Vec2d& rhs) const { return false; }

	// ベクトル和
	constexpr Vec2d operator+() const { return *this; }
	constexpr Vec2d operator+(const Vec2d& rhs) const {
		return Vec2d(this->x() + rhs.x(), this->y() + rhs.y());
	}

	// ベクトル差
	constexpr Vec2d operator-() const { return Vec2d(-this->x(), -this->y()); }
	constexpr Vec2d operator-(const Vec2d& rhs) const {
		return Vec2d(this->x() - rhs.x(), this->y() - rhs.y());
	}

	// スカラー積
	constexpr Vec2d operator*(const double rhs) const {
		return Vec2d(this->x() * rhs, this->y() * rhs);
	}
	constexpr Vec2d operator/(const double rhs) const {
		return Vec2d(this->x() / rhs, this->y() / rhs);
	}

	constexpr bool operator<(const Vec2d& rhs) const {
		return sign(this->x()) ? this->y() < rhs.y() : this->x() < rhs.x();
	}

	private:
};

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

int main() {
	double a, b, d;
	cin >> a >> b >> d;

	Vec2d ab(a, b);
	ab = ab.rotate(deg_to_rad(d));
	cout << PRECISION(15) << ab.x() << " " << ab.y() << endl;
	return 0;
}
