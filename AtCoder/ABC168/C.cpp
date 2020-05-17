#include <algorithm>
#include <boost/optional.hpp>
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
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

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
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
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

// 点の進行方向
constexpr int ccw(Vec2d a, Vec2d b, Vec2d c) {
	Vec2d ab = b - a;
	Vec2d ac = c - a;
	int det = ab.det(ac);
	if(det > 0) {
		return 1; // 反時計回り
	}
	if(det < 0) {
		return -1; // 時計回り
	}
	if(ab.dot(ac) < 0) {
		return 2; // c-a-b
	}
	if(ab.normal() < ac.normal()) {
		return -2; // a-b-c
	}
	return 0; // a-c-b
}

int main() {
	double a, b, h, m;
	cin >> a >> b >> h >> m;
	Vec2d hvec(0, a);
	Vec2d mvec(0, b);
	double harg = -deg_to_rad(h * 30 + (m / 2));
	double marg = -deg_to_rad(m * 6);
	Vec2d moved_hour_vec = hvec.rotate(harg);
	Vec2d moved_min_vec = mvec.rotate(marg);
	double dist = moved_min_vec.distance(moved_hour_vec);
	cout << setprecision(16) << dist << endl;
	return 0;
}
