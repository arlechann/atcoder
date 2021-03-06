#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

double deg_to_rad(double deg) {
	return PI * deg / 180.0;
}

double rad_to_deg(double rad) {
	return rad * 180 / PI;
}

class Vec2d {
	double _x;
	double _y;

	public:
	static Vec2d origin() { return Vec2d(0.0, 0.0); }
	static Vec2d zero() { return Vec2d::origin(); }

	Vec2d(double a, double b) : _x(a), _y(b) {}

	double x() const { return this->_x; }
	double y() const { return this->_y; }

	double det(const Vec2d& rhs) const {
		return this->x() * rhs.y() - this->y() * rhs.x();
	}
	double dot(const Vec2d& rhs) const {
		return this->x() * rhs.x() + this->y() * rhs.y();
	}
	double length() const { return this->distance(Vec2d::origin()); }
	double distance(const Vec2d& rhs) const {
		return sqrt(square(this->x() - rhs.x()) + square(this->y() - rhs.y()));
	}
	double manhattan_distance(const Vec2d& rhs) const {
		return abs(this->x() - rhs.x()) + abs(this->y() - rhs.y());
	}
	double argument() const { return atan2(this->y(), this->x()); }
	Vec2d rotate(double rad) const {
		double xx = this->x();
		double yy = this->y();
		return Vec2d(xx * cos(rad) - yy * sin(rad),
					 xx * sin(rad) + yy * cos(rad));
	}
	Vec2d unit() const {
		double len = this->length();
		return Vec2d(this->x() / len, this->y() / len);
	}
	Vec2d normal() const {
		double len = this->length();
		return Vec2d(this->y() / len, -this->x() / len);
	}

	bool is_parallel(const Vec2d& rhs) const { return false; }

	Vec2d operator+() const { return *this; }
	Vec2d operator+(const Vec2d& rhs) const {
		return Vec2d(this->x() + rhs.x(), this->y() + rhs.y());
	}
	Vec2d operator-() const { return Vec2d(-this->x(), -this->y()); }
	Vec2d operator-(const Vec2d& rhs) const {
		return Vec2d(this->x() - rhs.x(), this->y() - rhs.y());
	}
	Vec2d operator*(const double rhs) const {
		return Vec2d(this->x() * rhs, this->y() * rhs);
	}
	Vec2d operator/(const double rhs) const {
		return Vec2d(this->x() / rhs, this->y() / rhs);
	}
	bool operator<(const Vec2d& rhs) const {
		return sign(this->x()) ? this->y() < rhs.y() : this->x() < rhs.x();
	}

	private:
};

// 点の進行方向
int ccw(Vec2d a, Vec2d b, Vec2d c) {
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

// 垂線の足
Vec2d projection(const Vec2d f, const Vec2d t, const Vec2d p) {
	Vec2d line = t - f;
	double arg = line.argument();
	double height = f.rotate(-arg).y();
	return Vec2d(p.rotate(-arg).x(), height).rotate(arg);
}

class Circle {
	Vec2d _center;
	double _radius;

	public:
	Circle(const Vec2d c, const double r) : _center(c), _radius(r) {}

	Vec2d center() { return this->_center; }
	double radius() { return this->_radius; }

	int inside(const Vec2d v) {
		return sign((v - this->center()).length() -
					this->radius()); // inside: 1, online: 0, outside: -1
	}
};

int main() {
	double p[1000][2];
	scanf("%lf %lf %lf %lf", &p[0][0], &p[0][1], &p[1][0], &p[1][1]);
	Vec2d a(p[0][0], p[0][1]), b(p[1][0], p[1][1]);
	int q;
	scanf("%d", &q);
	REP(i, q) { scanf("%lf %lf", &p[i][0], &p[i][1]); }

	REP(i, q) {
		Vec2d t = projection(a, b, Vec2d(p[i][0], p[i][1]));
		printf("%.9f %.9f\n", t.x(), t.y());
	}
	return 0;
}