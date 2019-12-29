#include <cmath>
#include <utility>

const double PI = acos(-1.0);
const double EPS = 1e-10;

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

double deg_to_rad(double deg) {
	return PI * deg / 180.0;
}

double rad_to_deg(double rad) {
	return rad * 180 / PI;
}

// 二次元ベクトルクラス
class Vec2d {
	double _x;
	double _y;

	public:
	// 原点の位置ベクトル
	static Vec2d origin() { return Vec2d(0.0, 0.0); }

	// ゼロベクトル
	static Vec2d zero() { return Vec2d::origin(); }

	Vec2d(double a, double b) : _x(a), _y(b) {}

	double x() const { return this->_x; }
	double y() const { return this->_y; }

	// 外積
	double det(const Vec2d& rhs) const {
		return this->x() * rhs.y() - this->y() * rhs.x();
	}

	// 内積
	double dot(const Vec2d& rhs) const {
		return this->x() * rhs.x() + this->y() * rhs.y();
	}

	// 長さ
	double length() const { return this->distance(Vec2d::origin()); }

	// 2つの位置ベクトル間のユークリッド距離
	double distance(const Vec2d& rhs) const {
		return sqrt(square(this->x() - rhs.x()) + square(this->y() - rhs.y()));
	}

	// 2つの位置ベクトル間のマンハッタン距離
	double manhattan_distance(const Vec2d& rhs) const {
		return abs(this->x() - rhs.x()) + abs(this->y() - rhs.y());
	}

	// ベクトルとx軸のなす角
	double argument() const { return atan2(this->y(), this->x()); }

	// 反時計回りに回転したベクトル
	Vec2d rotate(double rad) const {
		double xx = this->x();
		double yy = this->y();
		return Vec2d(xx * cos(rad) - yy * sin(rad),
					 xx * sin(rad) + yy * cos(rad));
	}

	// 単位ベクトル
	Vec2d unit() const {
		double len = this->length();
		return Vec2d(this->x() / len, this->y() / len);
	}

	// 法線ベクトル
	Vec2d normal() const {
		double len = this->length();
		return Vec2d(this->y() / len, -this->x() / len);
	}

	// 平行判定
	bool is_parallel(const Vec2d& rhs) const { return false; }

	// ベクトル和
	Vec2d operator+() const { return *this; }
	Vec2d operator+(const Vec2d& rhs) const {
		return Vec2d(this->x() + rhs.x(), this->y() + rhs.y());
	}

	// ベクトル差
	Vec2d operator-() const { return Vec2d(-this->x(), -this->y()); }
	Vec2d operator-(const Vec2d& rhs) const {
		return Vec2d(this->x() - rhs.x(), this->y() - rhs.y());
	}

	// スカラー積
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

// 円クラス
class Circle {
	Vec2d _center;
	double _radius;

	public:
	Circle(const Vec2d c, const double r) : _center(c), _radius(r) {}

	Vec2d center() const { return this->_center; }
	double radius() const { return this->_radius; }

	// 内部判定
	int inside(const Vec2d p) const {
		return sign((p - this->center()).length() -
					this->radius()); // inside: 1, on edge: 0, outside: -1
	}
};
