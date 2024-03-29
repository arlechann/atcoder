#include <cmath>
#include <utility>

const double PI = acos(-1.0);
const double EPS = 1e-10;

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
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

	constexpr double x() { return this->_x; }
	constexpr double y() { return this->_y; }

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
enum class CCW : int {
	Clockwise,
	CounterClockwise,
	ABC,
	ACB,
	CAB,
};

constexpr CCW ccw(Vec2d a, Vec2d b, Vec2d c) {
	Vec2d ab = b - a;
	Vec2d ac = c - a;
	int det = ab.det(ac);
	if(det > 0) {
		return CCW::CounterClockwise; // 反時計回り
	}
	if(det < 0) {
		return CCW::Clockwize; // 時計回り
	}
	if(ab.dot(ac) < 0) {
		return CCW::CAB; // c-a-b
	}
	if(ab.length() < ac.length()) {
		return CCW::ABC; // a-b-c
	}
	return CCW::ACB; // a-c-b
}

// 垂線の足
constexpr Vec2d projection(const Vec2d f, const Vec2d t, const Vec2d p) {
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
	constexpr Circle(const Vec2d c, const double r) : _center(c), _radius(r) {}

	constexpr Vec2d center() const { return this->_center; }
	constexpr double radius() const { return this->_radius; }

	// 内部判定
	constexpr int inside(const Vec2d p) const {
		return sign((p - this->center()).length() -
					this->radius()); // inside: 1, on edge: 0, outside: -1
	}
};
