#include <array>
#include <vector>

class DynamicModInt {
	public:
	long long n;
	long long mod;

	DynamicModInt() : n(0), mod(1000000007) {}
	DynamicModInt(long long n, long long mod)
		: n(n < 0 ? n + mod : n % mod), mod(mod) {}

	long long get() const { return this->n; }
	long long get_mod() const { return this->mod; }

	DynamicModInt pow(int n) const {
		DynamicModInt a = *this;
		DynamicModInt ret(1, this->mod);
		while(n != 0) {
			if(n % 2) {
				ret *= a;
			}
			a *= a;
			n /= 2;
		}
		return ret;
	}

	DynamicModInt inv() const { return this->pow(this->mod - 2); }

	DynamicModInt& operator=(const long long rhs) {
		return *this = DynamicModInt(rhs, this->mod);
	}
	DynamicModInt& operator+=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n + rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator-=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n - rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator*=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n * rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator/=(const DynamicModInt rhs) {
		return *this *= rhs.inv();
	}
	bool operator==(const DynamicModInt rhs) const {
		return this->mod == rhs.get_mod() && this->n == rhs.n;
	}
	bool operator!=(const DynamicModInt rhs) const { return !(*this == rhs); }
};

DynamicModInt operator+(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) += rhs;
}
DynamicModInt operator+(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) += DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator+(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) += rhs;
}

DynamicModInt operator-(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) -= rhs;
}
DynamicModInt operator-(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) -= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator-(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) -= rhs;
}

DynamicModInt operator*(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) *= rhs;
}
DynamicModInt operator*(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) *= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator*(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) *= rhs;
}

DynamicModInt operator/(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) /= rhs;
}
DynamicModInt operator/(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) /= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator/(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) /= rhs;
}

std::ostream& operator<<(std::ostream& os, const DynamicModInt& x) {
	return os << x.n;
}

std::istream& operator>>(std::istream& is, DynamicModInt& x) {
	return is >> x.n;
}

using ModInt = DynamicModInt;