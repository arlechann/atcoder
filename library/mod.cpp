#include <vector>

// 計算量 前処理:O(n) クエリ:O(1)
template <long long MOD = 1000000007>
ModInt<MOD> mod_comb(long long n, long long r) {
	const int COMB_MAX = 1000000;
	static vector<ModInt<MOD>> fact(COMB_MAX);
	static vector<ModInt<MOD>> fact_inv(COMB_MAX);
	static vector<ModInt<MOD>> inv(COMB_MAX);

	if(n < r || n < 0 || r < 0) {
		return ModInt<MOD>(0);
	}

	if(fact[0] == 0) {
		fact[0] = fact[1] = 1;
		fact_inv[0] = fact_inv[1] = 1;
		inv[1] = 1;

		for(int i = 2; i < COMB_MAX; i++) {
			fact[i] = fact[i - 1] * i;
			inv[i] = MOD - (inv[MOD % i] * (MOD / i));
			fact_inv[i] = fact_inv[i - 1] * inv[i];
		}
	}

	return fact[n] * fact_inv[r] * fact_inv[n - r];
}

template <long long MOD = 1000000007>
class ModInt {
	public:
	long long n;

	ModInt() : n(0) {}
	ModInt(long long n) : n(n) {
		while(this->n < 0) {
			this->n += MOD;
		}
		this->n %= MOD;
	}

	long long get() const { return this->n; }
	long long get_mod() const { return MOD; }

	ModInt inv() const { return pow<ModInt<>>(*this, MOD - 2); }

	ModInt& operator=(const long long rhs) { return *this = ModInt(rhs); }
	ModInt& operator+=(const ModInt rhs) {
		return *this = ModInt(this->n + rhs.n);
	}
	ModInt& operator-=(const ModInt rhs) {
		return *this = ModInt(this->n - rhs.n);
	}
	ModInt& operator*=(const ModInt rhs) {
		return *this = ModInt(this->n * rhs.n);
	}
	ModInt& operator/=(const ModInt rhs) { return *this *= rhs.inv(); }
	bool operator==(const ModInt rhs) const { return this->n == rhs.n; }
};

template <long long MOD>
ModInt<MOD> operator+(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
ModInt<MOD> operator+(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
ModInt<MOD> operator+(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}

template <long long MOD>
ModInt<MOD> operator-(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
ModInt<MOD> operator-(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
ModInt<MOD> operator-(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}

template <long long MOD>
ModInt<MOD> operator*(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
ModInt<MOD> operator*(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
ModInt<MOD> operator*(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}

template <long long MOD>
ModInt<MOD> operator/(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
ModInt<MOD> operator/(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
ModInt<MOD> operator/(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}

template <long long MOD>
std::ostream& operator<<(std::ostream& os, const ModInt<MOD>& x) {
	return os << x.n;
}

template <long long MOD>
std::istream& operator>>(std::istream& is, const ModInt<MOD>& x) {
	return is >> x.n;
}
