int gcd(int a, int b) {
	if(a < b) {
		int tmp = a;
		a = b;
		b = tmp;
	}

	int r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

int lcm(int a, int b) {
	return a * b / gcd(a, b); // オーバーフロー注意
}