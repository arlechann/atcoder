#include <cstdio>

int main(void) {
	char s[5];
	scanf("%s", s);
	int result = 0;
	for(int i = 0; i < 4; i++) {
		for(int j = i + 1; j < 4; j++) {
			result += s[i] == s[j] ? 1 : 0;
		}
	}

	printf(result == 2 ? "Yes\n" : "No\n");
	return 0;
}