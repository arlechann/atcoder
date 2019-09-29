#include <cstdio>

int main(void) {
	char s[5];

	scanf("%s", s);
	for(int i = 0; i < 4; i++) {
		if(s[i] == s[i + 1]) {
			printf("Bad\n");
			return 0;
		}
	}
	printf("Good\n");
	return 0;
}