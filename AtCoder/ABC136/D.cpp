#include <algorithm>
#include <cstdio>
#include <cstring>

#define REP(i, n) for(int i = 0; i < (n); i++)
#define DEBUG printf("r_num: %d  l_num: %d\n", r_num, l_num)

int result[100000];

int main() {
	char s[100001];
	scanf("%s", s);

	int n = strlen(s);

	int r_num, l_num;
	r_num = l_num = 0;
	int border;
	for(int i = 0; i < n; i++) {
		if(s[i] == 'R') {
			if(i != 0 && s[i - 1] == 'L') {
				// DEBUG;
				//ここに計算
				if((r_num + l_num) % 2) {
					if(r_num % 2) {
						result[border] = (r_num + l_num) / 2 + 1;
						result[border + 1] = (r_num + l_num) / 2;
					} else {
						result[border] = (r_num + l_num) / 2;
						result[border + 1] = (r_num + l_num) / 2 + 1;
					}
				} else {
					result[border] = result[border + 1] = (r_num + l_num) / 2;
				}
				r_num = l_num = 0;
			}
			r_num++;
		} else {
			if(s[i - 1] == 'R') {
				border = i - 1;
			}
			l_num++;
		}
	}
	// DEBUG;
	//ここにもコピペ
	if((r_num + l_num) % 2) {
		if(r_num % 2) {
			result[border] = (r_num + l_num) / 2 + 1;
			result[border + 1] = (r_num + l_num) / 2;
		} else {
			result[border] = (r_num + l_num) / 2;
			result[border + 1] = (r_num + l_num) / 2 + 1;
		}
	} else {
		result[border] = result[border + 1] = (r_num + l_num) / 2;
	}

	REP(i, n) { printf("%d ", result[i]); }
	putchar('\n');

	return 0;
}