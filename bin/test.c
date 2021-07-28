int test(int i, int j) {
	i = i + 1;
	j = 2;
	i = i + j;
	return i;
}

int main () {
	int a = 10;
	int b = 11;
	a = a + b;
	if (a > 0) {
		test(a);
	} else {
		test(b);
	}
	return 0;
}

void nothing() {
	int aboba;
	aboba = 1;
}
