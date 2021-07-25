int test(int i) {
	i = i + 1;
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
