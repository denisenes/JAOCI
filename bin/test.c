int test(int i, int j) {
	i = i + 1;
	j = 2;
	i = i + j;
	return i;
}

int main (int argc) {
	int a = 10;
	int b = 11;
	a = a + b;
	if (a > 0) {
		test(a, b);
	} else {
		test(b, a);
	}
	double fortnite = 4.0;
	double balls = 2.0;
	nothing(fortnite, balls);
	a = nothing(fortnite, balls) + test(a, b); 
	return 0;
}

int nothing(double kuku, double kuku2) {
	double aboba = (++kuku) + (kuku2++);
	int sassy = aboba * kuku + (kuku2 * true);
	aboba = 100.0;
	return 1;
}
