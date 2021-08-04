double sqr(double a) {
	return a * a;
}

int main() {
	double r = 10.0;
	double pi = 3.14;

	double s;
        s = pi * sqr(r);

	//ATTENTION!!!...
	bool flag = true;
	int i = sqr_int(flag); 

	if (s > 0.0001) {
		return 0;
	} else {
		return 1;
	}
}

int sqr_int(int a) {
	return a * a;
}
