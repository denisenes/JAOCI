double sqrt(double x) 
{
    double s, last;

    if (x > 0.0) {
        if (x > 1.0) s = x;  else s = 1.0;
	last = s;  s = (x / s + s) / 2.0;
        while (s < last) {
            last = s;  s = (x / s + s) / 2.0;
        }
        return s;
    } else {
    	if (x != 0.0) { 
		return 0.0-1.0;
	} else {
    		return 0.0;
	}
    }
}

double sqr(double x) {
	return x * x;
}

double  main() {
	
	double x0, y0, z0;
	double x1, y1, z1;
	
	x0 = 0.0; y0 = 0.0; z0 = 0.0;
	x1 = 10.0; y1 = 15.0; z1 = 20.0;
	
	double d = sqr(x1 - x0) + sqr(y1 - y0) + sqr(z1 - z0);
	d = sqrt(d);
	return d;
}
