#include <stdio.h>

int main() {
	
	int x = 10;

	{
		int x;
		x = 5;
	}

	if (x > 0) {
		int y = 11;
		y++;
	} else {
		int y = 12;
		y--;
		
	}

	int y, z, x;

	return 0;
}
