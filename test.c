#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//打印复数
void printf_imaginary(char *buf, double real_part, double imaginary_part)
{
	if(fabs(imaginary_part) < 0.000001)
	{
		printf("%s %lf\n", buf, real_part);
	}
	else if(imaginary_part > 0)
	{
		printf("%s %lf + %lfi\n", buf, real_part, fabs(imaginary_part));
	}
	else if(imaginary_part < 0)
	{
		printf("%s %lf - %lfi\n", buf, real_part, fabs(imaginary_part));
	}
}

//开立方
double calculate_pow1_3(double x)
{
	double val = 0;

	if(x > 0)
	{
		val = pow(x, 1.0/3);
	}
	else if(x < 0)
	{
		val = 0 - pow(0-x, 1.0/3);
	}
	else
	{
		val = 0;
	}
	return val;
}

//一般三次方程的谢国芳求根方程
double cubic_equation_of_one_unknown(double a, double b, double c, double d)
{
	double real_part[3] = {0};
	double imaginary_part[3] = {0};
	double discriminant = 0, discriminant_sqrt = 0;
	double key_real = 0, k = 0, angle = 0, temp = 0;
	const double pi = acos(-1);

	discriminant = pow(b, 2) - 3*a*c;
	//printf("discriminant = %lf\n", discriminant);
	discriminant_sqrt = sqrt(fabs(discriminant));
	//1: discriminant = 0, 方程有一个实根和两个共轭虚根或一个三重实根
	if(fabs(discriminant) < 0.00000001)
	{
		if(fabs(pow(b, 3) - 27*pow(a, 2)*d) < 0.000000001)
		{
			//printf("1: 此方程有一个三重实根\n");
			real_part[0] = 0 - b/(3*a);
			real_part[1] = real_part[0];
			real_part[2] = real_part[0];
			imaginary_part[0] = 0;
			imaginary_part[1] = 0;
			imaginary_part[2] = 0;
		}
		else
		{
			//printf("1: 此方程有一个实根和两个共轭虚根\n");
			temp = calculate_pow1_3(pow(b, 3) - 27*pow(a, 2)*d);
			real_part[0] = (temp - b)/(3*a);
			real_part[1] = (0 - b - temp/2)/(3*a);
			real_part[2] = real_part[1];
			imaginary_part[0] = 0;
			imaginary_part[1] = sqrt(3)*temp/(6*a);
			imaginary_part[2] = 0 - sqrt(3)*temp/(6*a);
		}
	}
	else
	{
		key_real = (9*a*b*c - 2*pow(b, 3) - 27*pow(a, 2)*d)/(2*pow(discriminant_sqrt, 3));
		//printf("key_real = %lf\n", key_real);
		//2: discriminant < 0, 方程有一个实根和两个共轭虚根
		if(discriminant < 0)
		{
			//printf("2: 此方程有一个实根和两个共轭虚根\n");
			k = calculate_pow1_3((key_real + sqrt(pow(key_real, 2) + 1)));
			real_part[0] = (discriminant_sqrt*(k - 1.0/k) - b)/(3*a);
			real_part[1] = (0 - discriminant_sqrt*(k - 1.0/k)/2 - b)/(3*a);
			real_part[2] = real_part[1];
			imaginary_part[0] = 0;
			imaginary_part[1] = discriminant_sqrt*sqrt(3)*(k + 1.0/k)/(6*a);
			imaginary_part[2] = 0 - imaginary_part[1];
		}
		//3: discriminant > 0, |key_real| = 1, 方程有两个相等的实根和一个与之不等的实根
		else if((discriminant > 0)
			&&(fabs((fabs(key_real) - 1))<0.000000001))
		{
			//printf("3: 此方程有两个相等的实根和一个与之不等的实根\n");
			if(key_real > 0)//key_real = 1
			{
				real_part[0] = (2*discriminant_sqrt - b)/(3*a);
				real_part[1] = (0 - discriminant_sqrt - b)/(3*a);
				real_part[2] = real_part[1];
			}
			else//key_real = -1
			{
				real_part[0] = (0 - 2*discriminant_sqrt - b)/(3*a);
				real_part[1] = (discriminant_sqrt - b)/(3*a);
				real_part[2] = real_part[1];
			}
			imaginary_part[0] = 0;
			imaginary_part[1] = 0;
			imaginary_part[2] = 0;
		}
		//4: discriminant > 0, |key_real| < 1, 方程有三个互异的实根
		else if((discriminant > 0)
			&&(fabs(key_real) < 1))
		{
			//printf("4: 此方程有三个互异的实根\n");
			angle = acos(key_real);
			real_part[0] = (0 - b + discriminant_sqrt*2*cos(angle/3))/(3*a);
			real_part[1] = (0 - b + discriminant_sqrt*2*cos(angle/3 + 2*pi/3))/(3*a);
			real_part[2] = (0 - b + discriminant_sqrt*2*cos(angle/3 - 2*pi/3))/(3*a);
			imaginary_part[0] = 0;
			imaginary_part[1] = 0;
			imaginary_part[2] = 0;
		}
		//5: discriminant > 0, |key_real| > 1, 方程有一个实根和两个共轭虚根
		else if((discriminant > 0)
			&&(fabs(key_real) > 1))
		{
			//printf("5: 此方程有一个实根和两个共轭虚根\n");
			k = calculate_pow1_3((key_real + sqrt(pow(key_real, 2) - 1)));
			real_part[0] = (discriminant_sqrt*(k + 1.0/k) - b)/(3*a);
			real_part[1] = (0 - discriminant_sqrt*(k + 1.0/k)/2 - b)/(3*a);
			real_part[2] = real_part[1];
			imaginary_part[0] = 0;
			imaginary_part[1] = discriminant_sqrt*sqrt(3)*(k - 1.0/k)/(6*a);
			imaginary_part[2] = 0 - imaginary_part[1];
		}
	}
	printf("a = %lf; b = %lf; c = %lf; d = %lf\n", a, b, c, d);
	printf_imaginary("\tx1 =", real_part[0], imaginary_part[0]);
	printf_imaginary("\tx2 =", real_part[1], imaginary_part[1]);
	printf_imaginary("\tx3 =", real_part[2], imaginary_part[2]);
	printf("\n");
	return real_part[0];
}

int main(int argc, char *argv[])
{
	cubic_equation_of_one_unknown(atof(argv[1]), atof(argv[2]), atof(argv[3]), atof(argv[4]));
	return 0;
}

