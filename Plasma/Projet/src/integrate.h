#ifndef INTEGRATE_H
#define INTEGRATE_H

typedef unsigned int uint;

template <typename in = double, typename out = double>
class Integrate
{
	double operator()(out f(in), in a, in b, uint N=128)
	{
		for (uint i = 0; i < N; ++i)
		{
			// TODO Write the code to perform the
			//  integration
		}
	}
};

#endif // INTEGRATE_H
