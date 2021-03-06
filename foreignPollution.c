/* License: GPL v2 or later */

// I wrote this in C because the Haskell version I had wasn't being fast
// enough for me.  Now this is simple and works great!

#include <math.h>
#include <stdlib.h>
#include <inttypes.h>
#include <GL/gl.h>
#include <stdio.h>
#include "foreignPollution.h"

void foreignPollution(uint32_t randSeed, const double * pollution, uint32_t pollutionWidth, uint32_t pollutionHeight) {
	uint32_t x, y;
	const uint32_t negative1AsXYtype = -1;
	#define AT(array,x,y) ( (array)[((x)*(array##Height))+((y))] )
	//Glibc's parameters, seems to work fine. See wikipedia
	//http://en.wikipedia.org/wiki/Linear_congruential_generator
	#define RAND() ( randSeed = (((randSeed * 1103515245u) + 12345u) & 0x7fffffffu) )
	#define RAND_FLOAT_UP_TO(hi) ( (double)RAND() * (hi) / 0x7fffffffu )
	#define RAND_FLOAT_BETWEEN(lo,hi) ( (lo) + (double)RAND() * ((hi)-(lo)) / 0x7fffffffu )
	glPushMatrix();
	// To log pollution amounts to stderr (enable by changing the 'if').
	if(0) {
		for(y = pollutionHeight-1; y != negative1AsXYtype; --y) {
			for(x = 0; x != pollutionWidth; ++x) {
				const int roundedPollution = AT(pollution,x,y);
				const char indicator =
					( (roundedPollution < 10)
					  ? ( '0' + roundedPollution      )
					  : ( 'a' + (roundedPollution-10) ));
				fputc(indicator, stderr);
			}
			printf("\n");
		}
		printf("\n");
	}
	glScaled(1.0/3.0, 1.0/3.0, 1);
	glTranslated(-0.5, -0.5, 0.0);
	glBegin(GL_QUADS);
		const uint32_t deterministicHeavinessWidth = pollutionWidth + 2;
		const uint32_t deterministicHeavinessHeight = pollutionHeight + 2;
		double * deterministicHeaviness = (double*)alloca(deterministicHeavinessWidth*deterministicHeavinessHeight*sizeof(double));
		const uint32_t minX = 0;
		const uint32_t minY = 0;
		const uint32_t maxX = deterministicHeavinessWidth - 1;
		const uint32_t maxY = deterministicHeavinessHeight - 1;
		for(x = 0; x != deterministicHeavinessWidth; ++x) for(y = 0; y != deterministicHeavinessHeight; ++y) {
			//should probably move in 1 to find border pollution amounts
			//...but NOT segfault :-)
			const int is_border = (x == minX || y == minY || x == maxX || y == maxY);
			const double pollution_here = (is_border ? 0.0 : AT(pollution,x-1,y-1));
			//Maybe logarithm-of-pollution? (log ( 1 + pollution_here))
			AT(deterministicHeaviness,x,y) = pollution_here;
		}
		const uint32_t drawnWidth = pollutionWidth * 3 + 1;
		const uint32_t drawnHeight = pollutionHeight * 3 + 1;
		for(x = 0; x != drawnWidth; ++x) for(y = 0; y != drawnHeight; ++y) {
			const double heaviness1 = AT(deterministicHeaviness,(x+2)/3,(y+2)/3);
			const double heaviness2 = AT(deterministicHeaviness,(x+3)/3,(y+2)/3);
			const double heaviness3 = AT(deterministicHeaviness,(x+3)/3,(y+3)/3);
			const double heaviness4 = AT(deterministicHeaviness,(x+2)/3,(y+3)/3);
		//	if(heaviness1 > 1 || heaviness2 > 1 || heaviness3 > 1 || heaviness4 > 1) {
				glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(heaviness1));
				glVertex2i(x  , y  );
				glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(heaviness2));
				glVertex2i(x+1, y  );
				glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(heaviness3));
				glVertex2i(x+1, y+1);
				glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(heaviness4));
				glVertex2i(x  , y+1);
		//	}
			/*glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(AT(deterministicHeaviness,(x+2)/3,(y+2)/3)));
			glVertex2i(x  , y  );
			glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(AT(deterministicHeaviness,(x+3)/3,(y+2)/3)));
			glVertex2i(x+1, y  );
			glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(AT(deterministicHeaviness,(x+3)/3,(y+3)/3)));
			glVertex2i(x+1, y+1);
			glColor4d(0.5, 0.5, 0.5, RAND_FLOAT_UP_TO(AT(deterministicHeaviness,(x+2)/3,(y+3)/3)));
			glVertex2i(x  , y+1);*/
		}
	glEnd();
	glPopMatrix();
}


