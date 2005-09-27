
#include <math.h>
#include <stdlib.h>
#include <inttypes.h>
#include <GL/gl.h>
#include <stdio.h>
#include "foreignPollution.h"

void foreignPollution(uint32_t randSeed, const double * pollution, uint32_t pollutionWidth, uint32_t pollutionHeight) {
	uint32_t x, y;
	#define AT(array,x,y) ( (array)[((x)*(array##Height))+((y))] )
	#define RAND() ( randSeed = (((randSeed * 1103515245) + 12345) & 0x7fffffff) )
	#define RAND_FLOAT_UP_TO(hi) ( (double)RAND() * (hi) / 0x7fffffff )
	#define RAND_FLOAT_BETWEEN(lo,hi) ( (lo) + (double)RAND() * ((hi)-(lo)) / 0x7fffffff )
	/*for(x = 0; x != width; ++x) {
		for(y = 0; y != height; ++y) {
			int got = AT(pollution,x,y);
			printf("%c", got<10 ? '0'+got : 'a'+(got-10));
		}
		printf("\n");
	}
	printf("\n");*/
	glScaled(1.0/3.0, 1.0/3.0, 1);
	/*want to shift by 0.5*/
	glTranslated(-0.5, -0.5, 0.0);
	glBegin(GL_QUADS);
		const uint32_t deterministicHeavinessWidth = pollutionWidth + 2;
		const uint32_t deterministicHeavinessHeight = pollutionHeight + 2;
		double * deterministicHeaviness = (double*)alloca(deterministicHeavinessWidth*deterministicHeavinessHeight*sizeof(double));
		for(x = 0; x != deterministicHeavinessWidth; ++x) for(y = 0; y != deterministicHeavinessHeight; ++y) {
			//should probably move in 1 to find border pollution amounts
			AT(deterministicHeaviness,x,y) = (
				(x == 0 || y == 0 || x == deterministicHeavinessWidth || y == deterministicHeavinessHeight) ?
					0 :
					/*log(1 +*/ AT(pollution,x-1,y-1)/*)*/
			);
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
}


