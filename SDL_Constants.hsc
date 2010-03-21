module SDL_Constants where
--currently unused...
import Data.Word(Word32)

#include "SDL.h"

init_timer :: Word32
init_timer = #const SDL_INIT_TIMER
init_noparachute :: Word32
init_noparachute = #const SDL_INIT_NOPARACHUTE

