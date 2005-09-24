{-# OPTIONS_GHC -cpp #-}

module SDL_Constants where

{-
get the macros without the C-header code
#include "SDL.h"
-}

import Data.Word(Word32)
#define FLAG(Name,Value) Name :: Word32 ; Name = Value
FLAG(init_timer,SDL_INIT_TIMER)
FLAG(init_audio,SDL_INIT_AUDIO)
FLAG(init_video,SDL_INIT_VIDEO)
FLAG(init_cdrom,SDL_INIT_CDROM)
FLAG(init_joystick,SDL_INIT_JOYSTICK)
FLAG(init_everything,SDL_INIT_EVERYTHING)
FLAG(init_noparachute,SDL_INIT_NOPARACHUTE)
FLAG(init_eventthread,SDL_INIT_EVENTTHREAD)

