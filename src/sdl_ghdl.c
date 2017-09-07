/*
 * SDL adapter for Konami Arcade Emulator
 * (C) Copyright 2017 Christopher D. Kilgour
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include "SDL/SDL.h"
#include <stdio.h>

SDL_Surface * screen = NULL;

/* ---------------------------------------------------------------------- */

int init_sdl(int dummy)
{
  SDL_Init(SDL_INIT_EVERYTHING);

  screen = SDL_SetVideoMode(512, 512, 32, SDL_SWSURFACE);

  return 0;
}

/* ---------------------------------------------------------------------- */

int put_pixel(int x, int y, int r, int g, int b)
{
  // printf("x=%d, y=%d, r=%d, g=%d, b=%d\n", x, y, r, g, b);

  if ((x < 256) && (y < 256)) {
    Uint32 pixel = SDL_MapRGB(screen->format, r, g, b);

    SDL_LockSurface(screen);
    
    Uint8 * p = (Uint8*) screen->pixels;
    p += (2 * y * screen->pitch) + (2 * x * sizeof(Uint32));
    
    Uint32* pp = (Uint32*)p;
    pp[0] = pixel; pp[1] = pixel;
    
    p += screen->pitch;
    pp = (Uint32*)p;
    pp[0] = pixel; pp[1] = pixel;
  
    SDL_UnlockSurface(screen);
    SDL_UpdateRect(screen, 2*x, 2*y, 2, 2);
  }
  else {
    printf("skipping bogus pixel: x=%d, y=%d, r=%d, g=%d, b=%d\n", x, y, r, g, b);
  }

  return 0;
} 
