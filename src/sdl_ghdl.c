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

  screen = SDL_SetVideoMode(256, 256, 32, SDL_SWSURFACE);

  return 0;
}

/* ---------------------------------------------------------------------- */

int put_pixel(int x, int y, int r, int g, int b)
{
  Uint32 pixel = SDL_MapRGB(screen->format, r, g, b);

  SDL_LockSurface(screen);

  Uint8 * p = (Uint8*) screen->pixels;
  p += (y * screen->pitch) + (x * sizeof(Uint32));
  *((Uint32*)p) = pixel;

  SDL_UnlockSurface(screen);
  SDL_UpdateRect(screen, x, y, 1, 1);

  // printf("x=%d, y=%d, r=%d, g=%d, b=%d\n", x, y, r, g, b);

  return 0;
} 
