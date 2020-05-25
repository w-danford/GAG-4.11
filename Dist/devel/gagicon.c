/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
/*  iconfile.c must be the output file from GIMP in C format,
	and the struct with the icon data must be named gimp_image too */
 
#include "iconfile.c"
#include <stdio.h>

unsigned char palete[16][3]={
	{ 0,0,0 },
	{ 32,32,32 },
	{ 48,48,48 },
	{ 48,32,32 },
	{ 63,63,63 },
	{ 48,0,0 },
	{ 0,48,0 },
	{ 56,44,2 },
	{ 43,31,8 },
	{ 50,50,0 },
	{ 0,0,48 },
	{ 32,0,0 },
	{ 0,48,48 },
	{ 48,16,16 },
	{ 10,9,28 },
	{ 0,32,0 }};

int main(void) {

	int bucle,bucle3,increment,distancia,distancia2;
	unsigned char *puntero,*puntero2,r1,r2,g1,g2,b1,b2,a,bucle2,element;
	FILE *fichero;
	unsigned char salida[1600]; // temporary place to store the pixmap
	
	if((gimp_image.width!=40)||(gimp_image.height!=40)) {
		printf("Error, the picture hasn't the right size.\n");
		exit(1);
	}
	
	increment=gimp_image.bytes_per_pixel==3 ? 0 : 1;
		
	puntero=palete[0];
	for(bucle=0;bucle<48;bucle++)
		*(puntero)=4*(*puntero++); // rotate the palette

	puntero=gimp_image.pixel_data;
	a=255; // by default, all pixels are opaque
	for(bucle=0;bucle<1600;bucle++) {
		distancia=195076; // maximum distance
		r1=*puntero++;
		g1=*puntero++;
		b1=*puntero++;
		if(increment) // alfa channel
			a=*puntero++;
		if(a<128) // if pixel is transparent, use gray as colour
			element=2;
		else // if is opaque...
			for(bucle2=0;bucle2<16;bucle2++) { // test distance to each one colour in the palette
				r2=palete[bucle2][0];
				g2=palete[bucle2][1];
				b2=palete[bucle2][2];
				r2 = r2>r1 ? r2-r1 : r1-r2;
				g2 = g2>g1 ? g2-g1 : g1-g2;
				b2 = b2>b1 ? b2-b1 : b1-b2;
				distancia2=r2*r2+g2*g2+b2*b2;
				if(distancia2<distancia) {
					distancia=distancia2; // and take the nearest colour
					element=bucle2;
				}
			}
		salida[bucle]=element;
	}
	
	fichero=fopen("exit.asm","wb");
	if(fichero==NULL) {
		printf("Can't open output file. Exiting.\n");
		exit(1);
	}

	for(bucle2=8;bucle2!=0;bucle2=((bucle2>>1)&0x0F)) { // four planes		
		fprintf(fichero,"DB ");
		for(bucle=0;bucle<1600;bucle+=8) {
			element=0;
			for(bucle3=0;bucle3<8;bucle3++) {
				element=(element<<1)&0xFE;				
				if(((salida[bucle+bucle3])&bucle2))
					element++; // set bit 0 to 1
			}			
			if(bucle!=0)
				fprintf(fichero,",");
			fprintf(fichero,"%d",element);
		}
		fprintf(fichero,"\n");
	}
	fclose(fichero);
}
