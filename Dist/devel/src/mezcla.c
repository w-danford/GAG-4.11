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

 /* (C)1998-2004 Raster Software Vigo */

#include <stdio.h>
#include <stdlib.h>

// we use 4 tracks per image
#define NUMTRACKS 4
// NUMSECBYTES is the number of bytes per image
#define NUMSECBYTES (18*512*NUMTRACKS)


void position(int posicion,int valor,unsigned char *memoria) {

  memoria[posicion] = (unsigned char) (valor & 0xFF);
  valor = (valor >> 8);
  memoria[posicion+1] = (unsigned char) (valor & 0xFF);
}

void anade(int *contador,int *posicion,char *nfichero,unsigned char *memoria) {

  FILE *fichero;
  unsigned char dato,last;
  int contador2,posicion2;

  contador2=(*contador);
  posicion2=(*posicion);
  printf("%d %d\n",contador2,posicion2);
  fichero=fopen(nfichero,"rb");
  if(fichero==NULL) {
    printf("Error abriendo fichero %s\n",nfichero);
    exit(1);
  }
  while(!feof(fichero)) {
    fread(&dato,1,1,fichero);
    if((dato==10) || ((dato>31) && (dato<128))) {
	  last=dato;
	  if (dato==10)
		  memoria[posicion2]=13;
	  else
      	  memoria[posicion2]=dato;
      posicion2++;
    }
    if(dato==10)
      contador2++;
  }
  if (last!=10) {
  	memoria[posicion2]=13;
	posicion2++;
	contador2++;
  }
  memoria[posicion2]=0;
  memoria[posicion2+1]=13;
  posicion2+=2;
  contador2+=1;
  (*contador)=contador2;
  (*posicion)=posicion2;
}

void anade2(int posicion,char *nfichero,unsigned char *memoria) {

  FILE *fichero;
  unsigned char dato;
  unsigned char nfichero2[14];
  int bucle;

  nfichero2[0]=0;
  for(bucle=0;nfichero[bucle]!=0;bucle++) {
	  dato=nfichero[bucle];
      if ((dato>='a') && (dato<='z'))
		  dato-=32;
	  nfichero2[bucle]=dato;
      nfichero2[bucle+1]=0;
  }

  fichero=fopen(nfichero2,"rb");
  if(fichero==NULL) {
    printf("Error 2 abriendo fichero %s\n",nfichero);
    exit(1);
  }
  while(!feof(fichero)) {
    fread(&dato,1,1,fichero);
    memoria[posicion]=dato;
    posicion++;
  }
}

int main (int argc, char *argv[])
{
  FILE  *fichero;
  int   bucle, contador, posicion, pistas, pistaact;
  unsigned char *disk;

  disk = (unsigned char *) malloc (1474560); // Size of a floppy disk

  if (disk == NULL)
  {
     printf ("malloc() fail.\n");
     getchar();
     return -1;
  }

  printf ("mezcla...\n");

  for(bucle=0;bucle<1474560;bucle++)
    disk[bucle]=0;

  contador=0;
  posicion=9216; // Starting of the second track (first cylinder, side B)
  anade(&contador,&posicion,"messages.txt",disk);
  position(504,contador,disk); // write the offset for instructions
  anade(&contador,&posicion,"readme.txt",disk);
  position(506,contador,disk); // write the offset for the FAQ
  anade(&contador,&posicion,"faq.txt",disk);
  position(508,contador,disk); // write the offset for the license
  anade(&contador,&posicion,"copying",disk);
  printf ("Texto copiado.\n");

  posicion-=9216;
  // printf("Longitud: %d\n",posicion);
  pistas=posicion /(512*18); // calculate the number of tracks that occupies
  if((pistas*512*18)!= posicion) // the instructions, FAQ, license and mesgs.
    pistas++;
  // printf("Pistas: %d\n",pistas);
  disk[501]=1; // Head 1
  disk[502]=0; // cylinder 0
  disk[503]=(unsigned char) (pistas); // store it in the disk image

  disk[510]=0x55;
  disk[511]=0xAA; // Boot signature (needed for booting)

  disk[448]=NUMTRACKS; // number of tracks for each GAG image

  // offsetts for GAG images

  for(bucle=0;bucle<26;bucle++) {
    pistaact = 1+pistas+(bucle*NUMTRACKS);
    // printf("Pista: %d\n",pistaact);
    if((pistaact & 1)==0)
      disk[449+bucle*2]=0; // Head A
    else
      disk[449+bucle*2]=1; // Head B
    disk[450+bucle*2]=(unsigned char) (pistaact/2); // Cylinder
  }

  anade2(0,"boot.com",disk); // add the boot code in offset 0
  anade2(512,"install.com",disk); // add the installer in offset 512
  posicion=(pistas+1)*9216; // end of the last track used by instructions

  printf ("Programa copiado.\n");

  anade2(posicion,"russian.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"bable.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"brazilia.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"catalan.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"czech.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"dansk.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"nederlan.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"english.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"francais.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"galego.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"deutsch.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"magyar.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"italiano.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"japanese.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"norsk.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"polski.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"portugal.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"espanol.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"svenska.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"turkce.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"euskera.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track
  anade2(posicion,"suomen.com",disk);
  posicion+=NUMSECBYTES; // jump to the next track

  fichero=fopen("disk.dsk","wb");
  if (fichero == NULL)
     printf ("No se pudo crear disk.dsk\r\n");
  fwrite(disk,1474560,1,fichero); // write the disk image
  fclose(fichero);

  printf ("Imagen escrito.\r\n");

  free (disk);

  return 0;

}
