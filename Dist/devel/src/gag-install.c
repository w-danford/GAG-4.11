#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>

//  Linux installer for GAG
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Library General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//  (C)2007 Raster Software Vigo

void langerr(char *param) {

	DIR *directory;
	struct dirent *entry;
	int length;
	char a_string[256];

	if (param!=NULL) {
		printf("Unknown language %s\n",param);
	}
	printf("\nAvailable languages:\n");
	directory=opendir("/boot/gag");
	if (directory==NULL) {
		printf("Can't show the language list\n");
		exit(1);
	}
	do {
		entry=readdir(directory);
		if (entry==NULL) {
			break;
		}

		if (0==strcmp("HDBOOT.COM",entry->d_name)) {
			continue;
		}
		length=strlen(entry->d_name);
		
		if (strlen(entry->d_name)<4) {
			continue;
		}
		
		if (strcasecmp(entry->d_name+length-4,".COM")) {
			continue;
		}
		
		strcpy(a_string,entry->d_name);
		a_string[length-4]=0;
		printf("%s\n",a_string);
	} while (entry!=NULL);
	closedir(directory);
	exit(1);
}

void lang(char *param,unsigned char *language) {

	unsigned char *pointer;
	struct stat status;
	unsigned char path[1024];
	unsigned char lang2[14];
	
	if (param==NULL) {
		langerr(param);
	}
	
	if (strlen(param)>8) {
		langerr(param);
	}
	
	if (0==strcasecmp(param,"hdboot")) {
		langerr(param);
	}
	
	strcpy(lang2,param);
	pointer=lang2;
	while(*pointer!=0) {
		*(pointer++)&=0xDF; // change it to uppercase
	}

	sprintf(path,"/boot/gag/%s.COM",lang2);
	if (0!=stat(path,&status)) {
		langerr(param);
	}
	strcpy(language,lang2);
}

unsigned char keyb(char *param) {

	if (param!=NULL) {
		if (0==strcasecmp("qwerty",param)) {
			return 0;
		}
		if (0==strcasecmp("azerty",param)) {
			return 1;
		}
		if (0==strcasecmp("qwertz",param)) {
			return 2;
		}
		if (0==strcasecmp("dvorak",param)) {
			return 3;
		}
		if (0==strcasecmp("cyrillic",param)) {
			return 4;
		}
		
		printf("\nUnknown keyboard type.\n");
	}
	
	printf("\nSupported keyboards:\n");
	printf("QWERTY\n");
	printf("AZERTY\n");
	printf("QWERTZ\n");
	printf("DVORAK\n");
	printf("Cyrillic\n");
	exit(1);
}

unsigned long int read_start(unsigned char *buffer,int pos) {

	unsigned long int value,v1,v2,v3,v4;
	
	if (buffer[pos-4]==0) { // empty partition
		return 0xFFFFFFFF;
	}
	
	v1=(unsigned long int)buffer[pos];
	v2=(unsigned long int)buffer[pos+1];
	v3=(unsigned long int)buffer[pos+2];
	v4=(unsigned long int)buffer[pos+3];
	
	value=v1+((v2<<8)&0x0000FF00)+((v3<<16)&0x00FF0000)+((v4<<24)&0xFF000000);
	return (value);
}

int main(int argc,char **argv) {

	unsigned char buffer[32256]; // buffer to store the whole GAG and MBR sector
	unsigned char MBR[512]; // and here we store only the MBR
	int param;
	unsigned char keyboard,error;
	unsigned char language[13];
	unsigned char path[1024];
	unsigned char letter,*param2,*device;
	
	int devfile,bucle;
	unsigned long int s1,s2,s3,s4;
	
	keyboard=0; // default keyboard is QWERTY
	strcpy(language,"ENGLISH"); // default language is ENGLISH
	device=NULL;

	printf("GAG installer, v4.9\n");

	for(param=1;param<argc;param++) {
		if (argv[param][0]=='-') {
			letter=argv[param][1];
			param++;
			if (param==argc) { // there are no more parameters. Error
				param2=NULL;
			} else {
				param2=argv[param];
			}

			switch (letter) {
			case 'k': // keyboard
				keyboard=keyb(param2);
			break;
			case 'l': // language
				lang(param2,language);
			break;
			default:
				printf("Error: parameter -%c unknown.\n\n",letter);
				exit(1);
			break;
			}
		} else {
			device=argv[param]; // device
		}
	}

	if (device==NULL) {
		printf("Usage: gag-install [-k keyboard] [-l language] device\n");
		printf("List available keyboards: gag-install -k\n");
		printf("List available languages: gag-install -l\n\n");
		exit(1);
	}

	printf("Language: %s\n",language);
	printf("Keyboard type: ");
	switch (keyboard) {
	case 0:
		printf("QWERTY");
	break;
	case 1:
		printf("AZERTY");
	break;
	case 2:
		printf("QWERTZ");
	break;
	case 3:
		printf("DVORAK");
	break;
	case 4:
		printf("Cyrillic");
	break;
	}
	printf("\n");
	printf("Will intall GAG on device %s\n",device);
	
	devfile=open(device,O_RDONLY);
	if (devfile==-1) { // can't open device
		printf("Can't open device %s. Exiting.\n",device);
		exit(1);
	}
	
	if (512!=read(devfile,MBR,512)) {
		close(devfile);
		printf("Error while reading the MBR of %s. Exiting.\n",device);
		exit(1);
	}
	close(devfile);

	error=0;
	if (63>read_start(MBR,454)) {
		error=1;
	}
	
	if (63>read_start(MBR,470)) {
		error=1;
	}
	if (63>read_start(MBR,486)) {
		error=1;
	}
	if (63>read_start(MBR,502)) {
		error=1;
	}
	
	if (error==1) {
		printf("There are no free space to install GAG.\n");
		printf("Remake your partitions to ensure that they start after the sector 63\n");
		exit(1);
	}
	devfile=open("/boot/gag/HDBOOT.COM",O_RDONLY);
	if (devfile==-1) { // can't open hdboot
		printf("Can't open HDBOOT.COM. Exiting.\n");
		exit(1);
	}
	read(devfile,buffer,446);
	close(devfile);
	for (bucle=446;bucle<512;bucle++) {
		buffer[bucle]=MBR[bucle]; // copy the partition table and the boot signature
	}
	
	sprintf(path,"/boot/gag/%s.COM",language);
	devfile=open(path,O_RDONLY);
	if (devfile==-1) { // can't open hdboot
		printf("Can't open %s. Exiting.\n",path);
		exit(1);
	}
	read(devfile,buffer+512,127); // jump over the offset
	read(devfile,buffer+512,61*512);
	close(devfile);
	buffer[512]=keyboard;
	
	devfile=open(device,O_WRONLY);
	if (devfile==-1) { // can't open device
		printf("Can't open device %s. Exiting.\n",device);
		exit(1);
	}
	write(devfile,buffer,62*512);
	close(devfile);
	
	printf("\n\n\nGAG sucesfully installed in %s\n\n",device);
	printf("Don't forget to install GRUB in the superblock of your root partition using\n\n");
	printf("grub-install\n\n\n");
	
	return 0;
}
