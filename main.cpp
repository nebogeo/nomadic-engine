// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <iostream>
#include <string>
#include <png.h>

#include "engine/importgl.h"
#include "core/fixed.h"
#include "app.h"

using namespace std;

int w,h=0;
int gAppAlive = 1;

unsigned char* LoadPNG(const string filename,long &width, long &height)
{
	unsigned char *data = NULL;
	FILE *fp=fopen(filename.c_str(),"rb");
	if (!fp || filename=="")
	{
		cerr<<"Couldn't open image ["<<filename<<"]"<<endl;
	}
	else
	{
		png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
		png_infop info_ptr = png_create_info_struct(png_ptr);

		if (setjmp(png_jmpbuf(png_ptr)))
		{
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			cerr<<"Error reading image ["<<filename<<"]"<<endl;
			fclose(fp);
			return NULL;
		}

		png_init_io(png_ptr, fp);
		png_read_info(png_ptr, info_ptr);

		width = png_get_image_width(png_ptr, info_ptr);
		height = png_get_image_height(png_ptr, info_ptr);
		int bit_depth = png_get_bit_depth(png_ptr, info_ptr);
		int colour_type = png_get_color_type(png_ptr, info_ptr);
		png_bytep *row_pointers=new png_bytep[height];
		unsigned int rb = png_get_rowbytes(png_ptr, info_ptr);

		for (unsigned long row=0; row<height; row++)
		{
			row_pointers[row] = new png_byte[rb];
		}

		// read the data into the row pointers
		png_read_image(png_ptr, row_pointers);
		fclose(fp);

		// make a new contiguous array to store the pixels
		data=new unsigned char[rb*height];
		int p=0;
		for (int row = 0; row<height; row++) 
		{
			for (unsigned int i=0; i<rb; i++)
			{
				data[p]=(unsigned char)(row_pointers[row])[i];
				p++;
			}
		}

		// clear up the row_pointers
		for (unsigned long row=0; row<height; row++)
		{
			delete[] row_pointers[row];
		}
		delete[] row_pointers;

		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
	}
    return data;
}


string LoadFile(string filename)
{
    FILE *file=fopen(filename.c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);

		char *buffer = new char[size+1];
        long s = (long)fread(buffer,1,size,file);
        buffer[s]='\0';
        string r = buffer;
		delete[] buffer;
		fclose(file);
        return r;
    }
    cerr<<"couldn't open "<<filename<<endl;
    return "";
}

void ReshapeCallback(int width, int height)
{
    w=width;
    h=height;
}

void IdleCallback()
{
	glutPostRedisplay();
}

void MouseCallback(int button, int state, int x, int y)
{
	char code[256];
	sprintf(code,"(input-mouse-button %d %d) (input-mouse %d %d)",button,state,x,y);
    appEval(code);
}

void MotionCallback(int x, int y)
{
	char code[256];
	sprintf(code,"(input-mouse %d %d)",x,y);
    appEval(code);
}

void PassiveMotionCallback(int x, int y)
{
	char code[256];
	sprintf(code,"(input-mouse %d %d)",x,y);
    appEval(code);
}

void DisplayCallback()
{   
    appRender(0, w, h);
	glutSwapBuffers();

    static bool first=true;
    if (first)
    {
        appEval((char*)string("(pre-process-run '("+LoadFile("../assets/startup.scm")+"))").c_str());
        printf("running script\n");
        first=false;
    }
}

void KeyboardCallback(unsigned char key,int x, int y)
{
    if (key=='e')
    {
        appEval((char*)string("(pre-process-run '("+LoadFile("../assets/startup.scm")+"))").c_str());
    }
}

void glTranslatex(GLfixed x, GLfixed y, GLfixed z)
{
    glTranslatef(x/65536.0,y/65536.0,z/65536.0);
}

void glFrustumx(GLfixed xmin, GLfixed xmax, GLfixed ymin, GLfixed ymax, GLfixed zNear, GLfixed zFar)
{
    glFrustum(xmin/65536.0, xmax/65536.0,
              ymin/65536.0, ymax/65536.0,
              zNear/65536.0, zFar/65536.0);
}

void glClearColorx(GLfixed r, GLfixed g, GLfixed b, GLfixed a)
{
    glClearColor(r/65536.0,g/65536.0,b/65536.0,a/65536.0);
}

void glMaterialx( GLenum face, GLenum pname, GLfixed param)
{
    glMaterialf(face,pname,param/65536.0);
}

void glMaterialxv( GLenum face, GLenum pname, GLfixed * params)
{
    float fparams[4];
    fparams[0]=params[0]/65536.0;
    fparams[1]=params[1]/65536.0;
    fparams[2]=params[2]/65536.0;
    fparams[3]=params[3]/65536.0;
    glMaterialfv(face,pname,fparams);
}

void glLightxv( GLenum light, GLenum pname, GLfixed * params)
{
    float fparams[4];
    fparams[0]=params[0]/65536.0;
    fparams[1]=params[1]/65536.0;
    fparams[2]=params[2]/65536.0;
    fparams[3]=params[3]/65536.0;
    glLightfv(light,pname,fparams);
}

void glMultMatrixx( GLfixed * mat )
{
    float m[16];
    for (int i=0; i<16; i++)
    {
        m[i]=mat[i]/65536.0f;
    }
    glMultMatrixf(m);
}

int main(int argc, char *argv[])
{
	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL;

	// init OpenGL
	glutInit(&argc,argv);
	glutInitWindowSize(480,800);
	glutInitDisplayMode(flags);
	char windowtitle[256];
	sprintf(windowtitle,"nomadic android scratchpad");
	glutCreateWindow(windowtitle);
	glutDisplayFunc(DisplayCallback);
	glutReshapeFunc(ReshapeCallback);
	glutMouseFunc(MouseCallback);
	glutMotionFunc(MotionCallback);
	glutPassiveMotionFunc(PassiveMotionCallback);
	glutIdleFunc(IdleCallback);
	glutKeyboardFunc(KeyboardCallback);

    appInit();

    appEval((char*)LoadFile("../assets/init.scm").c_str());  
    appEval((char*)LoadFile("../assets/boot.scm").c_str());

    long w=0,h=0;
    unsigned char *tex=LoadPNG("../assets/font.png",w,h);
    appLoadTexture("font.png",w,h,(char *)tex);

    tex=LoadPNG("../assets/icons.png",w,h);
    appLoadTexture("icons.png",w,h,(char *)tex);

    tex=LoadPNG("../assets/squib.png",w,h);
    appLoadTexture("squib.png",w,h,(char *)tex);


	glutMainLoop();

	return 0;
}


