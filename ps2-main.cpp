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

#include <kernel.h>
#include <stdlib.h>
#include <stdio.h>
#include <sifrpc.h>
#include <loadfile.h>
#include <libpad.h>
#include <screenshot.h>
#include "app.h"
#include "engine/engine.h"
#include "engine/ee/ps2-renderer.h"

#include "core/mat44.h"

int gAppAlive = 1;

char *load_file(char* filename)
{
    FILE *file=fopen(filename,"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);
		char *buffer = new char[size+1];
        long s = (long)fread(buffer,1,size,file);
        buffer[s]='\0';
		fclose(file);
        return buffer;
    }
    return NULL;
}

void load_run(char *filename)
{
    char *code=load_file(filename);
    appEval(code);
    delete[] code;
}


/*
 * waitPadReady()
 */
int waitPadReady(int port, int slot)
{
    int state;
    int lastState;
    char stateString[16];

    state = padGetState(port, slot);
    lastState = -1;
    while((state != PAD_STATE_STABLE) && (state != PAD_STATE_FINDCTP1)) {
        if (state != lastState) {
            padStateInt2String(state, stateString);
            printf("Please wait, pad(%d,%d) is in state %s\n", 
                       port, slot, stateString);
        }
        lastState = state;
        state=padGetState(port, slot);
    }
    // Were the pad ever 'out of sync'?
    if (lastState != -1) {
        printf("Pad OK!\n");
    }
    return 0;
}


/*
 * initializePad()
 */
int
initializePad(int port, int slot)
{

    int ret;
    int modes;
    int i;

    waitPadReady(port, slot);

    // How many different modes can this device operate in?
    // i.e. get # entrys in the modetable
    modes = padInfoMode(port, slot, PAD_MODETABLE, -1);
    printf("The device has %d modes\n", modes);

    if (modes > 0) {
        printf("( ");
        for (i = 0; i < modes; i++) {
            printf("%d ", padInfoMode(port, slot, PAD_MODETABLE, i));
        }
        printf(")");
    }

    printf("It is currently using mode %d\n", 
               padInfoMode(port, slot, PAD_MODECURID, 0));

    // If modes == 0, this is not a Dual shock controller 
    // (it has no actuator engines)
    if (modes == 0) {
        printf("This is a digital controller?\n");
        return 1;
    }

    // Verify that the controller has a DUAL SHOCK mode
    i = 0;
    do {
        if (padInfoMode(port, slot, PAD_MODETABLE, i) == PAD_TYPE_DUALSHOCK)
            break;
        i++;
    } while (i < modes);
    if (i >= modes) {
        printf("This is no Dual Shock controller\n");
        return 1;
    }

    // If ExId != 0x0 => This controller has actuator engines
    // This check should always pass if the Dual Shock test above passed
    ret = padInfoMode(port, slot, PAD_MODECUREXID, 0);
    if (ret == 0) {
        printf("This is no Dual Shock controller??\n");
        return 1;
    }

    printf("Enabling dual shock functions\n");

    // When using MMODE_LOCK, user cant change mode with Select button
    padSetMainMode(port, slot, PAD_MMODE_DUALSHOCK, PAD_MMODE_LOCK);

    waitPadReady(port, slot);
    printf("infoPressMode: %d\n", padInfoPressMode(port, slot));

    waitPadReady(port, slot);        
    printf("enterPressMode: %d\n", padEnterPressMode(port, slot));

    waitPadReady(port, slot);

    return 1;
}



int main(int argc, char **argv)
{
    GS_SET_BGCOLOR(0xff,0x00,0x00);

    SifInitRpc(0);
    SifLoadModule("rom0:SIO2MAN", 0, NULL);
    SifLoadModule("rom0:PADMAN", 0, NULL); 
    padInit(0);

    ps2_renderer::init();
    GS_SET_BGCOLOR(0xff,0xff,0x00);
    appInit();
    GS_SET_BGCOLOR(0x00,0x00,0xff);

    load_run("host:scheme/scm/init.scm");
    load_run("host:scheme/scm/boot.scm");
    load_run("host:startup.scm");

//    appEval("(load-pre-process-run \"host:startup.scm\")");

    GS_SET_BGCOLOR(0x00,0xff,0xff);

    int tick=0;

    static char padBuf[256] __attribute__((aligned(64))); 

    padButtonStatus buttons;
    u32 paddata;
    u32 old_pad = 0;
    u32 new_pad;
    u32 i;
    u32 ret;
        int port=0;
        int slot=0;

    printf("PortMax: %d\n", padGetPortMax());
    printf("SlotMax: %d\n", padGetSlotMax(port));

    
    if((ret = padPortOpen(port, slot, padBuf)) == 0) {
        printf("padOpenPort failed: %d\n", ret);
        SleepThread();
    }
    
    if(!initializePad(port, slot)) {
        printf("pad initalization failed!\n");
        SleepThread();
    }


    for(;;)
    {
        ret=padGetState(port, slot);
        i=0;
        while((ret != PAD_STATE_STABLE) && (ret != PAD_STATE_FINDCTP1)) {
            if(ret==PAD_STATE_DISCONN) {
                printf("Pad(%d, %d) is disconnected\n", port, slot);
            }
            ret=padGetState(port, slot);
        }
        if(i==1) {
            printf("Pad: OK!\n");
        }
            
        ret = padRead(port, slot, &buttons); // port, slot, buttons
            
        if (ret != 0) {
            paddata = 0xffff ^ buttons.btns;
            new_pad = paddata & ~old_pad;
            old_pad = paddata;
            if(new_pad & PAD_CROSS) {
                load_run("host:startup.scm");
            }
            if(new_pad & PAD_CIRCLE) {
                printf("screenshot addr: %d\n",ps2_renderer::get()->m_env->Screen0);
                ps2_screenshot_file("host:shot",
                                    ps2_renderer::get()->m_env->Screen0,
                                    ps2_renderer::get()->m_env->Height,
                                    ps2_renderer::get()->m_env->Width,
                                    ps2_renderer::get()->m_env->Psm); 
                }
        }

        appRender(tick++, 0, 0);
    }
}
