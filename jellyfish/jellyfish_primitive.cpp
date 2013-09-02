// Copyright (C) 2011 Dave Griffiths
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

#include "jellyfish_primitive.h"
#include "../core/fixed.h"
#include "../core/geometry.h"
#include "../engine/scenenode.h"
#include "../core/msg.h"

jellyfish_primitive::jellyfish_primitive(u32 size):
    primitive(size, TRIANGLES)
{
}

// (called in scenenode ctr)
void jellyfish_primitive::build()
{
    cerr<<"building jellyfish"<<endl;

    // todo: EE code: (vec3*)memalign(128, sizeof(vec3) * m_size
    vec3 *data = new vec3[m_size*5];

    m_machine = new jellyfish(data,m_size*5);

    pdata_add("x",data);
    pdata_add("p",data+m_size);
    pdata_add("n",data+m_size*2);
    pdata_add("c",data+m_size*3);
    pdata_add("t",data+m_size*4);
    m_colours_=new u8[4*m_size];
    m_positions=get_pdata_arr("p");
    m_normals=get_pdata_arr("n");
    m_colours=get_pdata_arr("c");
    m_tex=get_pdata_arr("t");    
}


jellyfish_primitive::~jellyfish_primitive()
{

}

void jellyfish_primitive::render(u32 hints)
{
    for (int i=0; i<m_machine->peekiy(REG_CONTROL); i++) {
        m_machine->run();
//        m_machine->pretty_dump();
    }

//    sleep(10);

    u32 size=m_size;
    m_size=m_machine->peekix(REG_GRAPHICS);
    primitive::render(hints);
    m_size=size;
}
