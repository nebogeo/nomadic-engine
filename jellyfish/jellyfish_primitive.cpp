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

jellyfish_primitive::jellyfish_primitive():
    primitive(256, TRIANGLES),
    m_machine(m_positions,256)
{
    
}

jellyfish_primitive::~jellyfish_primitive()
{

}

void jellyfish_primitive::render(u32 hints)
{
    for (int i=0; i<100; i++) m_machine.run();
//    m_machine.pretty_dump();

    // fix up the arrays to only draw the 
    // model data parts
    u32 size=m_size;
    vec3* positions=m_positions;
    vec3* normals=m_normals;
    vec3* colours=m_colours;
    vec3* tex=m_tex;
    
    m_size=REG_MDL_END-REG_MDL;
    m_positions=positions+REG_MDL;
    m_normals=normals+REG_MDL;
    m_colours=colours+REG_MDL;
    m_tex=tex+REG_MDL;

    primitive::render(hints);
    
    m_size=size;
    m_positions=positions;
    m_normals=normals;
    m_colours=colours;
    m_tex=tex;
}

