// Copyright (C) 2010 Dave Griffiths
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

// jellyfish, out of wack vector processor

#ifndef JELLYF_MACHINE
#define JELLYF_MACHINE

#include <stdio.h>
#include "../core/types.h"
#include "../core/vec3.h"

// instruction set
#define NOP 0
#define JMP 1
#define JMZ 2
#define JLT 3 
#define JGT 4
#define LDL 5
#define LDA 6 
#define LDI 7
#define STA 8
#define STI 9
#define ADD 10
#define SUB 11
#define MUL 12
#define DIV 13
#define ABS 14
#define SIN 15
#define ATN 16
#define DOT 17
#define CRS 18
#define SQR 19
#define LEN 20
#define DUP 21
#define CMP 22
#define SHF 23
#define BLD 24
#define RET 25

// registers
#define REG_PCO 100
#define REG_SPD 101
#define REG_POS 102
#define REG_VEL 103
#define REG_COL 104
#define REG_NIT 105
#define REG_SCC 106
#define REG_SDR 107
#define REG_AND 108
//...
#define REG_MDL 120
//...
#define REG_MDL_END 199
#define REG_STP 200
#define REG_STK 201
#define REG_NDT 256

class jellyfish 
{
public:
	jellyfish(vec3 *heap_ptr, u32 heap_size);
	~jellyfish();

	// global
	vec3 peek(s32 addr) const;
    s32 peekix(s32 addr) const;
	void poke(s32 addr, const vec3 &data);
	bool is_instr(s32 addr) const;
	void set_instr(s32 addr,bool s);
	void print_instr(s32 addr) const;

    void push(const vec3 &data);
    vec3 pop();
    vec3 top();

	void run();
	void simple_dump() const;
	void pretty_dump() const;
    void trash();
    
private:

	vec3 *m_heap;
    u32 m_heap_size;
	bool *m_instruction;
};

#endif



