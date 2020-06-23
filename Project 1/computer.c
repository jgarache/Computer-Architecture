#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#include <string.h>	//Added
#include <math.h>
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

        /* ;
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        UpdatePC(&d,val);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    unsigned int working = instr;
    /* Values Used
     2 ^ (32 - 6) = 67108864
     2 ^ (32 - 11) = 2097152
     2 ^ (32 - 16) = 65536
     2 ^ (32 - 21) = 2048
     2 ^ (32 - 26) = 64
    */

    //Decode Dec -> [Dec][Dec]
    d->op = working / 67108864;
    working = working % 67108864;
    if(d->op == 0){
        d->type = R;

    	d->regs.r.rs = working / 2097152;
    	working = working % 2097152;
    	d->regs.r.rt = working / 65536;
		working = working % 65536;
    	d->regs.r.rd = working / 2048;
		working = working % 2048;
		d->regs.r.shamt = working / 64;
		working = working % 64;
		d->regs.r.funct = working;

        rVals->R_rs = mips.registers[d->regs.r.rs];
        rVals->R_rt = mips.registers[d->regs.r.rt];
        rVals->R_rd = mips.registers[d->regs.r.rd];
    }else if(d->op == 2 || d->op == 3){
        d->type = J;

        d->regs.j.target = working;
    }else{
        d->type = I;

    	d->regs.i.rs = working / 2097152;
    	working = working % 2097152;
    	d->regs.i.rt = working / 65536;
		working = working % 65536;
    	d->regs.i.addr_or_immed = working;

        rVals->R_rs = mips.registers[d->regs.i.rs];
        rVals->R_rt = mips.registers[d->regs.i.rt];
        rVals->R_rd = 0;
    }

    //Check for support;
    if(d->op == 0){
        switch(d->regs.r.funct){
            case 0:	    break;  //sll
            case 2:	    break;  //srl
            case 8:	    break;  //jr
            case 33:	break;  //addu
            case 35:	break;  //subu
            case 36:	break;  //and
            case 37:	break;  //or
            case 42:	break;  //slt
            default: exit(0);
        }
    }else{
        switch(d->op){
            case 2:	    break;  //j
            case 3:	    break;  //jal
            case 4:	    break;  //beq
            case 5:	    break;  //bne
            case 9:	    break;  //addiu
            case 12:	break;  //andi
            case 13:	break;  //ori
            case 15:	break;  //lui
            case 35:	break;  //lw
            case 43:	break;  //sw
            default: exit(0);
        }
    }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
	char op[10];

    if(d->op == 0){
        switch(d->regs.r.funct){
            case 0: printf("sll\t$%d, $%d, %d\n", d->regs.r.rd, d->regs.r.rt, d->regs.r.shamt);  return;  //sll
            case 2: printf("srl\t$%d, $%d, %d\n", d->regs.r.rd, d->regs.r.rt, d->regs.r.shamt);  return;  //srl
            case 8: printf("jr\t$%d\n", d->regs.r.rs);  return;  //jr
            case 33: strcpy(op, "addu");    break;  //addu
            case 35: strcpy(op, "subu");    break;  //subu
            case 36: strcpy(op, "and");     break;  //and
            case 37: strcpy(op, "or");      break;  //or
            case 42: strcpy(op, "slt");     break;  //slt
        }
		printf("%s\t$%d, $%d, $%d\n",op, d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
    }else{
        switch(d->op){
            case 2: printf("j\t0x%08x\n", d->regs.j.target * 4);   return;  //j
            case 3: printf("jal\t0x%08x\n", d->regs.j.target * 4);   return;  //jal
            case 4: printf("beq\t$%d, $%d, 0x%08x\n", d->regs.i.rs, d->regs.i.rt, (short)d->regs.i.addr_or_immed * 4 + mips.pc + 4);   return;  //beq
            case 5: printf("bne\t$%d, $%d, 0x%08x\n", d->regs.i.rs, d->regs.i.rt, (short)d->regs.i.addr_or_immed * 4 + mips.pc + 4);   return;  //bne
            case 9: strcpy(op, "addiu");    break;  //addiu
            case 12: strcpy(op, "andi");    break;  //andi
            case 13: strcpy(op, "ori");     break;  //ori
            case 15: printf("lui\t$%d, %08x\n", d->regs.i.rt, (short)d->regs.i.addr_or_immed);   return;  //lui
            case 35: printf("lw\t$%d, %d($%d)\n", d->regs.i.rt, (short)d->regs.i.addr_or_immed, d->regs.i.rs);   return;  //lw
            case 43: printf("sw\t$%d, %d($%d)\n", d->regs.i.rt, (short)d->regs.i.addr_or_immed, d->regs.i.rs);   return;  //sw
        }
		printf("%s\t$%d, $%d, %d\n",op, d->regs.i.rt, d->regs.i.rs, (short)d->regs.i.addr_or_immed);
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    int rs, rt, shamt, imm;
    
    if(d->type == J){ // J type
        switch(d->op){
            case 2: //"j"
                return (4 * d->regs.j.target);
            case 3: //"jal"
                return (4 * d->regs.j.target);
        }
    }else if(d->type == R){ // R type
        
        //set registers to rs, rt
        rs = rVals->R_rs;
        rt = rVals->R_rt;
        shamt = d->regs.r.shamt;

        //execution of instruction
        switch(d->regs.r.funct){
            case 0:  //"sll"
                return (rt << shamt);
            case 2: //"srl"
                return (rt >> shamt);
            case 8: //"jr"       
                return (rs);
            case 33: //"addu"    
                return (rs + rt);
            case 35: //"subu"    
                return (rs - rt);
            case 36: //"and"     
                return (rs & rt);
            case 37: //"or"
                return (rs  | rt);
            case 42: //"slt"     
                return ((rs - rt < 0) ? 1 : 0);
        }
    }else{// I type
    
        //set registers to rs, rt
        rs = rVals->R_rs;
        rt = rVals->R_rt;
        imm = d->regs.i.addr_or_immed;            
        //execution of instruction    
        switch(d->op){
            case 4: //"beq"
                return ((rs == rt)? (mips.pc+4 + (4*(short)imm)) : (mips.pc + 4));      
            case 5: //"bne"      
                return ((rs != rt)? (mips.pc+4 + (4*(short)imm)) : (mips.pc + 4));
            case 9: //"addiu"    
                return (rs + (short)imm);
            case 12: //"andi"
                return (rs & imm);
            case 13: //"ori"
                return (rs | imm);
            case 15: //"lui"
                return (imm << 16);
            case 35: //"lw"
                return (rs + (short)imm);
            case 43: //"sw"      
                return (rs + (short)imm);
        }
    }
    return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    if((d->op == 0 && d->regs.r.funct == 8) || (d->op >= 2 && d->op <= 5))
        mips.pc = val;
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    *changedMem = -1;

    if(d->op == 35){
        if(val < 0x00401000 || val > 0x00403fff || val % 4 != 0){
            printf("Memory Access Exception at 0x%08x: address 0x%08x\n", mips.pc, val);
            exit(0);
        }
        return mips.memory[(val-0x00400000)/4];
    }else if(d->op == 43){
        if(val < 0x00401000 || val > 0x00403fff || val % 4 != 0){
            printf("Memory Access Exception at 0x%08x: address 0x%08x\n", mips.pc, val);
            exit(0);
        }
        *changedMem = val;
        mips.memory[(val-0x00400000)/4] = mips.registers[d->regs.i.rt];
    }
    return val;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    *changedReg = -1;

    if(d->op == 0){
        switch(d->regs.r.funct){
            case 0:	    //sll
            case 2:	    //srl
            case 33:	//addu
            case 35:	//subu
            case 36:	//and
            case 37:	//or
            case 42:	//slt
            *changedReg = d->regs.r.rd;
        }
    }else{
        switch(d->op){
            case 3:     //jal
                *changedReg = 31;
                mips.registers[*changedReg] = mips.pc + 4;
                return;
            case 9:     //addiu
            case 12:	//andi
            case 13:	//ori
            case 15:	//lui
            case 35:	//lw
            *changedReg = d->regs.r.rt;
        }
    }

    if(*changedReg != -1)
        mips.registers[*changedReg] = val;
}