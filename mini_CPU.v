// alu module

module alu_operation_(result,parity,carry,zero,a,b,opcode);
input [15:0]a,b;  
input [2:0]opcode;
output reg [15:0]result;
output parity,carry,zero;
always @(a,b,opcode) begin
       case(opcode)
        3'b000: result=a&b;  //AND
        3'b001: result=a+b;  //ADD
        3'b010: result=a*b;  //MULT
        3'b011: result=a/b;  //DIV
        3'b100: result=a|b;  //OR
        3'b101: result=a-b;  //SUB
        3'b110: result=~(a|b); //NOR
        3'b111: result=~(a&b); //NAND
        
       endcase
	 
end
assign parity = ^result ? 0 : 1;
assign zero = (result == 16'd0) ? 1 : 0;
assign carry=result[8];
endmodule

// ram memory module

module ram_memory_(data_out,data_in,read_address,write_address,we,clk,rst);
input we,clk,rst;
input [11:0]read_address,write_address;
input [15:0]data_in;
output reg [15:0]data_out;
reg [15:0] ram[0:4095]; // 4096x16 ram memory - registers
integer i;
initial begin
ram[0]=16'h7400;  //CLE
ram[1]=16'h2015;  //LDA address 21
ram[2]=16'h7080;  //CIR
ram[3]=16'h3015;  //STA address 21
ram[4]=16'h7002;  //SZE
ram[5]=16'h4007;  //BUN to address 7
ram[6]=16'h400B;  //BUN to address 11
ram[7]=16'h2014;  //LDA address 20
ram[8]=16'h1016;  //ADD address 22
ram[9]=16'h3016;  //STA address 22
ram[10]=16'h7400; //CLE
ram[11]=16'h2014; //LDA address 20
ram[12]=16'h7040; //CIL
ram[13]=16'h3014; //STA address 20
ram[14]=16'h6013; //ISZ address 19
ram[15]=16'h4000; //BUN to address 0
ram[16]=16'h7001; //HLT
ram[19]=16'hFFF8; //Operand CTR
ram[20]=16'h000F; //Operand X
ram[21]=16'h000B; //Operand Y
ram[22]=16'h0000; //Operand P
end
always @(posedge clk)
begin
     if(rst) begin
	       for(i=0;i<4096;i=i+1) begin
			    ram[i]=16'd0; //reset each row data
			 end
	  end else begin    
        if(we) begin
	       ram[write_address]<=data_in; //write data
        end else begin	 
	           data_out<=ram[read_address]; //read data
	         end
	  end 
end
endmodule

// Counter module

module cpu_counter_(counter,clk,SC,rst);
input clk,SC,rst;
output reg[15:0]counter;
initial begin
counter=16'b1111111111111111;
end
always @(posedge clk or posedge rst) begin
    if (rst) begin
      counter <= 16'd0;
    end else if (!SC) begin
      counter <= 16'd0;
    end else begin
      counter <= counter + 1;
    end
end
endmodule

// Control unit module

module control_unit_(opcode,AC,SC,clk,rst,parity,carry,zero,counter);
input clk,rst,parity,carry,zero;
input [15:0]counter; // counter of the cpu
output reg[2:0]opcode;
output reg [15:0]AC; //Accumulator Register to store alu output value and other operations
output SC;
reg control_SC;
reg stop_SC=1'b1;
reg [1:0]state;
reg [15:0]a, b; // inputs of alu   **********
reg we;  //write enable of the memory
reg I,E; //decodes the type of instruction, carry register
reg [15:0]IR,in_mem; //Instruction register, data out/in of the of the ram memory
wire [15:0]out_mem;
wire [15:0]AC_wire;
reg [15:0]DR;
reg [11:0]operation; 
reg [11:0]PC; // PC register
reg [11:0]AR,pr_PC; //Address register, previous data of PC register 
reg IEN,FGI,FGO; // Interrupt enable, input & output flags
reg [7:0]INPR,OUTR; // Input & output registers
integer c=2'b00;
ram_memory_ ram1(out_mem,in_mem,AR,AR,we,clk,rst); 
alu_operation_ alu1(AC_wire,parity,carry,zero,a,b,opcode);
initial begin
PC=12'd0;
//AC=16'd0;
end

always @(posedge clk) begin
case(state)
2'b00: begin 
            control_SC <= 1'b0;
            state <= 2'b01;  // Transition to state 01
       end
2'b01: begin 
            control_SC <= 1'b1;
            state <= 2'b10;  // Transition to state 10 after one clock cycle
       end

 default: control_SC <= 1'b1;
 endcase
 
 if(rst) begin
 PC<=12'd0;
 end
 if (counter>16'd3) begin
  if(opcode==3'b111) begin  // Register-reference instruction / Input-Output instruction
      operation<=AR;
		if(I==1'b0) begin   // Register-reference instruction (7xxx)
        if(operation==12'h001) begin          //7001 - HLT - halt computer
        stop_SC=1'b0;     //*********
		  end else if(operation==12'h002) begin //7002 - SZE - skip next instruction if carry is 0
		  if (!control_SC) begin
        PC<=(E==1'b0) ? PC+1:PC;
        end
		  state<=2'b00;
        end else if(operation==12'h004) begin //7004 - SZA - skip next instruction if AC=0
		  if (!control_SC) begin
        PC<=(AC==16'd0) ? PC + 1 : PC;
        end
		  state<=2'b00;
		  end else if(operation==12'h008) begin //7008 - SNA - skip next instruction if AC negative
		  if (!control_SC) begin
        PC<=(AC[15]==1'b1) ? PC + 1 : PC;
        end
		  state<=2'b00;
		  end else if(operation==12'h010) begin //7010 - SPA - skip next instruction if AC positive
		  if (!control_SC) begin
        PC<=(AC[15]==1'b0) ? PC + 1 : PC;
        end
		  state=2'b00;
		  end else if(operation==12'h020) begin //7020 - INC - increment AC
		  if (!control_SC) begin
		  AC<=AC+1;
		  end
		  state=2'b00;
		  end else if(operation==12'h040) begin //7040 - CIL - circulate left AC and carry
		  if (!control_SC) begin
		  E<=AC[15];
		  AC={AC[14:0],E};
		  end
		  state<=2'b00;
		  end else if(operation==12'h080) begin //7080 - CIR - circulate right AC and carry
		  if (!control_SC) begin
		   E<=AC[0];
			AC={E,AC[15:1]};
			end 
			state<=2'b00;
		  end else if(operation==12'h100) begin //7100 - CME - complement carry
		  if (!control_SC) begin
		  E<=~E;
		  end
		  state=2'b00;
		  end else if(operation==12'h200) begin //7200 - CMA - complement AC
		  if (!control_SC) begin
		  AC<=(~AC);
		  end
		  state=2'b00;
		  end else if(operation==12'h400) begin //7400 - CLE - clear carry
		  E<=1'b0;
		  state=2'b0;
		  end else if(operation==12'h800) begin //7800 - CLA - clear AC
		  AC<=16'd0;
		  state=2'b00;
		  end
	
		end
		else begin // Input-Output instruction (Fxxx) - I=1
		  if(operation==12'h040) begin          //F040 - IOF - interrupt off
		  IEN<=1'b0;
		  end else if(operation==12'h080) begin //F080 - ION - interrupt on
		  IEN<=1'b1;
		  end else if(operation==12'h100) begin //F100 - SKO - skip on output flag
		  PC<=(FGO==1'b1) ? PC+1:PC;
		  end else if(operation==12'h200) begin //F200 - SKI - skip on input flag
		  PC<=(FGI==1'b1) ? PC+1:PC;
		  end else if(operation==12'h400) begin //F400 - OUT - output character from AC
		  OUTR<=AC[7:0];
		  FGO<=1'b0;
		  end else if(operation==12'h800) begin //F800 - INP input character to AC
		  AC[7:0]<=INPR;
		  FGI<=1'b0;
		  end
		end
	  end else begin //opcode = 000 ~ 110 - Memory-reference instruction
	     operation<=12'h000;
		  if(opcode==3'b000) begin //0xxx / 8xxx - AND - and memory word to AC  
		  we<=1'b0;
		  DR<=ram1.data_out;
		  a=DR;
		  b=AC;
		  state=2'b00;
		  end else if(opcode==3'b001) begin //1xxx / 9xxx - ADD - add memory word to AC
		  if (control_SC) begin
		  case(c)
		  2'b00: begin
		  we<=1'b0;
		  c<=c+1;
		  end
		  2'b01: begin
		  DR=ram1.data_out;
		  c<=c+1;
		  end
		  2'b10: begin
		  a=DR;
		  b=AC;
		  c<=c+1;
		  state=2'b00;
		 end 
		  2'b11: begin
		  AC=AC_wire; 
		  c=2'b00;
		  end
		  endcase
		  end
		  end else if(opcode==3'b010) begin //2xxx / Axxx - LDA - load memory word to AC
		  we<=1'b0;
		  DR=ram1.data_out;
		  AC=DR;
		  state=2'b00;
		  end else if(opcode==3'b011) begin //3xxx / Bxxx - STA - store content of AC in memory
		  in_mem<=AC;
		  we=1'b1; //*** ready for write, is also write?
		  state=2'b00;
		  end else if(opcode==3'b100) begin //4xxx / Cxxx - BUN - branch unconditionally
		  PC<=AR;  
		  state=2'b00;
		  end else if(opcode==3'b101) begin //5xxx / Dxxx - BSA - branch and save return address
		  in_mem<=AR;
		  AR<=AR + 1;
		  we<=1'b1; //*** ready for write, is also write?
		  PC=AR; 
		  state=2'b00;
		  end else if(opcode==3'b110) begin //6xxx / Exxx - ISZ - increment and skip if zero
		  we<=1'b0;
		  DR=ram1.data_out;
		  DR=DR+1;
		  in_mem<=DR;
		  we<=1'b1;
		  if (!control_SC) begin
		  PC<=(DR==16'd0) ? PC+1:PC;
		  end
		  state=2'b00;
		  end
	end
	end

end
assign SC=(stop_SC) ? control_SC:1'b0;

always @(counter) begin
  case(counter)
  16'd0: begin            //FETCH
        AR<=PC; 
		  in_mem<=16'd0; //for read mode, in_mem(data in) don't care  
		  pr_PC<=PC; //**** Save the initial PC value
		  we<=1'b0; //read mode
		  end
  16'd1: begin
        IR<=ram1.data_out;
		  PC<=PC+1;
		  end
  16'd2: begin          //DECODE
		  AR<=IR[11:0]; 
		  I<=IR[15];
		  end
  16'd3: begin          //EXECUTE     
        opcode<=IR[14:12];
		  end 
		  
  endcase
end

endmodule

// main module

module mini_CPU(d_out,clk,rst);
input clk,rst;
output reg [7:0]d_out;
wire [15:0]counter;
wire [11:0]PC;
wire SC;
wire [2:0]opcode;
wire [15:0]AC;
wire zero,carry,parity;
cpu_counter_ counter1(counter,clk,SC,rst);
control_unit_ control_unit1(opcode,AC,SC,clk,rst,parity,carry,zero,counter);
endmodule

// Test Bench
 
module tb_mini_CPU_();

reg clk;
reg rst;
wire [7:0] d_out;
  
reg we;
reg [11:0] read_address, write_address;
reg [15:0] data_in;
wire [15:0] data_out;
  
reg SC;
 
mini_CPU mini_CPU_t (.d_out(d_out),.clk(clk),.rst(rst));
ram_memory_ ram1(.data_out(data_out),.data_in(data_in),.read_address(read_address),
.write_address(write_address),.we(we),.clk(clk),.rst(rst));

initial begin
clk = 0;
rst = 0;
read_address = 0;
write_address = 0;
repeat (2000) begin   // Continue clock toggling until 100 time units
 #3 clk = ~clk;
end
 #5000 $stop;
end
endmodule
