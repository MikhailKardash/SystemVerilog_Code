module bitcoin_hash2( // note: name changed to match project submission protocol
	
input logic          clk, reset_n, start,
input logic   [15:0] message_addr, output_addr,
output logic          done, mem_clk, mem_we,
output logic   [15:0] mem_addr,
output logic   [31:0] mem_write_data,
input logic   [31:0] mem_read_data);

enum logic [1:0] {IDLE, WRITE, DONE} state;
 parameter NUM_NONCES = 16;

 logic [4:0] rc; // read and write counters
 logic [4:0] wc; //must be able to get to NUM_NONCES value, thus should be 5 bit.
 logic [31:0] theH[NUM_NONCES];
 logic [31:0] debug;
 assign mem_clk = clk;
 logic [NUM_NONCES-1:0] doneint, isRead, isComp;
 logic [15:0] getMem;
 logic [6:0] tnew/*,twoo*/;
 
 // SHA256 K constants
 

 
 genvar q;
 generate
 for (q = 0; q < NUM_NONCES; q++) begin: acquire_new_Morty
	hashBOI BlockchainMorty(
		.clk(clk), .reset_n(reset_n), .start(start), .mem_clk(mem_clk),
		.doneint(doneint[q]),.isRead(isRead[q]), .isComp(isComp[q]), //i wonder why we can't just make these 1 element. Ask LIN.
		.theH(theH[q]),
		.mem_read_data(mem_read_data), .nonce(q),
		.tnew(tnew)/*, .twoo(twoo)*/
	);
 end
endgenerate

always_ff @(posedge clk,negedge reset_n) begin
	if(!reset_n) begin
		state <= IDLE;
	end else 
	case (state)
	IDLE: //start
		begin
		if(doneint == 16'b1111111111111111) begin
			mem_we<= 1;
			mem_write_data<=theH[0];
			wc <= 1;
			state <= WRITE;
		end else begin
			mem_we <= 0;
			done <= 0;
		end
		end
	WRITE:
		begin
			if (wc < NUM_NONCES) begin
				mem_we <= 1;
				mem_write_data <= theH[wc];
				wc <= wc + 1;
			end else begin
				state<=DONE;
			end
		end
	DONE: begin
		done <=1;
		state <= IDLE;
		end
	endcase
end

/*always @(tnew) begin
	twoo = tnew + 7'b1;
end*/

always_ff@(posedge clk, negedge reset_n) begin
	if(!reset_n) begin
		tnew <= 1;
	end else 
	case (isComp)
	16'b1111111111111111: 
	begin
		tnew <= tnew + 1;
	end
	16'b0000000000000000: 
	begin
			tnew <= 1;
	end
	endcase
end

always_ff@(posedge clk) begin //rc and mem_addr controlled outside of the other ff.
	if(start) begin
		rc <= 1;
		mem_addr <= message_addr;
	end	
	if(isRead == 16'b1111111111111111) begin
		mem_addr <= message_addr+rc;
		rc <= rc + 5'd1;
	end
	if(doneint == 16'b1111111111111111) begin
		mem_addr <= output_addr;
	end
	if(state == WRITE) begin
		mem_addr <= output_addr + wc;
	end
end

endmodule