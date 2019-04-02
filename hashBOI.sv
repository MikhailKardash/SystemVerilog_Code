module hashBOI( // part of bitcoin_hash2
input logic   clk, reset_n, start, mem_clk,
output logic  doneint, isRead, isComp,
output logic [31:0] theH,
input logic   [31:0] mem_read_data, 
input logic [31:0] nonce,
input logic [6:0] tnew/*,twoo*/);

enum logic [3:0] {IDLE, PREPREP, READ, COMPUTE, POST, READ2, COMPUTE2, POST2, NEXTPREP, WRITE, PREP3, DONE} state;

 //logic [6:0] tnew,twoo;
 logic [31:0] A,B,C,D,E,F,G,H_H,/*kT,*/j_curr, wTemp/*,Sdot,Wnew*/;
 logic [31:0] w[16];
 logic [31:0] H[7];
 logic [1:0] update;
 logic comp_counter;
 // SHA256 K constants
parameter int sha256_k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
begin
    rightrotate = (x >> r) | (x << (32-r));
end
endfunction


function logic [255:0] sha256_opop(input logic [31:0] a, b, c, d, e, f, g, h, j);
    
	 logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g); 
    t1 = S1 + ch + j;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_opop = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

function logic [31:0] wtnew(input logic [31:0] woo); // function with precomputed w[0] + w[9]
 logic [31:0] s1, s2;
 s1 = rightrotate(w[14],17)^rightrotate(w[14],19)^(w[14]>>10);
 s2 = rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
 wtnew = woo + s1 + s2;
endfunction

always_ff @(posedge clk, negedge reset_n) begin
	if(!reset_n) begin
		state <= IDLE;
		isComp <= 0;
	end else 
	case (state)
	IDLE: //start
		if(start) begin
		isRead <= 1;
		doneint <= 0;
		state <= PREPREP;
		end
	PREPREP:
		begin
		//tnew <= 1;
		//kT <= 32'h5be0cd19 + sha256_k[0];
		state<=READ;
		end
	READ:
		begin
			A <= 32'h6a09e667;
			B <= 32'hbb67ae85;
			C <= 32'h3c6ef372;
			D <= 32'ha54ff53a;
			E <= 32'h510e527f;
			F <= 32'h9b05688c;
			G <= 32'h1f83d9ab;
			H_H <= 32'h5be0cd19;
			w[0] <= mem_read_data;
			j_curr  <= mem_read_data + 32'h5be0cd19 + sha256_k[0];// + kT;
			//kT <= 32'h1f83d9ab + sha256_k[tnew];
			isComp <=1;
			state <= COMPUTE;
		end
	COMPUTE:
		begin
			{A,B,C,D,E,F,G,H_H} <= sha256_opop(A,B,C,D,E,F,G,H_H,j_curr);  //0-15 precompute w_curr+kT in READ, 16-63 precompute in POST
			if (tnew == 13) isRead <= 0;
			if (tnew == 63) isRead <= 1;
			/*if (tnew == 14) begin  //2 cycles ahead precomputation
				wTemp <= w[0] + w[9];
				//Sdot <= rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
			end*/
			if (tnew == 15) begin
				//Wnew <= wTemp + Sdot;
				wTemp <= w[0] + w[9];
				//wTemp <= w[1] + w[10];
				//Sdot <= rightrotate(w[2],7)^rightrotate(w[2],18)^(w[2]>>3);
			end
			/*(if (tnew < 64) begin
				kT <= F + sha256_k[twoo];
			end*/
			if (tnew < 16) begin	
				w[tnew] <= mem_read_data;
				j_curr  <= mem_read_data + G + sha256_k[tnew];// + kT;
				//endstate
			end else if ((tnew < 64) && (tnew > 15)) begin
				for (int n = 0; n < 15; n++) w[n] <= w[n+1]; // just wires
				w[15] <= wtnew(wTemp);
				//j_curr <= kT + wtnew(Wnew);
				j_curr <= G + sha256_k[tnew] + wtnew(wTemp);
				//Wnew <= wTemp + Sdot;
				//wTemp <= w[2] + w[11]; //precompute w[0] + w[9]
				wTemp <= w[1] + w[10];
				//Sdot <= rightrotate(w[3],7)^rightrotate(w[3],18)^(w[3]>>3); //precompute rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
			end else begin
				isComp <=0;
				state<=POST;
			end
		end
	POST:
		begin
			//kT <= H[7] + sha256_k[0];
			A <= theH + A;
			B <= H[0];
			C <= H[1];
			D <= H[2];
			E <= H[3] + E;
			F <= H[4];
			G <= H[5];
			H_H <= H[6];
			state<=READ2;
		end
	READ2:
		begin
			w[0] <= mem_read_data;
			j_curr  <= mem_read_data + H[6] + sha256_k[0];// kT;
			//kT <= G + sha256_k[tnew];
			w[3] <= nonce; //nonce value, this segment is shared in another state...
			w[4] <= 32'h80000000;
			for (int i = 5; i < 15; i++) w[i] <= 32'h00000000;
			w[15] <= 32'd640;
			isRead<= 0;
			isComp <=1;
			comp_counter <= 0;
			state <= COMPUTE2;
		end
	COMPUTE2:
		begin
			{A,B,C,D,E,F,G,H_H} <= sha256_opop(A,B,C,D,E,F,G,H_H,j_curr);  //0-15 precompute w_curr+kT in READ, 16-63 precompute in POST
			if (tnew == 13) isRead <= 0; //for state equivalency
			if (tnew == 63) isRead <= 1;
			/*if (tnew == 14) begin  //2 cycles ahead precomputation
				wTemp <= w[0] + w[9];
				//Sdot <= rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
			end*/
			if (tnew == 15) begin
				//Wnew <= wTemp + Sdot;
				wTemp <= w[0] + w[9];
				//Wnew <= w[1] + w[10];
				//Sdot <= rightrotate(w[2],7)^rightrotate(w[2],18)^(w[2]>>3);
			end
			/*if (tnew < 64) begin
				kT <= F + sha256_k[twoo];
			end*/
			if (tnew < 3 && !comp_counter) begin
				w[tnew] <= mem_read_data;
				j_curr  <= mem_read_data + G + sha256_k[tnew];// kT;
			end else if(tnew < 16) begin  //PAD THE BOI
				j_curr  <= w[tnew] + G + sha256_k[tnew];// kT;
			end else if ((tnew < 64) && (tnew > 15)) begin
				for (int n = 0; n < 15; n++) w[n] <= w[n+1]; // just wires
				w[15] <= wtnew(wTemp);
				//Wnew <= wTemp;
				//j_curr <= kT + wtnew(Wnew);
				j_curr <= G + sha256_k[tnew] + wtnew(wTemp);
				//Wnew <= wTemp + Sdot;
				//wTemp <= w[2] + w[11]; //precompute w[0] + w[9]
				wTemp <= w[1] + w[10];
				//Sdot <= rightrotate(w[3],7)^rightrotate(w[3],18)^(w[3]>>3); //precompute rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
			end else begin
				isComp <=0;
				if(!comp_counter) begin
					comp_counter <= 1;
					state<= POST2;
				end else /*if(comp_counter)*/ begin
					state <= DONE;
				end
			end
		end
	POST2:
		begin
			isRead <= 0;
			w[0] <= A + theH;
			w[1] <= H[0];
			w[2] <= H[1];
			w[3] <= H[2];
			w[4] <= E + H[3];
			w[5] <= H[4];
			w[6] <= H[5];
			w[7] <= H[6];
			w[8] <= 32'h80000000;
			for (int i = 9; i < 15; i++) begin
				w[i] <= 32'h00000000;
			end
			w[15] <= 32'd256;
			//tnew <= 1;
			//kT <= 32'h5be0cd19 + sha256_k[0];
			state <= PREP3;
		end
	PREP3:
		begin //can remove this state by using 3 adder j_curr assignment in previous state but look at the amount of adders there already. :|
			j_curr  <= w[0] + 32'h5be0cd19 + sha256_k[0];// kT;
			A <= 32'h6a09e667;
			B <= 32'hbb67ae85;
			C <= 32'h3c6ef372;
			D <= 32'ha54ff53a;
			E <= 32'h510e527f;
			F <= 32'h9b05688c;
			G <= 32'h1f83d9ab;
			H_H <= 32'h5be0cd19;
			//kT <= 32'h1f83d9ab + sha256_k[tnew];
			isComp <=1;
			state <= COMPUTE2;
		end

	DONE: begin
		doneint <=1;
		isRead <= 0;
		state <= IDLE;
		end
	endcase
end

always_ff@(posedge clk) begin
	if (state == READ || state == PREP3) begin
			theH <= 32'h6a09e667;
			H[0] <= 32'hbb67ae85;
			H[1] <= 32'h3c6ef372;
			H[2] <= 32'ha54ff53a;
			H[3] <= 32'h510e527f;
			H[4] <= 32'h9b05688c;
			H[5] <= 32'h1f83d9ab;
			H[6] <= 32'h5be0cd19;
	end 
	if (tnew == 62) begin
		H[2] <= H[2] + A;
		H[6] <= H[6] + E;
		update <= 1;
	end
	case (update)
	1: begin
		H[1] <= H[1] + A;
		H[5] <= H[5] + E;
		update<=update + 1;
	end
	2: begin
		H[0] <= H[0] + A;
		H[4] <= H[4] + E;
		update <= update + 1;
	end
	3: begin
		theH <= theH + A;
		H[3] <= H[3] + E;
		update <= 0;
	end
	endcase
end

endmodule