module mux2(out,a,b,sel);
    input a,b,sel;
    output reg out;

    always @(*) begin
        if(sel) out = a;
        else out = b;
    end
endmodule : mux2

// using the verilator to check about it verilator --lint-only -Wall mux.v


// using the logic to test about it
module muxtwo(input a,input b,input sel, output out);
    wire sela,selb,nsl;
    assign nsl = ~sel;
    assign sela = a & nsl;
    assign selb = b & sel;
    assign out = sela | selb;
endmodule : muxtwo

module compare(a,b,equal);
    output equal;
    input[1:0]a,b;
    assign equal = (a==b) ? 1:0;
endmodule : compare