module mux2(out,a,b,sel);
    input a,b,sel;
    output reg out;

    always @(*) begin
        if(sel) out = a;
        else out = b;
    end
endmodule : mux2

// using the verilator to check about it verilator --lint-only -Wall mux.v