`timescale 1ns/1ps

module mux2_tb;

    reg clk;
    reg a_in,b_in,sel;
    integer i;
    reg out;

    initial begin
        $fsdbDumpfile("mux2.fsdb");
        $fsdbDumpvars;
        $monitor("time=%3d, a_in =%b, b_in=%b, sel=%b ,out = %b \n", $time, a_in, b_in, sel,out);

        a_in = 0;
        b_in = 0;
        sel = 0;
        clk = 1'b0;
        for(i = 0; i < 255; i = i + 1) begin
            #1 clk = ~clk;
        end

    end

    muxtwo dut(
        .a(a_in),
        .b(b_in),
        .sel(sel),
        .out(out)
    );

    always @(posedge clk) begin
        a_in = {$random} % 2;
        b_in = {$random} % 2;
        sel = {$random} % 2;
    end

endmodule : mux2_tb