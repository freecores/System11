--===========================================================================--
--
-- CPU11 Microprocessor Test Bench 2
--
-- Subroutine test code.
--
-- John Kent 21st October 2002
--
--
-------------------------------------------------------------------------------
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity my_testbench2 is
end my_testbench2;

-------------------------------------------------------------------------------
-- Architecture for CPU11 Testbench 2
-------------------------------------------------------------------------------
architecture behavior of my_testbench2 is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal uart_irq    : Std_Logic;
  signal timer_irq   : std_logic;

  -- Sequencer Interface signals
  signal SysClk      : Std_Logic;
  signal cpu_reset   : Std_Logic;
  signal cpu_rw      : std_logic;
  signal cpu_vma     : std_logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);

  constant width   : integer := 8;
  constant memsize : integer := 64;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
    "10001110", "11111111", "11011101", -- FFC0 - 8E FFDD  RESET LDS #$FFDD
	 "11001110", "00010010", "00110100", -- FFC3 - CE 1234        LDX #$1234
	 "10001101", "00000100",             -- FFC6 - 8D 04          BSR SAVGET
    "00100111", "11110110",             -- FFC8 - 27 F6    REENT BEQ RESET
	 "00100000", "11111110",             -- FFCA - 20 FF          BRA *
	 "11111111", "11111111", "11110000", -- FFCC - FF FFF0 SAVGET STX $FFF0
	 "11111110", "11111111", "11011001", -- FFCF - FE FFD9        LDX $FFD9
	 "00110111",                         -- FFD2 - 37             PSHB
	 "11100110", "00000001",             -- FFD3 - E6 01          LDAB 1,X
	 "11100001", "00000011",             -- FFD5 - E1 03          CMPB 3,X
	 "00110011",                         -- FFD7 - 33             PULB
	 "00111001",                         -- FFD8 - 39             RTS
	 "11111111", "11100000",             -- FFD9 - FF E0          FDB $FFE0
	 "01010101",                         -- FFDB - 55             FCB $55
	 "11111111", "11001000",             -- FFDC - FFC8           FDB REENT
	 "00100000", "11100000",             -- FFDE - 20 E0          BRA RESET
	 "00000000", "00000000",             -- FFE0 - 00 00          fcb $00,$00
	 "00000000", "00000000",             -- FFE2 - 00 00          fcb $00,$00
	 "00000000", "00000000",             -- FFE4 - 00 00          fcb $00,$00
	 "00000000", "00000000",             -- FFE6 - 00 00          fcb $00,$00
    "01001000", "01100101", "01101100", -- FFE8 - 48 65 6c MSG   FCC "Hel"
	 "01101100", "01101111", "00100000", -- FFEB - 6c 6f 20       FCC "lo "
	 "01010111", "01101111", "01110010", -- FFEE - 57 6f 72       FCC "Wor"
    "01101100", "01100100",             -- FFF1 - 6c 64          FCC "ld"
    "00001010", "00001101", "00000000", -- FFF3 - 0a 0d 00       FCB LF,CR,NULL
    "00000000", "00000000",             -- FFF6 - 00 00          fcb null,null           
	 "11111111", "11000000",             -- FFF8 - FF C0          fdb $FFC0 ; Timer irq
	 "11111111", "11000000",             -- FFFA - FF C0          fdb $FFC0 ; Ext IRQ
	 "11111111", "11000000",             -- FFFC - FF C0          fcb $FFC0 ; SWI
	 "11111111", "11000000"              -- FFFE - FF C0          fdb $FFC0 ; Reset
	 );

component cpu11
  port (    
    data_in:  in	std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0);
    address:  out	std_logic_vector(15 downto 0);
    vma:	     out	std_logic;
    rw:	     out	std_logic;		-- Asynchronous memory interface
    rst:	     in	std_logic;
	 clk:	     in	std_logic;
	 irq:      in  std_logic;
	 xirq:     in  std_logic
  );
end component;


begin
cpu : cpu11  port map (    
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
    address   => cpu_addr(15 downto 0),
    vma       => cpu_vma,
    rw	     => cpu_rw,
    rst	     => cpu_reset,
	 clk	     => SysClk,
	 irq       => uart_irq,
	 xirq      => timer_irq
  );

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

	cpu_reset <= '0';
	SysClk <= '0';
   uart_irq <= '0';
	timer_irq <= '0';

		for count in 0 to 256 loop
			SysClk <= '0';
			if count = 0 then
				cpu_reset <= '1';
			elsif count = 1 then
				cpu_reset <= '0';
			end if;
			wait for 100 ns;
			SysClk <= '1';
			wait for 100 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***


  rom : PROCESS( cpu_addr )
  begin
    cpu_data_in <= rom_data(conv_integer(cpu_addr(5 downto 0))); 
  end process;

end behavior; --===================== End of architecture =======================--

