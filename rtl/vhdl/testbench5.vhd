--===========================================================================--
--
-- CPU11 Microprocessor Test Bench 5
--
-- CPU11, ROM & RAM test
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

entity my_testbench5 is
end my_testbench5;

-------------------------------------------------------------------------------
-- Architecture for CPU11 Testbench 5
-------------------------------------------------------------------------------
architecture behavior of my_testbench5 is
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
  signal rom_data_out: Std_Logic_Vector(7 downto 0);
  signal ram0_data_out: Std_Logic_Vector(7 downto 0);
  signal ram0_cs      : std_logic;
  signal ram1_data_out: Std_Logic_Vector(7 downto 0);
  signal ram1_cs      : std_logic;
  signal ram2_data_out: Std_Logic_Vector(7 downto 0);
  signal ram2_cs      : std_logic;
  signal ram3_data_out: Std_Logic_Vector(7 downto 0);
  signal ram3_cs      : std_logic;
  signal ram4_data_out: Std_Logic_Vector(7 downto 0);
  signal ram4_cs      : std_logic;
  signal ram5_data_out: Std_Logic_Vector(7 downto 0);
  signal ram5_cs      : std_logic;
  signal ram6_data_out: Std_Logic_Vector(7 downto 0);
  signal ram6_cs      : std_logic;
  signal ram7_data_out: Std_Logic_Vector(7 downto 0);
  signal ram7_cs      : std_logic;

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

component my_ram
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(3 downto 0);
	 data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0)
	 );
end component;


component boot_rom
  port (
    addr  : in  Std_Logic_Vector(9 downto 0);  -- 1K byte boot rom
	 data  : out Std_Logic_Vector(7 downto 0)
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

sram0 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram0_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram0_data_out
	 );

sram1 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram1_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram1_data_out
	 );

sram2 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram2_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram2_data_out
	 );

sram3 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram3_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram3_data_out
	 );

sram4 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram4_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram4_data_out
	 );

sram5 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram5_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram5_data_out
	 );

sram6 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram6_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram6_data_out
	 );

sram7 : my_ram port map (	
	 clk       => SysClk,
    rst       => cpu_reset,
    cs        => ram7_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(3 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ram7_data_out
	 );

  rom : boot_rom port map (
	 addr       => cpu_addr(9 downto 0),
    data       => rom_data_out
	 );

decode : process( cpu_addr, cpu_vma, rom_data_out,
                  ram0_data_out, ram1_data_out, ram2_data_out, ram3_data_out, 
						ram4_data_out, ram5_data_out, ram6_data_out, ram7_data_out  )
begin
   ram0_cs <= '0';
   ram1_cs <= '0';
   ram2_cs <= '0';
   ram3_cs <= '0';
   ram4_cs <= '0';
   ram5_cs <= '0';
   ram6_cs <= '0';
   ram7_cs <= '0';
   case cpu_addr(15 downto 13) is
	when "111" =>
	   cpu_data_in <= rom_data_out;
   when "101" =>
	   case cpu_addr(6 downto 4 ) is
		when "000" =>
	     cpu_data_in <= ram0_data_out;
		  ram0_cs <= cpu_vma;
		when "001" =>
	     cpu_data_in <= ram1_data_out;
		  ram1_cs <= cpu_vma;
		when "010" =>
	     cpu_data_in <= ram2_data_out;
		  ram2_cs <= cpu_vma;
		when "011" =>
	     cpu_data_in <= ram3_data_out;
		  ram3_cs <= cpu_vma;
		when "100" =>
	     cpu_data_in <= ram4_data_out;
		  ram4_cs <= cpu_vma;
		when "101" =>
	     cpu_data_in <= ram5_data_out;
		  ram5_cs <= cpu_vma;
		when "110" =>
	     cpu_data_in <= ram6_data_out;
		  ram6_cs <= cpu_vma;
		when "111" =>
	     cpu_data_in <= ram7_data_out;
		  ram7_cs <= cpu_vma;
      when others =>
		  null;
      end case;
   when others =>
	   cpu_data_in <= "00000000";
   end case;
end process; 

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

end behavior; --===================== End of architecture =======================--

