--===========================================================================--
--
--  S Y N T H E Z I A B L E    System11 - System on a Chip
--
--  www.OpenCores.Org - September 2003
--  This core adheres to the GNU public license  
--
-- File name      : system11.vhd
--
-- Entity name    : system11
--
-- Purpose        : SWTBUG Monitor modified for the 68HC11
--                  ROM sits at $E000
--                  Assumes an 6850 ACIA sits at $8004
--                  Assumes RAM at $A000 - $BFFF for Flex 2
--                  1024 byte x 8 bit
--                  Modified stack to allow for Y register
--                  This SOC does not include any 68HC11 
--                  on chip peripherals
--
-- Dependencies   : ieee.std_logic_1164
--                  ieee.std_logic_arith
--                  ieee.std_logic_unsigned
--
-- Uses            : cpu11      (cpu11.vhd)     CPU core
--                   boot_rom   (swtbug11.vhd)  Monitor ROM
--                   dat_ram    (datram.vhd)    Dynamic Address Translation
--                   miniuart   (miniUART.vhd)  UART
--                      clkunit (clkunit.vhd)
--                      rxunit  (rxunit.vhd)
--                      txunit  (txunit.vhd)
--                   ioport     (ioport.vhd)    parallel i/o port
--                   timer      (timer.vhd)     small counter timer
--
-------------------------------------------------------------------------------
-- Revision list
-- Version   Author         Date                Changes
-- 1.0       John Kent      6 September 2003 	Initial release to open corse
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity System11 is
  port(
    SysClk      : in  Std_Logic;  -- System Clock input
	 Reset_n     : in  Std_logic;  -- Master Reset input (active low)
    LED         : out std_logic;  -- Diagnostic LED Flasher

    -- Memory Interface signals
    ram_csn     : out Std_Logic;
    ram_wrln    : out Std_Logic;
    ram_wrun    : out Std_Logic;
    ram_addr    : out Std_Logic_Vector(16 downto 0);
    ram_data    : inout Std_Logic_Vector(15 downto 0);

	 -- Stuff on the peripheral board
--  aux_clock   : in  Std_Logic;  -- FPGA-CPU-IO clock

	 -- PS/2 Mouse interface
--	 mouse_clock : in  Std_Logic;
--	 mouse_data  : in  Std_Logic;

	 -- Uart Interface
    rxbit       : in  Std_Logic;
	 txbit       : out Std_Logic;
    rts_n       : out Std_Logic;
    cts_n       : in  Std_Logic;

	 -- CRTC output signals
--	 v_drive     : out Std_Logic;
--    h_drive     : out Std_Logic;
--    blue_lo     : out std_logic;
--    blue_hi     : out std_logic;
--    green_lo    : out std_logic;
--    green_hi    : out std_logic;
--    red_lo      : out std_logic;
--    red_hi      : out std_logic;
--	 buzzer      : out std_logic;

-- I/O Ports
    PortA        : inout std_logic_vector(7 downto 0);
    PortB        : inout std_logic_vector(7 downto 0);
--    PortC        : inout std_logic_vector(7 downto 0);
--    PortD        : inout std_logic_vector(7 downto 0);

-- Timer I/O
	 timer_out    : out std_logic;

    -- Compact Flash B5-CF Module
    cf_rst_n     : out std_logic;
	 cf_cs0_n     : out std_logic;
	 cf_cs1_n     : out std_logic;
    cf_rd_n      : out std_logic;
    cf_wr_n      : out std_logic;
	 cf_cs16_n    : out std_logic;
    cf_a         : out std_logic_vector(2 downto 0);
    cf_d         : inout std_logic_vector(15 downto 0);
--    cf_intrq     : in std_logic;
--    cf_iordy     : in std_logic;
--  	cf_dase      : in std_logic;
--	   cf_pdiag     : in std_logic;
--	   cf_present   : in std_logic;

-- test signals
	 test_alu    : out std_logic_vector(15 downto 0);
	 test_cc     : out std_logic_vector(7 downto 0)
	 );
end;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture my_computer of System11 is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- CPU Interface signals
  signal cpu_reset   : Std_Logic;
  signal cpu_clk     : Std_Logic;
  signal cpu_rw      : std_logic;
  signal cpu_vma     : std_logic;
  signal cpu_irq     : std_logic;
  signal cpu_xirq    : std_logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);

  -- BOOT ROM
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);

  -- UART Interface signals
  signal uart_data_out : Std_Logic_Vector(7 downto 0);  
  signal uart_cs       : Std_Logic;
  signal uart_irq      : Std_Logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs    : std_logic;
  signal timer_irq   : std_logic;

  -- i/o port
  signal ioport_data_out : std_logic_vector(7 downto 0);
  signal ioport_cs   : std_logic;

  -- RAM
  signal ram_cs      : std_logic; -- memory chip select
  signal ram_wrl     : std_logic; -- memory write lower
  signal ram_wru     : std_logic; -- memory write upper
  signal ram_data_out    : std_logic_vector(7 downto 0);

  -- compact flash port
  signal cf_cs       : std_logic;
  signal cf_rd       : std_logic;
  signal cf_wr       : std_logic;
  signal cf_data_out : std_logic_vector(7 downto 0);

  -- Dynamic Address Translation RAM
  signal dat_cs      : std_logic;
  signal dat_data_out: std_logic_vector(7 downto 0);

  -- Flashing Led test signals
  signal countL      : std_logic_vector(23 downto 0);


-----------------------------------------------------------------
--
-- Open Cores Mini UART
--
-----------------------------------------------------------------

component miniUART
  port (
     SysClk   : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input
     cs       : in  Std_Logic;
     rw       : in  Std_Logic;
     RxD      : in  Std_Logic;
     TxD      : out Std_Logic;
     CTS_n    : in  Std_Logic;
     RTS_n    : out Std_Logic;
     Irq      : out Std_logic;
     Addr     : in  Std_Logic;
     DataIn   : in  Std_Logic_Vector(7 downto 0); -- 
     DataOut  : out Std_Logic_Vector(7 downto 0)); -- 
end component;

--------------------------------------
--
-- Three port parallel I/O
--
---------------------------------------

component ioport
  port (
     clk      : in std_logic;
	  rst      : in std_logic;
	  cs       : in std_logic;
	  rw       : in std_logic;
	  addr     : in std_logic_vector(1 downto 0);
	  data_in  : in std_logic_vector(7 downto 0);
	  data_out : out std_logic_vector(7 downto 0);
	  porta_io : inout std_logic_vector(7 downto 0);
	  portb_io : inout std_logic_vector(7 downto 0)
	  );
end component;

----------------------------------------
--
-- Timer module
--
----------------------------------------

component timer
  port (
     clk       : in std_logic;
	  rst       : in std_logic;
	  cs        : in std_logic;
	  rw        : in std_logic;
	  addr      : in std_logic;
	  data_in   : in std_logic_vector(7 downto 0);
	  data_out  : out std_logic_vector(7 downto 0);
	  irq       : out std_logic;
     timer_in  : in std_logic;
	  timer_out : out std_logic
	  );
end component;

component cpu11
  port (    
	 clk:	     in	std_logic;
    rst:      in	std_logic;
    rw:	     out	std_logic;		-- Asynchronous memory interface
    vma:	     out	std_logic;
    address:  out	std_logic_vector(15 downto 0);
    data_in:  in	std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0);
	 irq:      in  std_logic;
	 xirq:     in  std_logic;
	 test_alu: out std_logic_vector(15 downto 0);
	 test_cc:  out std_logic_vector(7 downto 0)
  );
end component;

component dat_ram
  port (
    clk:      in  std_logic;
	 rst:      in  std_logic;
	 cs:       in  std_logic;
	 rw:       in  std_logic;
	 addr_lo:  in  std_logic_vector(3 downto 0);
	 addr_hi:  in  std_logic_vector(3 downto 0);
    data_in:  in  std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0)
	 );
end component;

component boot_rom
  port (
    addr  : in  Std_Logic_Vector(9 downto 0);  -- 1K byte boot rom
	 data  : out Std_Logic_Vector(7 downto 0)
  );
end component;

-- component BUFG is 
--  port (
--     i: in std_logic;
--	  o: out std_logic
--  );
-- end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_uart  : miniUART port map (
    SysClk    => SysClk,
	 rst       => cpu_reset,
    cs        => uart_cs,
	 rw        => cpu_rw,
	 RxD       => rxbit,
	 TxD       => txbit,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n,
    Irq       => uart_irq,
    Addr      => cpu_addr(0),
	 Datain    => cpu_data_out,
	 DataOut   => uart_data_out
	 );

my_ioport  : ioport port map (
    clk       => SysClk,
	 rst       => cpu_reset,
    cs        => ioport_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(1 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => ioport_data_out,
	 porta_io  => porta,
	 portb_io  => portb
    );

my_timer  : timer port map (
    clk       => SysClk,
	 rst       => cpu_reset,
    cs        => timer_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(0),
	 data_in   => cpu_data_out,
	 data_out  => timer_data_out,
    irq       => timer_irq,
	 timer_in  => CountL(5),
	 timer_out => timer_out
    );

my_cpu : cpu11  port map (    
	 clk	     => SysClk,
    rst       => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 irq       => cpu_irq,
	 xirq      => cpu_xirq,
	 test_alu  => test_alu,
	 test_cc   => test_cc
  );


my_dat : dat_ram port map (
    clk        => SysClk,
	 rst        => cpu_reset,
	 cs         => dat_cs,
	 rw         => cpu_rw,
	 addr_hi    => cpu_addr(15 downto 12),
	 addr_lo    => cpu_addr(3 downto 0),
    data_in    => cpu_data_out,
	 data_out   => dat_data_out(7 downto 0)
	 );

  rom : boot_rom port map (
	 addr       => cpu_addr(9 downto 0),
    data       => rom_data_out
	 );

--  clk_buffer : BUFG port map(
--    i => e_clk,
--	 o => cpu_clk
--  );	 
	 
----------------------------------------------------------------------
--
--  Processes to read and write memory based on bus signals
--
----------------------------------------------------------------------

my_decoder: process(
                 cpu_addr, cpu_vma,
					  rom_data_out, ram_data_out,
					  ioport_data_out, timer_data_out, uart_data_out, cf_data_out )
begin
    case cpu_addr(15 downto 13) is
	   --
		-- ROM & DAT Space $E000 - $FFFF
		--
		when "111" => -- $E000 - $FFFF
 		   cpu_data_in <= rom_data_out;
			dat_cs      <= cpu_vma;
			ram_cs      <= '0';
			uart_cs     <= '0';
			cf_cs       <= '0';
			timer_cs    <= '0';
			ioport_cs   <= '0';
		--
		-- I/O Space at $8000 - $9FFF
		--
		when "100" => -- $8000 - $9FFF
			dat_cs      <= '0';
			ram_cs      <= '0';
		   case cpu_addr(6 downto 4) is
			--
			-- UART $8004
			--
			when "000" => -- $8000 - $800F
		     cpu_data_in <= uart_data_out;
			  uart_cs     <= cpu_vma;
			  cf_cs       <= '0';
			  timer_cs    <= '0';
			  ioport_cs   <= '0';
			--
			-- Compact Flash $8010
			--
			when "001" => -- $8010 - $801F
           cpu_data_in <= cf_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= cpu_vma;
			  timer_cs    <= '0';
           ioport_cs   <= '0';
         --
			-- Timer $8020
			--
			when "010" => -- $8020 - $802F
           cpu_data_in <= timer_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= cpu_vma;
			  ioport_cs   <= '0';
			--
			-- I/O Port $8030
			--
			when "011" => -- $8030 - $803F
           cpu_data_in <= ioport_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
			  timer_cs    <= '0';
           ioport_cs   <= cpu_vma;
			--
			-- Empty
			--
			when others => -- $8040 to $9FFF
           cpu_data_in <= "00000000";
			  uart_cs     <= '0';
			  cf_cs       <= '0';
			  timer_cs    <= '0';
			  ioport_cs   <= '0';
		   end case;
		--
		-- The rest is all RAM
		--
		when others =>
		  cpu_data_in <= ram_data_out;
		  ram_cs      <= cpu_vma;
		  dat_cs      <= '0';
		  uart_cs     <= '0';
		  cf_cs       <= '0';
		  timer_cs    <= '0';
		  ioport_cs   <= '0';
	 end case;
end process;

----------------------------------------------------------------------
--
--  Processes to read and write external RAM
--
----------------------------------------------------------------------

my_ram: process( SysClk, Reset_n,
                 cpu_addr, cpu_rw, cpu_data_out,
                 ram_cs, ram_wrl, ram_wru,
					  ram_data, dat_data_out )
begin
    ram_csn <= not( ram_cs and Reset_n );
	 ram_wrl  <= (not dat_data_out(5)) and (not cpu_rw) and SysClk;
	 ram_wrln <= not ram_wrl;
    ram_wru  <= dat_data_out(5) and (not cpu_rw) and SysClk;
	 ram_wrun <= not ram_wru;
	 ram_addr(16 downto 12) <= dat_data_out(4 downto 0);
	 ram_addr(11 downto 0) <= cpu_addr(11 downto 0);

    if ram_wrl = '1' then
		ram_data(7 downto 0) <= cpu_data_out;
	 else
      ram_data(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

	 if ram_wru = '1' then
		ram_data(15 downto 8) <= cpu_data_out;
	 else
      ram_data(15 downto 8)  <= "ZZZZZZZZ";
    end if;

	 if dat_data_out(5) = '1' then
      ram_data_out <= ram_data(15 downto 8);
	 else
      ram_data_out <= ram_data(7 downto 0);
    end if;
end process;


--
-- B5-CF Compact Flash Control
--
b5_cf: process( Reset_n,
                cpu_addr, cpu_rw, cpu_data_out,
					 cf_cs, cf_rd, cf_wr, cf_d )
begin
	 cf_rst_n  <= Reset_n;
	 cf_cs0_n  <= not( cf_cs ) or cpu_addr(3);
	 cf_cs1_n  <= not( cf_cs and cpu_addr(3));
	 cf_cs16_n <= '1';
	 cf_wr     <= cf_cs and (not cpu_rw);
	 cf_rd     <= cf_cs and cpu_rw;
	 cf_wr_n   <= not cf_wr;
	 cf_rd_n   <= not cf_rd;
	 cf_a      <= cpu_addr(2 downto 0);
	 if cf_wr = '1' then
	   cf_d(7 downto 0) <= cpu_data_out;
	 else
	   cf_d(7 downto 0) <= "ZZZZZZZZ";
	 end if;
	 cf_data_out <= cf_d(7 downto 0);
	 cf_d(15 downto 8) <= "ZZZZZZZZ";
end process;

--
-- tie together interrupts
--
interrupts : process( Reset_n, timer_irq, uart_irq )
begin
 	 cpu_reset <= not Reset_n; -- CPU reset is active high
    cpu_irq   <= uart_irq;
	 cpu_xirq  <= timer_irq;
end process;

--
--
--clock_gen : process( SysClk, e_clk )
--begin
--  if SysClk'event and SysClk='0' then
--    e_clk <= not e_clk;
--  end if;
--end process;

  --
  -- flash led to indicate code is working
  --
  increment: process (SysClk, CountL )
  begin
    if(SysClk'event and SysClk = '0') then
      countL <= countL + 1;			 
    end if;
	 LED <= countL(21);
  end process;



  --
  -- CRTC output signals
  --
--	 v_drive     <= '0';
--    h_drive     <= '0';
--    blue_lo     <= '0';
--    blue_hi     <= '0';
--    green_lo    <= '0';
--    green_hi    <= '0';
--    red_lo      <= '0';
--    red_hi      <= '0';
--	 buzzer      <= '0';
  
end my_computer; --===================== End of architecture =======================--

