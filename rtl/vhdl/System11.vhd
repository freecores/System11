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
--                   miniuart   (miniUART3.vhd)  UART
--                      rxunit  (rxunit3.vhd)
--                      txunit  (txunit3.vhd)
--                   ioport     (ioport.vhd)    parallel i/o port
--                   timer      (timer.vhd)     small counter timer
--
-------------------------------------------------------------------------------
-- Revision list
-- Version   Author         Date                Changes
-- 1.0       John Kent      6 September 2003 	Initial release to open corse
--	1.1       John Kent      3 April 2004        Change MiniUart to version with external Baud clock.
--                                              Added Baud Clock for 57.6 KBd at 25MHz
--																Added System clock divider to run CPU at 12.5MHz
--																Added Bus I/O in I/O map.
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

 	 -- PS/2 Keyboard
--	 kb_clock    : inout Std_logic;
--	 kb_data     : inout Std_Logic;

	 -- PS/2 Mouse interface
--	 mouse_clock : in  Std_Logic;
--	 mouse_data  : in  Std_Logic;

	 -- Uart Interface
    rxbit       : in  Std_Logic;
	 txbit       : out Std_Logic;
    rts_n       : out Std_Logic;
    cts_n       : in  Std_Logic;

	 -- CRTC output signals
--		v_drive     : out Std_Logic;
--    h_drive     : out Std_Logic;
--    blue_lo     : out std_logic;
--    blue_hi     : out std_logic;
--    green_lo    : out std_logic;
--    green_hi    : out std_logic;
--    red_lo      : out std_logic;
--    red_hi      : out std_logic;
--	 buzzer      : out std_logic;

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

-- I/O Ports
    Porta        : inout std_logic_vector(7 downto 0);
    Portb        : inout std_logic_vector(7 downto 0);
--    PortC        : inout std_logic_vector(7 downto 0);
--    PortD        : inout std_logic_vector(7 downto 0);

-- CPU bus
	 bus_clk      : out std_logic;
	 bus_reset    : out std_logic;
	 bus_rw       : out std_logic;
	 bus_cs       : out std_logic;
    bus_addr     : out std_logic_vector(15 downto 0);
	 bus_data     : inout std_logic_vector(7 downto 0);

-- Timer I/O
	 timer_out    : out std_logic
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
  signal baudclk       : Std_Logic;
  signal DCD_n         : Std_Logic;

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
  signal BaudCount   : std_logic_vector(4 downto 0);

  -- attribute buffer_type : string; 
  -- attribute buffer_type of cpu_clk : signal is "BUFG"; 

-----------------------------------------------------------------
--
-- CPU11 Core
--
-----------------------------------------------------------------

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
	 xirq:     in  std_logic
  );
end component;


-----------------------------------------------------------------
--
-- Open Cores Mini UART
--
-----------------------------------------------------------------

component miniUART
  port (
     clk      : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input (active high)
     cs       : in  Std_Logic;  -- miniUART Chip Select
     rw       : in  Std_Logic;  -- Read / Not Write
     irq      : out Std_Logic;  -- Interrupt
     Addr     : in  Std_Logic;  -- Register Select
     DataIn   : in  Std_Logic_Vector(7 downto 0); -- Data Bus In 
     DataOut  : out Std_Logic_Vector(7 downto 0); -- Data Bus Out
     RxC      : in  Std_Logic;  -- Receive Baud Clock
     TxC      : in  Std_Logic;  -- Transmit Baud Clock
     RxD      : in  Std_Logic;  -- Receive Data
     TxD      : out Std_Logic;  -- Transmit Data
	  DCD_n    : in  Std_Logic;  -- Data Carrier Detect
     CTS_n    : in  Std_Logic;  -- Clear To Send
     RTS_n    : out Std_Logic );  -- Request To send
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

component BUFG
  port (
     i: in std_logic;
	  o: out std_logic
 );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_cpu : cpu11  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 irq       => cpu_irq,
	 xirq      => cpu_xirq
  );

my_uart  : miniUART port map (
	 clk	     => cpu_clk,
	 rst       => cpu_reset,
    cs        => uart_cs,
	 rw        => cpu_rw,
    irq       => uart_irq,
    Addr      => cpu_addr(0),
	 Datain    => cpu_data_out,
	 DataOut   => uart_data_out,
	 RxC       => baudclk,
	 TxC       => baudclk,
	 RxD       => rxbit,
	 TxD       => txbit,
	 DCD_n     => dcd_n,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n
	 );


my_ioport  : ioport port map (
    clk       => cpu_clk,
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
    clk       => cpu_clk,
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


my_dat : dat_ram port map (
    clk        => cpu_clk,
	 rst        => cpu_reset,
	 cs         => dat_cs,
	 rw         => cpu_rw,
	 addr_hi    => cpu_addr(15 downto 12),
	 addr_lo    => cpu_addr(3 downto 0),
    data_in    => cpu_data_out,
	 data_out   => dat_data_out(7 downto 0)
	 );

my_rom : boot_rom port map (
	 addr       => cpu_addr(9 downto 0),
    data       => rom_data_out
	 );


clk_buffer : BUFG port map(
      i => countL(0),
	   o => cpu_clk
   );	 

	 
----------------------------------------------------------------------
--
--  Processes to read and write memory based on bus signals
--
----------------------------------------------------------------------

my_decoder: process(
                 cpu_addr, cpu_vma,
					  rom_data_out, ram_data_out, bus_data,
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
			bus_cs      <= '0';

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
			  bus_cs      <= '0';
			--
			-- Compact Flash $8010
			--
			when "001" => -- $8010 - $801F
           cpu_data_in <= cf_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= cpu_vma;
			  timer_cs    <= '0';
           ioport_cs   <= '0';
			  bus_cs      <= '0';
         --
			-- Timer $8020
			--
			when "010" => -- $8020 - $802F
           cpu_data_in <= timer_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= cpu_vma;
			  ioport_cs   <= '0';
			  bus_cs      <= '0';
			--
			-- I/O Port $8030
			--
			when "011" => -- $8030 - $803F
           cpu_data_in <= ioport_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
			  timer_cs    <= '0';
           ioport_cs   <= cpu_vma;
			  bus_cs      <= '0';
			--
			-- Empty
			--
			when others => -- $8040 to $9FFF
           cpu_data_in <= bus_data;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
			  timer_cs    <= '0';
			  ioport_cs   <= '0';
			  bus_cs      <= cpu_vma;
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
		  bus_cs      <= '0';
	   end case;
end process;

----------------------------------------------------------------------
--
--  Processes to read and write external RAM
--
----------------------------------------------------------------------

my_ram: process( cpu_clk, Reset_n,
                 cpu_addr, cpu_rw, cpu_data_out,
                 ram_cs, ram_wrl, ram_wru,
					  ram_data, dat_data_out )
begin
    ram_csn <= not( ram_cs and Reset_n );
	 ram_wrl  <= (not cpu_addr(0)) and (not cpu_rw) and cpu_clk;
	 ram_wrln <= not ram_wrl;
    ram_wru  <= cpu_addr(0) and (not cpu_rw) and cpu_clk;
	 ram_wrun <= not ram_wru;
	 ram_addr(16 downto 11) <= dat_data_out(5 downto 0);
	 ram_addr(10 downto 0)  <= cpu_addr(11 downto 1);

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

	 if cpu_addr(0) = '1' then
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
-- CPU bus signals
--
my_bus : process( cpu_clk, cpu_reset, cpu_rw, cpu_addr, cpu_data_out )
begin
	bus_clk   <= cpu_clk;
   bus_reset <= cpu_reset;
	bus_rw    <= cpu_rw;
   bus_addr  <= cpu_addr;
	if( cpu_rw = '0' ) then
	   bus_data <= cpu_data_out;
   else
	   bus_data <= "ZZZZZZZZ";
   end if;
end process;

  --
  -- flash led to indicate code is working
  --
increment: process (SysClk, Reset_n, CountL )
begin
    if( Reset_n = '0' )	then
	   countL <= "000000000000000000000000";
    elsif(SysClk'event and SysClk = '0') then
      countL <= countL + 1;			 
    end if;
	 LED     <= countL(22);
--	 cpu_clk <= countL(0);
--	 baudclk <= countL(5);  -- 9.8MHz / 64 = 153,600 KHz =  9600Bd * 16
--	 baudclk <= countL(4);  -- 9.8MHz / 32 = 307,200 KHz = 19200Bd * 16
--	 baudclk <= countL(3);  -- 9.8MHz / 16 = 614,400 KHz = 38400Bd * 16
--  baudclk <= countL(2);  -- 4.9MHz / 8  = 614,400 KHz = 38400Bd * 16
end process;

my_clock: process( SysClk, Reset_n, BaudCount )
begin
    if(SysClk'event and SysClk = '0') then
      if( Reset_n = '0' )	then
	     BaudCount <= "00000";
		elsif( BaudCount = 26 )	then
		   BaudCount <= "00000";
		else
		   BaudCount <= BaudCount + 1;
		end if;			 
    end if;
    baudclk <= BaudCount(4);  -- 25MHz / 27  = 926,000 KHz = 57,870Bd * 16
	 dcd_n <= '0';
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

