--===========================================================================--
--
-- CPU11 Microprocessor Test Bench 4
--
-- Complete system test
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

entity my_testbench4 is
end my_testbench4;

-------------------------------------------------------------------------------
-- Architecture for System11 Testbench 4
-------------------------------------------------------------------------------
architecture behavior of my_testbench4 is

signal    tb_SysClk       : Std_Logic;  -- System Clock input
signal	 tb_Reset_n      : Std_logic;  -- Master Reset input (active low)
signal    tb_LED          : std_logic;  -- Diagnostic LED Flasher

    -- Memory Interface signals
signal    tb_ram_csn      : Std_Logic;
signal    tb_ram_wrln     : Std_Logic;
signal    tb_ram_wrun     : Std_Logic;
signal    tb_ram_addr     : Std_Logic_Vector(16 downto 0);
signal    tb_ram_data     : Std_Logic_Vector(15 downto 0);
	 -- Uart Interface
signal    tb_rxbit        : Std_Logic;
signal	 tb_txbit        : Std_Logic;
signal    tb_rts_n        : Std_Logic;
signal    tb_cts_n        : Std_Logic;
    -- Compact Flash B5-CF Module
signal    tb_cf_rst_n     : std_logic;
signal	 tb_cf_cs0_n     : std_logic;
signal	 tb_cf_cs1_n     : std_logic;
signal    tb_cf_rd_n      : std_logic;
signal    tb_cf_wr_n      : std_logic;
signal	 tb_cf_cs16_n    : std_logic;
signal    tb_cf_a         : std_logic_vector(2 downto 0);
signal    tb_cf_d         : std_logic_vector(15 downto 0);
-- I/O Ports
signal    tb_Porta        : std_logic_vector(7 downto 0);
signal    tb_Portb        : std_logic_vector(7 downto 0);
--signal    tb_PortC        : std_logic_vector(7 downto 0);
--signal    tb_PortD        : std_logic_vector(7 downto 0);

-- CPU bus
signal	 tb_bus_clk      : std_logic;
signal	 tb_bus_reset    : std_logic;
signal	 tb_bus_rw       : std_logic;
signal	 tb_bus_cs       : std_logic;
signal    tb_bus_addr     : std_logic_vector(15 downto 0);
signal	 tb_bus_data     : std_logic_vector(7 downto 0);

signal    tb_reset        : std_logic;
signal    tb_ram_cs       : std_logic;
signal    tb_ramlo_din    : std_logic_vector(7 downto 0);
signal    tb_ramlo_dout	  : std_logic_vector(7 downto 0);
signal    tb_ramhi_din    : std_logic_vector(7 downto 0);
signal    tb_ramhi_dout	  : std_logic_vector(7 downto 0);

-- Timer I/O
signal	 tb_timer_out    : std_logic;

component System11
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
end component;

component my_ram
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(5 downto 0);
	 data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0)
	 );
end component;

begin

my_system11:  System11 port map (
    SysClk     => tb_SysClk,  -- System Clock input
	 Reset_n    => tb_Reset_n,  -- Master Reset input (active low)
    LED        => tb_LED,  -- Diagnostic LED Flasher

    -- Memory Interface signals
    ram_csn     => tb_ram_csn,
    ram_wrln    => tb_ram_wrln,
    ram_wrun    => tb_ram_wrun,
    ram_addr    => tb_ram_addr,
    ram_data    => tb_ram_data,

	 -- Stuff on the peripheral board

 	 -- PS/2 Keyboard
--	 kb_clock   => tb_kb_clk.
--	 kb_data    => tb_kb_data,

	 -- PS/2 Mouse interface
--	 mouse_clock => tb_mouse_clock,
--	 mouse_data  => tb_mouse_data,

	 -- Uart Interface
    rxbit       => tb_rxbit,
	 txbit       => tb_txbit,
    rts_n       => tb_rts_n,
    cts_n       => tb_cts_n,

	 -- CRTC output signals
--		v_drive    => tb_v_drive,
--    h_drive    => tb_h_drive,
--    blue_lo    => tb_blue_lo,
--    blue_hi    => tb_blue_hi,
--    green_lo   => tb_green_lo,
--    green_hi   => tb_green_hi,
--    red_lo     => tb_red_lo,
--    red_hi     => tb_red_hi,
--	   buzzer     => tb_buzzer,

    -- Compact Flash B5-CF Module
    cf_rst_n     => tb_cf_rst_n,
	 cf_cs0_n     => tb_cf_cs0_n,
	 cf_cs1_n     => tb_cf_cs1_n,
    cf_rd_n      => tb_cf_rd_n,
    cf_wr_n      => tb_cf_wr_n,
	 cf_cs16_n    => tb_cf_cs16_n,
    cf_a         => tb_cf_a,
    cf_d         => tb_cf_d,
--    cf_intrq     => tb_cf_intrq,
--    cf_iordy     => tb_cf_iordy,
--  	cf_dase      => tb_cf_dase,
--	   cf_pdiag     => tb_cf_pdiag,
--	   cf_present   => tb_cf_present,

-- I/O Ports
    Porta        => tb_Porta,
    Portb        => tb_Portb,
--    PortC        => tb_Portc,
--    PortD        => tb_portd,

-- CPU bus
	 bus_clk      => tb_bus_clk,
	 bus_reset    => tb_bus_reset,
	 bus_rw       => tb_bus_rw,
	 bus_cs       => tb_bus_cs,
    bus_addr     => tb_bus_addr,
	 bus_data     => tb_bus_data,

-- Timer I/O
	 timer_out    => tb_timer_out
	 );

ramlo : my_ram port map (	
	 clk       => tb_bus_clk,
    rst       => tb_reset,
    cs        => tb_ram_cs,
    rw        => tb_ram_wrln,
    addr      => tb_ram_addr(5 downto 0),
	 data_in   => tb_ramlo_din,
	 data_out  => tb_ramlo_dout
	 );

ramhi : my_ram port map (	
	 clk       => tb_bus_clk,
    rst       => tb_reset,
    cs        => tb_ram_cs,
    rw        => tb_ram_wrun,
    addr      => tb_ram_addr(5 downto 0),
	 data_in   => tb_ramhi_din,
	 data_out  => tb_ramhi_dout
	 );

   tb_ram : PROCESS( tb_reset_n, tb_ram_csn, tb_ram_wrln, tb_ram_wrun, 
	              tb_ramlo_din, tb_ramlo_dout, tb_ramhi_din, tb_ramhi_dout,
					  tb_ram_data )
	 begin
      tb_reset      <= not tb_reset_n;
      tb_ram_cs     <= not tb_ram_csn;

	   if( tb_ram_wrln = '1' ) then
        tb_ram_data( 7 downto 0) <= tb_ramlo_dout;
      else
        tb_ram_data( 7 downto 0) <= "ZZZZZZZZ";
        tb_ramlo_din  <= tb_ram_data(7 downto 0);
      end if;

	   if( tb_ram_wrun = '1' ) then
        tb_ram_data( 15 downto 8) <= tb_ramhi_dout;
      else
        tb_ram_data( 15 downto 8) <= "ZZZZZZZZ";
        tb_ramhi_din  <= tb_ram_data( 15 downto 8);
      end if;
	 end process;

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

		tb_rxbit      <= '1';
		tb_cts_n      <= '0';
 	   tb_reset_n    <= '0';
	   tb_SysClk     <= '0';

		for count in 0 to 512 loop
			tb_SysClk <= '0';
			if count = 0 then
				tb_reset_n <= '0';
			elsif count = 1 then
				tb_reset_n <= '1';
			end if;
			wait for 50 ns;
			tb_SysClk <= '1';
			wait for 50 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***

 
end behavior; --===================== End of architecture =======================--

