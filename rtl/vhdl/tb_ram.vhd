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

entity my_ram is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(5 downto 0);
	 data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0));
end;


architecture ram_arch of my_ram is
signal ram_reg00 : std_logic_vector(7 downto 0);
signal ram_reg01 : std_logic_vector(7 downto 0);
signal ram_reg02 : std_logic_vector(7 downto 0);
signal ram_reg03 : std_logic_vector(7 downto 0);
signal ram_reg04 : std_logic_vector(7 downto 0);
signal ram_reg05 : std_logic_vector(7 downto 0);
signal ram_reg06 : std_logic_vector(7 downto 0);
signal ram_reg07 : std_logic_vector(7 downto 0);
signal ram_reg08 : std_logic_vector(7 downto 0);
signal ram_reg09 : std_logic_vector(7 downto 0);
signal ram_reg010 : std_logic_vector(7 downto 0);
signal ram_reg011 : std_logic_vector(7 downto 0);
signal ram_reg012 : std_logic_vector(7 downto 0);
signal ram_reg013 : std_logic_vector(7 downto 0);
signal ram_reg014 : std_logic_vector(7 downto 0);
signal ram_reg015 : std_logic_vector(7 downto 0);
signal ram_reg10 : std_logic_vector(7 downto 0);
signal ram_reg11 : std_logic_vector(7 downto 0);
signal ram_reg12 : std_logic_vector(7 downto 0);
signal ram_reg13 : std_logic_vector(7 downto 0);
signal ram_reg14 : std_logic_vector(7 downto 0);
signal ram_reg15 : std_logic_vector(7 downto 0);
signal ram_reg16 : std_logic_vector(7 downto 0);
signal ram_reg17 : std_logic_vector(7 downto 0);
signal ram_reg18 : std_logic_vector(7 downto 0);
signal ram_reg19 : std_logic_vector(7 downto 0);
signal ram_reg110 : std_logic_vector(7 downto 0);
signal ram_reg111 : std_logic_vector(7 downto 0);
signal ram_reg112 : std_logic_vector(7 downto 0);
signal ram_reg113 : std_logic_vector(7 downto 0);
signal ram_reg114 : std_logic_vector(7 downto 0);
signal ram_reg115 : std_logic_vector(7 downto 0);
signal ram_reg20 : std_logic_vector(7 downto 0);
signal ram_reg21 : std_logic_vector(7 downto 0);
signal ram_reg22 : std_logic_vector(7 downto 0);
signal ram_reg23 : std_logic_vector(7 downto 0);
signal ram_reg24 : std_logic_vector(7 downto 0);
signal ram_reg25 : std_logic_vector(7 downto 0);
signal ram_reg26 : std_logic_vector(7 downto 0);
signal ram_reg27 : std_logic_vector(7 downto 0);
signal ram_reg28 : std_logic_vector(7 downto 0);
signal ram_reg29 : std_logic_vector(7 downto 0);
signal ram_reg210 : std_logic_vector(7 downto 0);
signal ram_reg211 : std_logic_vector(7 downto 0);
signal ram_reg212 : std_logic_vector(7 downto 0);
signal ram_reg213 : std_logic_vector(7 downto 0);
signal ram_reg214 : std_logic_vector(7 downto 0);
signal ram_reg215 : std_logic_vector(7 downto 0);
signal ram_reg30 : std_logic_vector(7 downto 0);
signal ram_reg31 : std_logic_vector(7 downto 0);
signal ram_reg32 : std_logic_vector(7 downto 0);
signal ram_reg33 : std_logic_vector(7 downto 0);
signal ram_reg34 : std_logic_vector(7 downto 0);
signal ram_reg35 : std_logic_vector(7 downto 0);
signal ram_reg36 : std_logic_vector(7 downto 0);
signal ram_reg37 : std_logic_vector(7 downto 0);
signal ram_reg38 : std_logic_vector(7 downto 0);
signal ram_reg39 : std_logic_vector(7 downto 0);
signal ram_reg310 : std_logic_vector(7 downto 0);
signal ram_reg311 : std_logic_vector(7 downto 0);
signal ram_reg312 : std_logic_vector(7 downto 0);
signal ram_reg313 : std_logic_vector(7 downto 0);
signal ram_reg314 : std_logic_vector(7 downto 0);
signal ram_reg315 : std_logic_vector(7 downto 0);

begin


---------------------------------
--
-- Write DAT RAM
--
---------------------------------

ram_write : process( clk, rst, addr, cs, rw, data_in )
begin
  if rst = '1' then
      ram_reg00 <= "00000000";
      ram_reg01 <= "00000000";
      ram_reg02 <= "00000000";
      ram_reg03 <= "00000000";
      ram_reg04 <= "00000000";
      ram_reg05 <= "00000000";
      ram_reg06 <= "00000000";
      ram_reg07 <= "00000000";
      ram_reg08 <= "00000000";
      ram_reg09 <= "00000000";
      ram_reg010 <= "00000000";
      ram_reg011 <= "00000000";
      ram_reg012 <= "00000000";
      ram_reg013 <= "00000000";
      ram_reg014 <= "00000000";
      ram_reg015 <= "00000000";
      ram_reg10 <= "00000000";
      ram_reg11 <= "00000000";
      ram_reg12 <= "00000000";
      ram_reg13 <= "00000000";
      ram_reg14 <= "00000000";
      ram_reg15 <= "00000000";
      ram_reg16 <= "00000000";
      ram_reg17 <= "00000000";
      ram_reg18 <= "00000000";
      ram_reg19 <= "00000000";
      ram_reg110 <= "00000000";
      ram_reg111 <= "00000000";
      ram_reg112 <= "00000000";
      ram_reg113 <= "00000000";
      ram_reg114 <= "00000000";
      ram_reg115 <= "00000000";
      ram_reg20 <= "00000000";
      ram_reg21 <= "00000000";
      ram_reg22 <= "00000000";
      ram_reg23 <= "00000000";
      ram_reg24 <= "00000000";
      ram_reg25 <= "00000000";
      ram_reg26 <= "00000000";
      ram_reg27 <= "00000000";
      ram_reg28 <= "00000000";
      ram_reg29 <= "00000000";
      ram_reg210 <= "00000000";
      ram_reg211 <= "00000000";
      ram_reg212 <= "00000000";
      ram_reg213 <= "00000000";
      ram_reg214 <= "00000000";
      ram_reg215 <= "00000000";
      ram_reg30 <= "00000000";
      ram_reg31 <= "00000000";
      ram_reg32 <= "00000000";
      ram_reg33 <= "00000000";
      ram_reg34 <= "00000000";
      ram_reg35 <= "00000000";
      ram_reg36 <= "00000000";
      ram_reg37 <= "00000000";
      ram_reg38 <= "00000000";
      ram_reg39 <= "00000000";
      ram_reg310 <= "00000000";
      ram_reg311 <= "00000000";
      ram_reg312 <= "00000000";
      ram_reg313 <= "00000000";
      ram_reg314 <= "00000000";
      ram_reg315 <= "00000000";
  elsif clk'event and clk = '0' then
	   if cs = '1' and rw = '0' then
        case addr is
	     when "000000" =>
		    ram_reg00(7 downto 0) <= data_in(7 downto 0);
	     when "000001" =>
		    ram_reg01(7 downto 0) <= data_in(7 downto 0);
	     when "000010" =>
		    ram_reg02(7 downto 0) <= data_in(7 downto 0);
	     when "000011" =>
		    ram_reg03(7 downto 0) <= data_in(7 downto 0);
	     when "000100" =>
		    ram_reg04(7 downto 0) <= data_in(7 downto 0);
	     when "000101" =>
		    ram_reg05(7 downto 0) <= data_in(7 downto 0);
	     when "000110" =>
		    ram_reg06(7 downto 0) <= data_in(7 downto 0);
	     when "000111" =>
		    ram_reg07(7 downto 0) <= data_in(7 downto 0);
	     when "001000" =>
		    ram_reg08(7 downto 0) <= data_in(7 downto 0);
	     when "001001" =>
		    ram_reg09(7 downto 0) <= data_in(7 downto 0);
	     when "001010" =>
		    ram_reg010(7 downto 0) <= data_in(7 downto 0);
	     when "001011" =>
		    ram_reg011(7 downto 0) <= data_in(7 downto 0);
	     when "001100" =>
		    ram_reg012(7 downto 0) <= data_in(7 downto 0);
	     when "001101" =>
		    ram_reg013(7 downto 0) <= data_in(7 downto 0);
	     when "001110" =>
		    ram_reg014(7 downto 0) <= data_in(7 downto 0);
	     when "001111" =>
		    ram_reg015(7 downto 0) <= data_in(7 downto 0);
	     when "010000" =>
		    ram_reg10(7 downto 0) <= data_in(7 downto 0);
	     when "010001" =>
		    ram_reg11(7 downto 0) <= data_in(7 downto 0);
	     when "010010" =>
		    ram_reg12(7 downto 0) <= data_in(7 downto 0);
	     when "010011" =>
		    ram_reg13(7 downto 0) <= data_in(7 downto 0);
	     when "010100" =>
		    ram_reg14(7 downto 0) <= data_in(7 downto 0);
	     when "010101" =>
		    ram_reg15(7 downto 0) <= data_in(7 downto 0);
	     when "010110" =>
		    ram_reg16(7 downto 0) <= data_in(7 downto 0);
	     when "010111" =>
		    ram_reg17(7 downto 0) <= data_in(7 downto 0);
	     when "011000" =>
		    ram_reg18(7 downto 0) <= data_in(7 downto 0);
	     when "011001" =>
		    ram_reg19(7 downto 0) <= data_in(7 downto 0);
	     when "011010" =>
		    ram_reg110(7 downto 0) <= data_in(7 downto 0);
	     when "011011" =>
		    ram_reg111(7 downto 0) <= data_in(7 downto 0);
	     when "011100" =>
		    ram_reg112(7 downto 0) <= data_in(7 downto 0);
	     when "011101" =>
		    ram_reg113(7 downto 0) <= data_in(7 downto 0);
	     when "011110" =>
		    ram_reg114(7 downto 0) <= data_in(7 downto 0);
	     when "011111" =>
		    ram_reg115(7 downto 0) <= data_in(7 downto 0);
	     when "100000" =>
		    ram_reg20(7 downto 0) <= data_in(7 downto 0);
	     when "100001" =>
		    ram_reg21(7 downto 0) <= data_in(7 downto 0);
	     when "100010" =>
		    ram_reg22(7 downto 0) <= data_in(7 downto 0);
	     when "100011" =>
		    ram_reg23(7 downto 0) <= data_in(7 downto 0);
	     when "100100" =>
		    ram_reg24(7 downto 0) <= data_in(7 downto 0);
	     when "100101" =>
		    ram_reg25(7 downto 0) <= data_in(7 downto 0);
	     when "100110" =>
		    ram_reg26(7 downto 0) <= data_in(7 downto 0);
	     when "100111" =>
		    ram_reg27(7 downto 0) <= data_in(7 downto 0);
	     when "101000" =>
		    ram_reg28(7 downto 0) <= data_in(7 downto 0);
	     when "101001" =>
		    ram_reg29(7 downto 0) <= data_in(7 downto 0);
	     when "101010" =>
		    ram_reg210(7 downto 0) <= data_in(7 downto 0);
	     when "101011" =>
		    ram_reg211(7 downto 0) <= data_in(7 downto 0);
	     when "101100" =>
		    ram_reg212(7 downto 0) <= data_in(7 downto 0);
	     when "101101" =>
		    ram_reg213(7 downto 0) <= data_in(7 downto 0);
	     when "101110" =>
		    ram_reg214(7 downto 0) <= data_in(7 downto 0);
	     when "101111" =>
		    ram_reg215(7 downto 0) <= data_in(7 downto 0);
	     when "110000" =>
		    ram_reg30(7 downto 0) <= data_in(7 downto 0);
	     when "110001" =>
		    ram_reg31(7 downto 0) <= data_in(7 downto 0);
	     when "110010" =>
		    ram_reg32(7 downto 0) <= data_in(7 downto 0);
	     when "110011" =>
		    ram_reg33(7 downto 0) <= data_in(7 downto 0);
	     when "110100" =>
		    ram_reg34(7 downto 0) <= data_in(7 downto 0);
	     when "110101" =>
		    ram_reg35(7 downto 0) <= data_in(7 downto 0);
	     when "110110" =>
		    ram_reg36(7 downto 0) <= data_in(7 downto 0);
	     when "110111" =>
		    ram_reg37(7 downto 0) <= data_in(7 downto 0);
	     when "111000" =>
		    ram_reg38(7 downto 0) <= data_in(7 downto 0);
	     when "111001" =>
		    ram_reg39(7 downto 0) <= data_in(7 downto 0);
	     when "111010" =>
		    ram_reg310(7 downto 0) <= data_in(7 downto 0);
	     when "111011" =>
		    ram_reg311(7 downto 0) <= data_in(7 downto 0);
	     when "111100" =>
		    ram_reg312(7 downto 0) <= data_in(7 downto 0);
	     when "111101" =>
		    ram_reg313(7 downto 0) <= data_in(7 downto 0);
	     when "111110" =>
		    ram_reg314(7 downto 0) <= data_in(7 downto 0);
	     when "111111" =>
		    ram_reg315(7 downto 0) <= data_in(7 downto 0);
        when others =>
		    null;
		  end case;
	   end if;
  end if;
end process;

dat_read : process(  addr,
                     ram_reg00, ram_reg01, ram_reg02, ram_reg03,
                     ram_reg04, ram_reg05, ram_reg06, ram_reg07,
                     ram_reg08, ram_reg09, ram_reg010, ram_reg011,
                     ram_reg012, ram_reg013, ram_reg014, ram_reg015,
                     ram_reg10, ram_reg11, ram_reg12, ram_reg13,
                     ram_reg14, ram_reg15, ram_reg16, ram_reg17,
                     ram_reg18, ram_reg19, ram_reg110, ram_reg111,
                     ram_reg112, ram_reg113, ram_reg114, ram_reg115,
                     ram_reg20, ram_reg21, ram_reg22, ram_reg23,
                     ram_reg24, ram_reg25, ram_reg26, ram_reg27,
                     ram_reg28, ram_reg29, ram_reg210, ram_reg211,
                     ram_reg212, ram_reg213, ram_reg214, ram_reg215,
                     ram_reg30, ram_reg31, ram_reg32, ram_reg33,
                     ram_reg34, ram_reg35, ram_reg36, ram_reg37,
                     ram_reg38, ram_reg39, ram_reg310, ram_reg311,
                     ram_reg312, ram_reg313, ram_reg314, ram_reg315
							 )
begin
      case addr is
	     when "000000" =>
		    data_out <= ram_reg00;
	     when "000001" =>
		    data_out <= ram_reg01;
	     when "000010" =>
		    data_out <= ram_reg02;
	     when "000011" =>
		    data_out <= ram_reg03;
	     when "000100" =>
		    data_out <= ram_reg04;
	     when "000101" =>
		    data_out <= ram_reg05;
	     when "000110" =>
		    data_out <= ram_reg06;
	     when "000111" =>
		    data_out <= ram_reg07;
	     when "001000" =>
		    data_out <= ram_reg08;
	     when "001001" =>
		    data_out <= ram_reg09;
	     when "001010" =>
		    data_out <= ram_reg010;
	     when "001011" =>
		    data_out <= ram_reg011;
	     when "001100" =>
		    data_out <= ram_reg012;
	     when "001101" =>
		    data_out <= ram_reg013;
	     when "001110" =>
		    data_out <= ram_reg014;
	     when "001111" =>
		    data_out <= ram_reg015;
	     when "010000" =>
		    data_out <= ram_reg10;
	     when "010001" =>
		    data_out <= ram_reg11;
	     when "010010" =>
		    data_out <= ram_reg12;
	     when "010011" =>
		    data_out <= ram_reg13;
	     when "010100" =>
		    data_out <= ram_reg14;
	     when "010101" =>
		    data_out <= ram_reg15;
	     when "010110" =>
		    data_out <= ram_reg16;
	     when "010111" =>
		    data_out <= ram_reg17;
	     when "011000" =>
		    data_out <= ram_reg18;
	     when "011001" =>
		    data_out <= ram_reg19;
	     when "011010" =>
		    data_out <= ram_reg110;
	     when "011011" =>
		    data_out <= ram_reg111;
	     when "011100" =>
		    data_out <= ram_reg112;
	     when "011101" =>
		    data_out <= ram_reg113;
	     when "011110" =>
		    data_out <= ram_reg114;
	     when "011111" =>
		    data_out <= ram_reg115;
	     when "100000" =>
		    data_out <= ram_reg20;
	     when "100001" =>
		    data_out <= ram_reg21;
	     when "100010" =>
		    data_out <= ram_reg22;
	     when "100011" =>
		    data_out <= ram_reg23;
	     when "100100" =>
		    data_out <= ram_reg24;
	     when "100101" =>
		    data_out <= ram_reg25;
	     when "100110" =>
		    data_out <= ram_reg26;
	     when "100111" =>
		    data_out <= ram_reg27;
	     when "101000" =>
		    data_out <= ram_reg28;
	     when "101001" =>
		    data_out <= ram_reg29;
	     when "101010" =>
		    data_out <= ram_reg210;
	     when "101011" =>
		    data_out <= ram_reg211;
	     when "101100" =>
		    data_out <= ram_reg212;
	     when "101101" =>
		    data_out <= ram_reg213;
	     when "101110" =>
		    data_out <= ram_reg214;
	     when "101111" =>
		    data_out <= ram_reg215;
	     when "110000" =>
		    data_out <= ram_reg30;
	     when "110001" =>
		    data_out <= ram_reg31;
	     when "110010" =>
		    data_out <= ram_reg32;
	     when "110011" =>
		    data_out <= ram_reg33;
	     when "110100" =>
		    data_out <= ram_reg34;
	     when "110101" =>
		    data_out <= ram_reg35;
	     when "110110" =>
		    data_out <= ram_reg36;
	     when "110111" =>
		    data_out <= ram_reg37;
	     when "111000" =>
		    data_out <= ram_reg38;
	     when "111001" =>
		    data_out <= ram_reg39;
	     when "111010" =>
		    data_out <= ram_reg310;
	     when "111011" =>
		    data_out <= ram_reg311;
	     when "111100" =>
		    data_out <= ram_reg312;
	     when "111101" =>
		    data_out <= ram_reg313;
	     when "111110" =>
		    data_out <= ram_reg314;
	     when "111111" =>
		    data_out <= ram_reg315;
        when others =>
		    null;
		end case;
end process;

end;

