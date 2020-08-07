library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity bin2bcd is
port (
    i_bin       :   in std_logic_vector (3 downto 0);
    o_bcd       :   out std_logic_vector (6 downto 0)
);
end entity bin2bcd;

architecture behavior of bin2bcd is
begin
    process (i_bin)
        begin
            case (i_bin) is 
                when x"0"   =>  o_bcd <= "1000000";
                when x"1"   =>  o_bcd <= "1111001";
                when x"2"   =>  o_bcd <= "0100100";
                when x"3"   =>  o_bcd <= "0110000";
                when x"4"   =>  o_bcd <= "0011001";
                when x"5"   =>  o_bcd <= "0010010";
                when x"6"   =>  o_bcd <= "0000010";
                when x"7"   =>  o_bcd <= "1111000";
                when x"8"   =>  o_bcd <= "0000000";
                when x"9"   =>  o_bcd <= "0010000";
                when x"A"   =>  o_bcd <= "0001000"; 
                when x"B"   =>  o_bcd <= "0000011"; 
                when x"C"   =>  o_bcd <= "1000110"; 
                when x"D"   =>  o_bcd <= "0100001"; 
                when x"E"   =>  o_bcd <= "0000110"; 
                when x"F"   =>  o_bcd <= "0001110"; 
            end case;
    end process;
end architecture behavior;