library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.MATH_REAL.ALL;

entity mod_N_counter is
generic (
    N       : integer
);
port (
    clk     : in    std_logic;  
    reset_n : in    std_logic;
    cnt_en  : in    std_logic;
    pulse   : out   std_logic
);
end entity mod_N_counter;

architecture behavior of mod_N_counter is 
    constant    N_WIDTH         : integer       := integer(ceil(log2(real(N))));
    signal      counter         : std_logic_vector (N_WIDTH - 1 downto 0);
    signal      pulse_o         : std_logic;
begin

    STANDARD_COUNTER : if N > 1 generate
        counter_process : process (
            clk,
            reset_n
        )
        begin
            if reset_n = '0' then
                counter <= (others => '0');
            elsif rising_edge(clk) then
                if cnt_en = '1' then
                    if unsigned(counter) = to_unsigned(N - 1, counter'LENGTH) then
                        counter <= (others => '0');
                    else
                        counter <= std_logic_vector(unsigned(counter) + to_unsigned(1, counter'LENGTH));
                    end if;
                end if;
            end if;
        end process counter_process;

        pulse_process : process (
            clk,
            reset_n
        )
        begin
            if reset_n = '0' then
                pulse <= '0';
            elsif rising_edge(clk) then
                if cnt_en = '1' and unsigned(counter) = to_unsigned(N - 1, counter'LENGTH) then
                    pulse <= '1';
                else 
                    pulse <= '0';
                end if;
            end if;
        end process pulse_process;
    end generate STANDARD_COUNTER;

    EMPTY_COUNTER : if N <= 1 generate
        pulse <= cnt_en;
    end generate EMPTY_COUNTER;
        
end architecture behavior;