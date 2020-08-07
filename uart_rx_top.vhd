library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity uart_rx_top is
port (
    i_clk_50        : in std_logic;
    i_button        : in std_logic;
    i_gpio          : in std_logic;
    o_seg7_u          : out std_logic_vector (6 downto 0);
    o_seg7_l          : out std_logic_vector (6 downto 0);
    o_led_1            : out std_logic;
    o_led_2            : out std_logic
);
end entity uart_rx_top;

architecture structural of uart_rx_top is
    
    component uart_rx is
        generic (
            CLK_FREQ        : integer   := 50_000_000;      -- Hz
            BAUD_RATE       : integer   := 19_200;
            OS_RATE         : integer   := 16;              -- Oversample rate per baud period
            N               : integer   := 8;               -- Bits per transmission (typically 8)
            HAS_PARITY      : integer   := 0;           
            PARITY_EVEN_ODD : integer   := 0;               -- 0: Even. 1: Odd.
            NUM_STOP_BITS   : integer   := 1
        );
        port (
            clk     : in    std_logic;  
            reset_n : in    std_logic;
            rx      : in    std_logic;
            rx_data : out   std_logic_vector (N - 1 downto 0);
            rx_busy : out   std_logic;
            rx_err  : out   std_logic
        );
    end component uart_rx;

    component bin2bcd is
        port (
            i_bin       :  in std_logic_vector (3 downto 0);
            o_bcd       :  out std_logic_vector (6 downto 0)
        );
    end component bin2bcd;

    signal i_button_n : std_logic;
    signal rx_data  : std_logic_vector (7 downto 0);
    signal rx_data_u, rx_data_l : std_logic_vector (3 downto 0);

    begin

    i_button_n  <= i_button;
    rx_data_u   <= rx_data(7 downto 4);
    rx_data_l    <= rx_data(3 downto 0);

    uart_rx_inst : uart_rx
    port map (
        clk     => i_clk_50,
        reset_n => i_button_n,
        rx      => i_gpio,
        rx_data => rx_data,
        rx_busy => o_led_1,
        rx_err  => o_led_2
    );

    bin2bcd1    : bin2bcd
    port map (
        i_bin   => rx_data_u,
        o_bcd   => o_seg7_u 
    );

    bin2bcd2    : bin2bcd
    port map (
        i_bin   => rx_data_l,
        o_bcd   => o_seg7_l
    );

end architecture structural;