library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.MATH_REAL.ALL;

entity uart_rx is
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
end entity uart_rx;

architecture behavior of uart_rx is 
    constant    OS_RATE_WIDTH   : integer := integer(ceil(log2(real(OS_RATE))));
    constant    OS_TICKS        : integer := integer(round(real(real(CLK_FREQ) / real(BAUD_RATE * OS_RATE))));
    constant    N_WIDTH         : integer := integer(ceil(log2(real(N))));

    -- State encoding
    constant    START_BIT       : std_logic_vector (1 downto 0) := "00";
    constant    RX_BITS         : std_logic_vector (1 downto 0) := "01";
    constant    PARITY_BIT      : std_logic_vector (1 downto 0) := "11";
    constant    RECEIVE         : std_logic_vector (1 downto 0) := "10";

    signal      rx_stage        : std_logic_vector (1 downto 0);
    signal      rx_stage_c      : std_logic_vector (1 downto 0);
    signal      os_count        : std_logic_vector (OS_RATE_WIDTH - 1 downto 0);
    signal      os_count_c      : std_logic_vector (OS_RATE_WIDTH - 1 downto 0);
    signal      rx_busy_o       : std_logic;
    signal      rx_busy_c       : std_logic;
    signal      rx_data_o       : std_logic_vector (N - 1 downto 0);
    signal      rx_data_c       : std_logic_vector (N - 1 downto 0);
    signal      rx_err_o        : std_logic;
    signal      rx_err_c        : std_logic;
    signal      shift_cnt       : std_logic_vector (N_WIDTH - 1 downto 0);
    signal      shift_cnt_c     : std_logic_vector (N_WIDTH - 1 downto 0);
    signal      shift_reg       : std_logic_vector (N - 1 downto 0);
    signal      shift_reg_c     : std_logic_vector (N - 1 downto 0);
    signal      sync_rx_1       : std_logic;
    signal      sync_rx_2       : std_logic;
    signal      os_pulse        : std_logic;
    signal      parity_err      : std_logic;
    signal      parity_err_c    : std_logic;

    component mod_N_counter is
        generic (
            N       : integer
        );
        port (
            clk     : in    std_logic;  
            reset_n : in    std_logic;
            cnt_en  : in    std_logic;
            pulse   : out   std_logic
        );
    end component mod_N_counter;

    -- For parity computations
    function xor_reduce(slv : in std_logic_vector) return std_logic is
        variable res_v : std_logic := '1';
    begin
        for i in slv'range loop
            res_v := res_v xor slv(i);
        end loop;
        return res_v;
    end function;

    function xnor_reduce(slv : in std_logic_vector) return std_logic is
        variable res_v : std_logic := '1';
    begin
        for i in slv'range loop
            res_v := res_v xnor slv(i);
        end loop;
        return res_v;
    end function;

begin

    -- Generates a pulse every oversampling period. If BAUD_RATE = 19_200 and OS_RATE = 16,
    -- a pulse will be generated at a rate of 19_200 * 16 = 307_200 Hz.
    os_pulse_gen : mod_N_counter
    generic map (
        N       => OS_TICKS
    )
    port map (
        clk     => clk,
        reset_n => reset_n,
        cnt_en  => '1',
        pulse   => os_pulse
    );
    
    -- There is no guarantee that RX is not metastable, so a 2-FF synchronizer is used.
    synchronize_rx : process (
        clk,
        reset_n
    )
    begin
        if reset_n = '0' then
            sync_rx_1   <= '0';
            sync_rx_2   <= '0';
        elsif rising_edge(clk) then
            sync_rx_1   <= rx;
            sync_rx_2   <= sync_rx_1;
        end if;
    end process synchronize_rx;

    dff : process (
        clk, 
        reset_n
    )
    begin
        if reset_n = '0' then
            rx_stage    <= START_BIT;
            os_count    <= (others => '0');
            rx_busy_o   <= '0';
            rx_data_o   <= (others => '0');
            rx_err_o    <= '0';
            shift_cnt   <= (others => '0');
            shift_reg   <= (others => '0');
            parity_err  <= '0';
        elsif rising_edge(clk) then
            rx_stage    <= rx_stage_c;
            os_count    <= os_count_c;
            rx_busy_o   <= rx_busy_c;
            rx_data_o   <= rx_data_c;
            rx_err_o    <= rx_err_c;
            shift_cnt   <= shift_cnt_c;
            shift_reg   <= shift_reg_c;
            parity_err  <= parity_err_c;
        end if;
    end process dff;

    rx_data <= rx_data_o;
    rx_busy <= rx_busy_o;
    rx_err  <= rx_err_o;

    NO_PARITY : if HAS_PARITY = 0 generate
        rx_fsm : process (
            rx_stage,
            os_count,
            rx_busy_o,
            rx_err_o,
            rx_data_o,
            shift_cnt,
            shift_reg,
            sync_rx_2,
            os_pulse
        )
        begin
            rx_stage_c      <= rx_stage;
            os_count_c      <= os_count;
            rx_busy_c       <= '0';
            rx_data_c       <= rx_data_o;
            rx_err_c        <= rx_err_o;
            shift_cnt_c     <= shift_cnt;
            shift_reg_c     <= shift_reg;
            case (rx_stage) is
                when START_BIT =>
                    if os_pulse = '1' then

                        -- Sample the start bit at its midpoint (occurring at OS_RATE / 2 pulses). Start bit should be '0'.
                        if sync_rx_2 = '0' then
                            if unsigned(os_count) = to_unsigned(OS_RATE / 2 - 1, os_count'LENGTH) then
                                os_count_c  <= (others => '0');
                                rx_stage_c  <= RX_BITS;
                                rx_busy_c   <= '1';
                            else
                                os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                            end if;
                        else
                            os_count_c  <= (others => '0');
                        end if;
                    end if;
                when RX_BITS =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then

                        -- Sample the RX message bits at midpoints and shift them into the shift register.
                        if unsigned(os_count) = to_unsigned(OS_RATE - 1, os_count'LENGTH) then
                            os_count_c  <= (others => '0');
                            for i in 0 to N - 2 loop
                                shift_reg_c(i)  <= shift_reg(i + 1);
                            end loop;
                            shift_reg_c(N - 1) <= sync_rx_2;

                            -- Once all N message bits have been received, proceed to detecting the parity or stop bit.
                            if unsigned(shift_cnt) = to_unsigned(N - 1, shift_cnt'LENGTH) then
                                rx_stage_c  <= RECEIVE;
                                shift_cnt_c <= (others => '0');
                            else
                                shift_cnt_c <= std_logic_vector(unsigned(shift_cnt) + to_unsigned(1, shift_cnt'LENGTH));
                            end if; 
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                    
                -- No parity stage HW generated because this case does not entail parity.
                when RECEIVE =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned((NUM_STOP_BITS * OS_RATE) - 1, os_count'LENGTH) then
                            rx_stage_c  <= START_BIT;
                            if sync_rx_2 = '1' then
                                rx_data_c   <= shift_reg;
                                rx_err_c    <= '0';
                            else
                                rx_err_c    <= '1';
                            end if;
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when others =>
                    rx_stage_c <= START_BIT;
            end case;
        end process rx_fsm;
    end generate NO_PARITY;

    EVEN_PARITY : if HAS_PARITY = 1 and PARITY_EVEN_ODD = 0 generate
        rx_fsm : process (
            rx_stage,
            os_count,
            rx_busy_o,
            rx_err_o,
            rx_data_o,
            shift_cnt,
            shift_reg,
            sync_rx_2,
            os_pulse,
            parity_err
        )
        begin
            rx_stage_c      <= rx_stage;
            os_count_c      <= os_count;
            rx_busy_c       <= '0';
            rx_data_c       <= rx_data_o;
            rx_err_c        <= rx_err_o;
            shift_cnt_c     <= shift_cnt;
            shift_reg_c     <= shift_reg;
            parity_err_c    <= parity_err;
            case (rx_stage) is
                when START_BIT =>
                    if os_pulse = '1' then
                        if sync_rx_2 = '0' then
                            if unsigned(os_count) = to_unsigned(OS_RATE / 2 - 1, os_count'LENGTH) then
                                os_count_c  <= (others => '0');
                                rx_stage_c  <= RX_BITS;
                                rx_busy_c   <= '1';
                            else
                                os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                            end if;
                        else
                            os_count_c  <= (others => '0');
                        end if;
                    end if;
                when RX_BITS =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned(OS_RATE - 1, os_count'LENGTH) then
                            os_count_c  <= (others => '0');
                            for i in 0 to N - 2 loop
                                shift_reg_c(i)  <= shift_reg(i + 1);
                            end loop;
                            shift_reg_c(N - 1) <= sync_rx_2;
                            if unsigned(shift_cnt) = to_unsigned(N - 1, shift_cnt'LENGTH) then
                                rx_stage_c  <= PARITY_BIT;
                                shift_cnt_c <= (others => '0');
                            else
                                shift_cnt_c <= std_logic_vector(unsigned(shift_cnt) + to_unsigned(1, shift_cnt'LENGTH));
                            end if; 
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when PARITY_BIT =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned(OS_RATE - 1, os_count'LENGTH) then
                            rx_stage_c <= RECEIVE;
                            if xor_reduce(shift_reg) = sync_rx_2 then
                                parity_err_c <= '0';
                            else
                                parity_err_c <= '1';
                            end if;
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when RECEIVE =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned((NUM_STOP_BITS * OS_RATE) - 1, os_count'LENGTH) then
                            rx_stage_c  <= START_BIT;
                            if sync_rx_2 = '1' and parity_err = '0' then
                                rx_data_c   <= shift_reg;
                                rx_err_c    <= '0';
                            else
                                rx_err_c    <= '1';
                            end if;
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when others =>
                    rx_stage_c <= START_BIT;
            end case;
        end process rx_fsm;
    end generate EVEN_PARITY;

    ODD_PARITY : if HAS_PARITY = 1 and PARITY_EVEN_ODD = 1 generate
        rx_fsm : process (
            rx_stage,
            os_count,
            rx_busy_o,
            rx_err_o,
            rx_data_o,
            shift_cnt,
            shift_reg,
            sync_rx_2,
            os_pulse,
            parity_err
        )
        begin
            rx_stage_c      <= rx_stage;
            os_count_c      <= os_count;
            rx_busy_c       <= '0';
            rx_data_c       <= rx_data_o;
            rx_err_c        <= rx_err_o;
            shift_cnt_c     <= shift_cnt;
            shift_reg_c     <= shift_reg;
            parity_err_c    <= parity_err;
            case (rx_stage) is
                when START_BIT =>
                    if os_pulse = '1' then
                        if sync_rx_2 = '0' then
                            if unsigned(os_count) = to_unsigned(OS_RATE / 2 - 1, os_count'LENGTH) then
                                os_count_c  <= (others => '0');
                                rx_stage_c  <= RX_BITS;
                                rx_busy_c   <= '1';
                            else
                                os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                            end if;
                        else
                            os_count_c  <= (others => '0');
                        end if;
                    end if;
                when RX_BITS =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned(OS_RATE - 1, os_count'LENGTH) then
                            os_count_c  <= (others => '0');
                            for i in 0 to N - 2 loop
                                shift_reg_c(i)  <= shift_reg(i + 1);
                            end loop;
                            shift_reg_c(N - 1) <= sync_rx_2;
                            if unsigned(shift_cnt) = to_unsigned(N - 1, shift_cnt'LENGTH) then
                                rx_stage_c  <= PARITY_BIT;
                                shift_cnt_c <= (others => '0');
                            else
                                shift_cnt_c <= std_logic_vector(unsigned(shift_cnt) + to_unsigned(1, shift_cnt'LENGTH));
                            end if; 
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when PARITY_BIT =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned(OS_RATE - 1, os_count'LENGTH) then
                            rx_stage_c <= RECEIVE;
                            if xnor_reduce(shift_reg) = sync_rx_2 then
                                parity_err_c <= '0';
                            else
                                parity_err_c <= '1';
                            end if;
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when RECEIVE =>
                    rx_busy_c   <= '1';
                    if os_pulse = '1' then
                        if unsigned(os_count) = to_unsigned((NUM_STOP_BITS * OS_RATE) - 1, os_count'LENGTH) then
                            rx_stage_c  <= START_BIT;
                            if sync_rx_2 = '1' and parity_err = '0' then
                                rx_data_c   <= shift_reg;
                                rx_err_c    <= '0';
                            else
                                rx_err_c    <= '1';
                            end if;
                        else
                            os_count_c  <= std_logic_vector(unsigned(os_count) + to_unsigned(1, os_count'LENGTH));
                        end if;
                    end if;
                when others =>
                    rx_stage_c <= START_BIT;
            end case;
        end process rx_fsm;
    end generate ODD_PARITY;
end architecture behavior;