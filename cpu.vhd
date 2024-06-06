-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Marek Effenberger <xeffen00@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

--- PROGRAM COUNTER REGISTER
  signal program_counter_data : std_logic_vector(12 downto 0) := (others => '0');
  signal program_counter_inc  : std_logic;
  signal program_counter_dec  : std_logic;

--- POINTER REGISTER
  signal pointer_data         : std_logic_vector(12 downto 0) := (others => '0');
  signal pointer_increase     : std_logic;
  signal pointer_decrease     : std_logic;

--- PARENTHESES COUNTER REGISTER
  signal parentheses_data     : std_logic_vector(12 downto 0) := (others => '0');
  signal parentheses_increase : std_logic;
  signal parentheses_decrease : std_logic;
  signal parentheses_set      : std_logic;

--- MULTIPLEXOR 1
--  signal mx1_output           : std_logic_vector(12 downto 0);
  signal mx1_select           : std_logic;
 
--- MULTIPLEXOR 2
  signal mx2_select           : std_logic_vector(1 downto 0) := (others => '0'); 

--- FSM
type fsm_state is (

    s_idle,
    s_init,
    s_init_the_init,
    s_fetch,
    s_decode,

    s_pointer_inc,
    s_pointer_dec,
    
    s_ic_data,
    s_inc_inc_data,
    s_inc_data,

    s_dc_data,
    s_dec_dec_data,
    s_dec_data,

    s_while_cycle,
    s_while_while_cycle,
    s_while_while_while_cycle,
    s_while_while_while_while_cycle,

    s_end_of_while,
    s_end_of_while_while,
    s_end_of_while_while_while,
    s_end_of_while_while_while_while,

    s_break,
    s_break_break,
    s_break_break_break,

    s_write_write,
    s_write,

    s_get_get,
    s_get,

    s_halt,
    s_others
    

);

signal fsm_pstate : fsm_state := s_idle; 
signal fsm_nstate : fsm_state;



begin

-- DEKLARACE KOMPONENT

pc_reg : process (RESET, CLK)
  begin
    if (RESET = '1') then
      program_counter_data <= (others => '0');
    elsif rising_edge(CLK) then
      if (program_counter_inc = '1') then
        program_counter_data <= program_counter_data + 1;
      elsif (program_counter_dec = '1') then
        program_counter_data <= program_counter_data - 1;
      end if;
    end if;
end process;

ptr_reg : process (RESET, CLK)
  begin
    if (RESET = '1') then
      pointer_data <= (others => '0');
    elsif rising_edge(CLK) then
      if (pointer_increase = '1') then
        pointer_data <= pointer_data + 1;
      elsif (pointer_decrease = '1') then
        pointer_data <= pointer_data - 1;
      end if;
    end if;
end process;

par_reg : process (RESET, CLK)
  begin
    if (RESET = '1') then
      parentheses_data <= (others => '0');
    elsif rising_edge(CLK) then
      if (parentheses_increase = '1') then
        parentheses_data <= parentheses_data + 1;
      elsif (parentheses_decrease = '1') then
        parentheses_data <= parentheses_data - 1;
      elsif (parentheses_set = '1') then          --POMOCNY SIGNAL PRO NASTAVENI HODNOTY CNT NA 1
        parentheses_data <= "0000000000001";
      end if;
    end if;
end process;

mx1 : process ( pointer_data, program_counter_data, mx1_select)
  begin
    case mx1_select is
      when '0' => DATA_ADDR <= pointer_data;
      when '1' => DATA_ADDR <= program_counter_data;
      when others => DATA_ADDR <= (others => '0');
    end case;
end process;

mx2 : process ( IN_DATA, DATA_RDATA, mx2_select)
  begin
    case mx2_select is
      when "00" => DATA_WDATA <= IN_DATA;
      when "01" => DATA_WDATA <= DATA_RDATA - 1;
      when "10" => DATA_WDATA <= DATA_RDATA + 1;
      when others => DATA_WDATA <= (others => '0');
    end case;
end process;

fsm_present_state: process (CLK, RESET, EN)
      begin
        if RESET = '1' then
            fsm_pstate <= s_idle;      
        elsif rising_edge(CLK) and EN = '1' 
            then 
              fsm_pstate <= fsm_nstate;
        end if ;
end process;

fsm: process (fsm_pstate, DATA_RDATA, IN_VLD, OUT_BUSY, IN_DATA) is
  begin
      program_counter_inc      <= '0';
      program_counter_dec      <= '0';
      pointer_increase         <= '0';
      pointer_decrease         <= '0';
      parentheses_increase     <= '0';
      parentheses_decrease     <= '0';
      parentheses_set          <= '0';
      
      DATA_EN                  <= '0';
      IN_REQ                   <= '0';
      OUT_WE                   <= '0';
      DATA_RDWR                <= '0';
      DONE                     <= '0';

      mx1_select               <= '0';
      mx2_select               <= "00";

      --OUT_DATA          <= DATA_RDATA;


      
      case fsm_pstate is

        when s_idle =>
          READY <= '0';
          fsm_nstate <= s_init_the_init;

------------------------------------------------------------------------------------------------------
-- INICIALIZACE, POSUN POINTERU ZA ZNAK @
        when s_init_the_init =>

          DATA_EN       <= '1';
          DATA_RDWR     <= '0';
          mx1_select    <= '0';

          if DATA_RDATA = X"40" then
            fsm_nstate  <= s_init;
          else 
            pointer_increase  <= '1';
          end if;

        when s_init =>
          READY         <= '1';
          DATA_EN       <= '1';
          DATA_RDWR     <= '0';
          fsm_nstate <= s_fetch;

------------------------------------------------------------------------------------------------------
-- FETCH A DECODE, FETCH NUTNY PRO SPRAVNE NASTAVENI KOMPONENT, DECODE PRO ROZPOZNANI INSTRUKCE
        when s_fetch =>
          mx1_select    <= '1';
          DATA_RDWR     <= '0';
          DATA_EN       <= '1';
          fsm_nstate <= s_decode;

        when s_decode =>  
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        mx1_select <= '1';
            case DATA_RDATA is
              when X"3E" => fsm_nstate <= s_pointer_inc;
              when X"3C" => fsm_nstate <= s_pointer_dec;
              when X"2B" => fsm_nstate <= s_inc_inc_data;
              when X"2D" => fsm_nstate <= s_dec_dec_data;
              when X"5B" => fsm_nstate <= s_while_while_while_while_cycle;
              when X"5D" => fsm_nstate <= s_end_of_while_while_while_while;
              when X"7E" => fsm_nstate <= s_break_break_break;
              when X"2E" => fsm_nstate <= s_write_write;
              when X"2C" => fsm_nstate <= s_get_get;
              when X"40" => fsm_nstate <= s_halt;
              when others => fsm_nstate <= s_others;	
            end case;

------------------------------------------------------------------------------------------------------
--- INKREMENTACE, DEKREMENTACE POINTERU

        when s_pointer_inc =>
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        mx1_select <= '0';
        pointer_increase <= '1';
        program_counter_inc <= '1';
        fsm_nstate <= s_fetch;
        
        when s_pointer_dec =>
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        mx1_select <= '0';
        pointer_decrease <= '1';
        program_counter_inc <= '1';
        fsm_nstate <= s_fetch;

------------------------------------------------------------------------------------------------------
--- INKREMENTACE, DEKREMENTACE BUNKY NA KTEROU UKAZUJE POINTER
        
        when s_inc_inc_data =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_nstate <= s_inc_data;

        when s_inc_data =>
        DATA_EN <= '1';
        mx2_select <= "10";
        mx1_select <= '0';
        program_counter_inc <= '1';
        fsm_nstate <= s_ic_data;

        when s_ic_data =>
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        mx2_select <= "10";
        mx1_select <= '0';
        fsm_nstate <= s_fetch;

------------------------------------------------------------------------------------------------------

        when s_dec_dec_data =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_nstate <= s_dec_data;

        when s_dec_data =>
        DATA_EN <= '1';
        mx2_select <= "01";
        mx1_select <= '0';
        program_counter_inc <= '1';
        fsm_nstate <= s_dc_data;
            
        when s_dc_data =>
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        mx2_select <= "01";
        mx1_select <= '0';
        fsm_nstate <= s_fetch;
------------------------------------------------------------------------------------------------------
--- WHILE, NUTNO ROZDELIT DO 4 STAVU KVULI MOZNEMU NESTINGU A SPRAVNEMU NASTAVENI KOMPONENT
     
        when s_while_while_while_while_cycle =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        program_counter_inc <= '1';
        fsm_nstate <= s_while_while_while_cycle;

        when s_while_while_while_cycle =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '0';

        if DATA_RDATA = X"00" then
          parentheses_set <= '1';         ------------------------------------------ set na 1
          fsm_nstate <= s_while_while_cycle;
        else
          fsm_nstate <= s_fetch;
        end if;

        when s_while_while_cycle =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';
        if parentheses_data = X"00" then
          fsm_nstate <= s_fetch;
        else
          fsm_nstate <= s_while_cycle;
        end if;

        when s_while_cycle =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';

        if DATA_RDATA = X"5B" then
          parentheses_increase <= '1';
        elsif DATA_RDATA = X"5D" then
          parentheses_decrease <= '1';
        end if;
        program_counter_inc <= '1';
        fsm_nstate <= s_while_while_cycle;
        
------------------------------------------------------------------------------------------------------
        
        when s_end_of_while_while_while_while =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_nstate <= s_end_of_while_while_while;

        when s_end_of_while_while_while =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '0';	
        if DATA_RDATA = X"00" then
          program_counter_inc <= '1';
          fsm_nstate <= s_fetch;
        else
          parentheses_set <= '1';
          program_counter_dec <= '1';
          fsm_nstate <= s_end_of_while_while;
        end if;


        when s_end_of_while_while =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';

        if parentheses_data = X"00" then
          fsm_nstate <= s_fetch;
          program_counter_inc <= '1';
        else
          fsm_nstate <= s_end_of_while;
        end if;


        when s_end_of_while =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';        
        
        if DATA_RDATA = X"5B" then
          parentheses_decrease <= '1';
        elsif DATA_RDATA = X"5D" then
          parentheses_increase <= '1';
        end if;
          program_counter_dec <= '1';
        
        fsm_nstate <= s_end_of_while_while;
        
        

------------------------------------------------------------------------------------------------------
-- VYUZITI CNT REGISTRU, KVULI MOZNE SITUACI VE STYLU [~[[[]]]] XYZ] MUSI SE SKOCIT ZA ODPOVIDAJICI ZAVORKU, LOGIKA PODOBNA WHILE

        when s_break_break_break =>
        mx1_select <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        program_counter_inc <= '1';
        parentheses_set <= '1';
        fsm_nstate <= s_break_break;

        when s_break_break =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';
        if parentheses_data = X"00" then
          fsm_nstate <= s_fetch;
        else
          fsm_nstate <= s_break;
        end if;

        when s_break =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mx1_select <= '1';
        if DATA_RDATA = X"5B" then
          parentheses_increase <= '1';
        elsif DATA_RDATA = X"5D" then
          parentheses_decrease <= '1';
        end if;
        program_counter_inc <= '1';
        fsm_nstate <= s_break_break;

------------------------------------------------------------------------------------------------------
-- ZAPIS DAT NA ZAKLADE OUT_BUSY SIGNALU
        
        when s_write_write =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_nstate <= s_write;

        when s_write =>
        if OUT_BUSY = '0' then
          OUT_DATA <= DATA_RDATA;
          mx2_select <= "00";
          mx1_select <= '0';
          OUT_WE <= '1';
          program_counter_inc <= '1';
          fsm_nstate <= s_fetch;
        elsif OUT_BUSY = '1' then
          DATA_EN <= '0';
          DATA_RDWR <= '0';
          mx1_select <= '0';
          fsm_nstate <= s_write;
        end if;
        

------------------------------------------------------------------------------------------------------
-- NACTENI DAT Z KLAVESNICE

        when s_get_get =>
        mx1_select <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_nstate <= s_get;

        when s_get =>
        IN_REQ <= '1';
        if IN_VLD = '1' then
          DATA_EN <= '1';
          DATA_RDWR <= '1';
          mx1_select <= '0';
          program_counter_inc <= '1';
          fsm_nstate <= s_fetch;

        elsif IN_VLD = '0' then
          DATA_EN <= '0';
          DATA_RDWR <= '0';
          mx1_select <= '0';
          fsm_nstate <= s_get;
        end if;

------------------------------------------------------------------------------------------------------
-- OTHERS NUTNY PRO SPRACOVANI KOMENTARU, EOF JE HALTING STAV

        when s_others =>
        program_counter_inc <= '1';
        fsm_nstate <= s_fetch;
        
        when s_halt =>
        DONE <= '1';
        fsm_nstate <= s_halt;

        end case;
      end process fsm;



end behavioral;
      



        

