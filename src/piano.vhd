--
-- piano.vhd - FPGA Piano
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
library UNISIM;
use UNISIM.VComponents.all;

entity piano is
    port ( CLK_IN       : in std_logic;
           pb_in        : in std_logic_vector(3 downto 0);
           switch_in    : in std_logic_vector(7 downto 0);
           SPK_N        : out std_logic; 
           SPK_P        : out std_logic;
           led_out      : out std_logic_vector(7 downto 0);
           digit_out    : out std_logic_vector(3 downto 0);
           seg_out      : out std_logic_vector(7 downto 0)
         );
end piano;

architecture Behavioral of piano is
   -- Xilinx Native Components
   component BUFG  port ( I : in std_logic; O : out std_logic); end component;
   component IBUFG port ( I : in std_logic; O : out std_logic); end component;
   component IBUF  port ( I : in std_logic; O : out std_logic); end component;
   component OBUF  port ( I : in std_logic; O : out std_logic); end component;
   component MMCME2_BASE
      generic( CLKFBOUT_MULT_F : real;
                DIVCLK_DIVIDE :  integer;
                CLKOUT0_DIVIDE_F  :  real
              );
      port ( CLKIN1     : in    std_logic; 
             CLKFBIN    : in    std_logic; 
             RST        : in    std_logic; 
             PWRDWN     : in    std_logic; 
             CLKOUT0    : out   std_logic; 
             CLKOUT0B   : out   std_logic;
             CLKOUT1    : out   std_logic;
             CLKOUT1B   : out   std_logic;
             CLKOUT2    : out   std_logic;
             CLKOUT2B   : out   std_logic;
             CLKOUT3    : out   std_logic;
             CLKOUT3B   : out   std_logic;
             CLKOUT4    : out   std_logic;
             CLKOUT5    : out   std_logic;
             CLKOUT6    : out   std_logic;
             CLKFBOUT   : out   std_logic; 
             CLKFBOUTB  : out   std_logic; 
             LOCKED     : out   std_logic);
   end component;

    -- My Components:

    --  Clock Divider
    component clk_dvd
    port (
          CLK     : in std_logic;
          RST     : in std_logic;
          DIV     : in std_logic_vector(15 downto 0);
          EN      : in std_logic;
          CLK_OUT : out std_logic;
          ONE_SHOT: out std_logic
         );
    end component;

    -- Note decoder
    component note_gen
    port (
          CLK       : in  std_logic;
          RST       : in  std_logic;
          NOTE_IN   : in  std_logic_vector(4 downto 0);
          DIV       : out std_logic_vector(15 downto 0)
         );
    end component;
    
    -- 7-Segment Display for Notes
    component seven_seg
        port ( CLK      : in std_logic;
               RST      : in std_logic;
               NOTE_IN  : in std_logic_vector(4 downto 0);
               SCAN_EN  : in std_logic; 
               DIGIT    : out std_logic_vector(3 downto 0);
               SEG      : out std_logic_vector(7 downto 0) 
             );
   end component;

   -- Custom-defined Signals
   signal PLAYER_MODE : std_logic := '0';      -- Will be set to 1 when player piano mode is enabled. By default, we are not in player piano mode
                                               -- This will disable manual playing until the song finishes.
                                               -- The last switch, the one unused by the manual-playing code, will be used to enable PLAYER_MODE.

   -- Signals
   signal CLK         : std_logic;                      -- 50MHz clock after DCM and BUFG
   signal CLK0        : std_logic;                      -- 50MHz clock from pad
   signal CLK_BUF     : std_logic;                      -- 50MHz clock after IBUF
   signal GND         : std_logic;                      
   signal RST         : std_logic;              
   signal PB          : std_logic_vector(3 downto 0);   -- Pushbuttons after ibufs
   signal digit_l     : std_logic_vector(3 downto 0);   -- 7-seg digit MUX before obuf
   signal switch      : std_logic_vector(7 downto 0);   -- Toggle switches after ibufs
   signal led         : std_logic_vector(7 downto 0);   -- LEDs after ibufs
   signal seg_l       : std_logic_vector(7 downto 0);   -- 7-seg segment select before obuf.
  
   signal one_mhz     : std_logic;                      -- 1MHz Clock
   signal one_mhz_1   : std_logic;                      -- pulse with f=1 MHz created by divider
   signal clk_10k_1   : std_logic;                      -- pulse with f=10kHz created by divider
   signal div         : std_logic_vector(15 downto 0);  -- variable clock divider for loadable counter
   signal note_in     : std_logic_vector(4 downto 0);   -- output of user interface. Current Note
   signal note_next   : std_logic_vector(4 downto 0);   -- Buffer holding current Note
   signal note_sel    : std_logic_vector(3 downto 0);   -- Encoding of switches.
   signal div_1       : std_logic;                      -- 1MHz pulse
   signal sound       : std_logic;                      -- Output of Loadable Clock Divider. Sent to Speaker if note is playing.
   signal SPK         : std_logic;                      -- Output for Speaker fed to OBUF
   
begin
    GND    <= '0';     
    RST    <= PB(0);    -- push button one is the reset
    led(1) <= RST;      -- This is just to make sure our design is running.
    
    -- Combinational logic to turn the sound on and off
    process (div, sound) begin
        if (div = x"0000") then
            SPK <= GND;
        else
            SPK <= sound;
        end if;
    end process;
    
    -- Speaker output
    SPK_OBUF_INST : OBUF port map (I=>SPK, O=>SPK_N);
    SPK_P <= GND; 

    -- Input/Output Buffers
    loop0 : for i in 0 to 3 generate
        pb_ibuf  : IBUF  port map(I => pb_in(i),   O => PB(i));
        dig_obuf : OBUF  port map(I => digit_l(i), O => digit_out(i));
    end generate ;
    loop1 : for i in 0 to 7 generate
        swt_obuf : IBUF  port map(I => switch_in(i), O => switch(i));
        led_obuf : OBUF  port map(I => led(i),   O => led_out(i));
        seg_obuf : OBUF  port map(I => seg_l(i), O => seg_out(i));
    end generate ;

    -- Global Clock Buffers

    -- Pad -> DCM
    CLKIN_IBUFG_INST : IBUFG
      port map (I=>CLK_IN,      
                O=>CLK0);

    -- DCM -> CLK
    CLK0_BUFG_INST : BUFG
      port map (I=>CLK_BUF,      
                O=>CLK);

   
    -- MMCM for Clock deskew and frequency synthesis
    MMCM_INST : MMCME2_BASE
      generic map(
        CLKFBOUT_MULT_F =>10.0,
        DIVCLK_DIVIDE=>1,
        CLKOUT0_DIVIDE_F =>10.0
      )
      port map (CLKIN1=>CLK0,
               CLKFBIN=>CLK, 
               RST=>RST, 
               PWRDWN=>GND, 
               CLKOUT0=>CLK_BUF,
               CLKOUT0B=>open,
               CLKOUT1=>open,
               CLKOUT1B=>open,
               CLKOUT2=>open,
               CLKOUT2B=>open,
               CLKOUT3=>open,
               CLKOUT3B=>open,
               CLKOUT4=>open,
               CLKOUT5=>open,
               CLKOUT6=>open,
               CLKFBOUT=>open, 
               CLKFBOUTB=>open, 
               LOCKED=>led(0)
               );

    -- Divide 100Mhz to 1Mhz clock
    DIV_1M : clk_dvd        
        port map ( CLK      => CLK,
                   RST      => RST,
                   DIV      => x"0032",  -- 50
                   EN       => '1',
                   CLK_OUT  => one_mhz,
                   ONE_SHOT => one_mhz_1
                 );

    -- Divide 1Mhz to Various frequencies for the notes.
    DIV_NOTE : clk_dvd        
        port map ( CLK      => CLK,
                   RST      => RST,
                   DIV      => div,
                   EN       => one_mhz_1,
                   CLK_OUT  => sound,
                   ONE_SHOT => div_1
                 );

    -- Divide 1Mhz to 10k
    DIV_10k : clk_dvd        
        port map ( CLK      => CLK,
                   RST      => RST,
                   DIV      => x"0032", -- 50
                   EN       => one_mhz_1,
                   CLK_OUT  => open,
                   ONE_SHOT => clk_10k_1
                 );

    -- Translate Encoded Note to clock divider for 1MHz clock.
    note_gen_inst : note_gen
        port map ( CLK     => CLK,
                   RST     => RST,
                   NOTE_IN => note_in,
                   DIV     => div
                 );

    -- Wire up seven-seg controller to display current note.
    seven_seg_inst : seven_seg
        port map ( CLK     => CLK,
                   RST     => RST,
                   NOTE_IN => note_in,
                   SCAN_EN => clk_10k_1,
                   DIGIT   => digit_l,
                   SEG     => seg_l
                 );

    -- User Interface
    note_in <= note_next;
    
    led(2) <= PLAYER_MODE;  -- The LED at index 2 is the player piano mode indicator (LD2 on the board)
    
    process (CLK,RST)
        -- The song is 110 BPM, so each measure is 2.18 seconds. The smallest notes are 16ths and 12ths, so we'll represent ALL notes as 1+ 48th-notes in a row
        variable note_length      : integer := 4541667; -- This is how many clock cycles a 48th note will be - (2.18 seconds / 48) * 10^8 (because the frequency is 100 MHz)
        variable current_note_num : integer := 0;        -- This is the number (0-indexed) of the note we currently are on in the song. Ex: 2 = 3rd note.
        variable song_cycle_counter : integer := 0;      -- This counts how many internal clock cycles have passed since the song has started playing.
                                                         -- This is used to handle timing to play the song by switching the notes at the right time.
        variable PLAYING_SONG : std_logic := '0';        -- A flag used to keep track of when the song is ready to be played, starting the counting of cycles
        
        variable num_notes : integer := 192; -- Contains the total number of notes in the song, so we know when to stop.   
        -- Stores all the notes of the song, encoded in the same format as note_next in order, so each 48th note is a 5-bit sequence. 
        -- Thus, the first 5 bits are the note_next for the first 48th note, bits 6-10 are the note_next for the second 48th note, and so on.
        -- For the sake of readability, we've split it up into many concatenated 5-bit chunks, with paired parentheses around the components of each note larger than a 48th
        -- A chunk "00000" represents a rest for that 48th, stored in REST for readability
        variable REST : std_logic_vector(4 downto 0) := "00000";
        variable SONG_NOTES  : std_logic_vector(0 to (num_notes * 5) - 1) := ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") & (REST & REST & REST & REST & REST & REST)
                             & ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") & ("10011" & "10011" & "10011" & "10011" & "10011" & "10011") 
                             & ("10101" & "10101" & "10101" & "10101" & "10101" & "10101") & ("10110" & "10110" & "10110" & "10110" & "10110" & "10110") 
                             & ("10101" & "10101" & "10101" & "10101" & "10101" & "10101") & ("10011" & "10011" & "10011" & "10011" & "10011" & "10011") 
                             & ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") 
                             & (REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST) & ("01010" & "01010" & "01010") 
                             & ("10010" & "10010" & "10010") & ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") 
                             & (REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST) 
                             & ("00110" & "00110" & "00110" & "00110" & "00110" & "00110") 
                             & ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") 
                             & (REST & REST & REST & REST & REST & REST) & ("01100" & "01100" & "01100" & "01100" & "01100" & "01100") 
                             & ("10011" & "10011" & "10011" & "10011" & "10011" & "10011") & ("10101" & "10101" & "10101" & "10101" & "10101" & "10101") 
                             & ("10110" & "10110" & "10110" & "10110" & "10110" & "10110") & ("10101" & "10101" & "10101" & "10101" & "10101" & "10101") 
                             & ("10011" & "10011" & "10011" & "10011" & "10011" & "10011") & ("10110" & "10110" & "10110" & "10110" & "10110" & "10110") 
                             & (REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST & REST) 
                             & ("10110" & "10110" & "10110" & "10110") & ("10101" & "10101" & "10101" & "10101") & ("10011" & "10011" & "10011" & "10011") 
                             & ("10110" & "10110" & "10110" & "10110") & ("10101" & "10101" & "10101" & "10101") & ("10011" & "10011" & "10011" & "10011");                                                     
                                                 
    begin
        if (RST = '1') then
            note_next <= (others => '0');
            -- We should also reset everything related to player piano mode and playing the song
            song_cycle_counter := 0;
            current_note_num := 0;
            PLAYING_SONG := '0';
            PLAYER_MODE <= '0';
        elsif (CLK'event and CLK = '1') then  
            -- Are we in song? If so, start incrementing the cycle counter to keep track of time!
            if (PLAYING_SONG = '1') then
                song_cycle_counter := song_cycle_counter + 1;
                -- Increment the current_note_num if we need to!
                if (song_cycle_counter >= (current_note_num + 1) * note_length) then
                    current_note_num := current_note_num + 1;
                end if;
                
                if (current_note_num = num_notes) then
                    song_cycle_counter := 0;
                    current_note_num := 0;
                    PLAYING_SONG := '0';
                    PLAYER_MODE <= '0';
                end if;
            end if;
            -- When this switch is flipped, the piano goes into player piano mode, but we don't let the user start this mode unless all other switches are off
            -- That is to say, to avoid users accidentally entering player piano mode while playing, we only start player piano mode when that's the only switch pressed
            if (PLAYER_MODE = '0') and (switch = "00000001") then
                -- We've just toggled and we're ready to start playing the song - reset the song_cycle_counter and current_note_num, and flag us as in-song
                song_cycle_counter := 0;
                current_note_num := 0;
                PLAYING_SONG := '1';
                
                PLAYER_MODE <= '1';
            elsif (PLAYER_MODE = '1') and(switch(0) = '1') then
                -- We're in player piano mode, and the switch to stay in it is still on
                -- We ignore all other inputs - the user isn't able to play, and them attempting to do so should not break us out of PLAYER_MODE
                -- This handles the playing of the current note 
                if (PLAYING_SONG = '1') then
                    note_next <= SONG_NOTES((current_note_num * 5) to ((current_note_num * 5) + 4));
                end if;
            else                   
                -- We're not in player piano mode
                PLAYER_MODE <= '0';
                PLAYING_SONG := '0';
                -- Handle manual playing as usual
                case switch is 
                    when "10000000" => note_sel <= "0001"; -- C
                    when "01000000" => note_sel <= "0011"; -- D
                    when "00100000" => note_sel <= "0101"; -- E
                    when "00010000" => note_sel <= "0110"; -- F
                    when "00001000" => note_sel <= "1000"; -- G
                    when "00000100" => note_sel <= "1010"; -- A
                    when "00000010" => note_sel <= "1100"; -- B
                    when others     => note_sel <= "0000";
                end case;
    
                -- Sharp -- Add one.  PB(3) is the octave key.
                if (PB(2) = '1') then
                    note_next <= PB(3) & note_sel + 1;
                -- Flat --  Minus one.
                elsif (PB(1) = '1') then
                    note_next <= PB(3) & note_sel - 1;
                else 
                    note_next <= PB(3) & note_sel;
                end if;
            end if;
        end if;
    end process;
end Behavioral;
