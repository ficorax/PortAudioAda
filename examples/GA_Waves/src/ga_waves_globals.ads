with Ada.Numerics;

with Gtk.Widget; use Gtk.Widget;

with PortAudioAda; use PortAudioAda;

package GA_Waves_Globals is

   Frames_Per_Buffer : Long_Float := 64.0;
   Sample_Rate       : Long_Float := 44_100.0;

   stream    : aliased PA_Stream_Ptr;

   Two_Pi : Long_Float := Ada.Numerics.Pi * 2.0;

   type Float_Array is array (Integer range <>) of aliased Float;
   pragma Convention (C, Float_Array);

   type User_Data is record
      amplitude : aliased Long_Float   := 0.5;
      frequency : aliased Long_Float   := 440.0;
      phase     : aliased Long_Float   := 0.0;
   end record;
   pragma Convention (C, User_Data);

   type User_Data_Ptr is access all User_Data;
   pragma Convention (C, User_Data_Ptr);
   pragma No_Strict_Aliasing (User_Data_Ptr);

   gUserData : aliased User_Data;

   type Option_Use is
     (OU_Before,
      OU_After,
      OU_NoMatter);

   type One_Option is record
      optionWidget : access Gtk_Widget_Record'Class;
      optionUse    :        Option_Use;
   end record;

   type Enum_Options is
     (EO_Start,
      EO_Frames_Per_Buffer,
      EO_Sample_Rate,
      EO_Wave_Type,
      EO_Amplitude,
      EO_Frequency);

   options : array (Enum_Options) of One_Option;

   type Samples is record
      numRate : Long_Float;
      txtRate : String (1 .. 6);
   end record;

   sample_Rates : array (Natural range <>) of Samples :=
                    ((8000.0,   "  8000"),
                     (9600.0,   "  9600"),
                     (1025.0,   " 11025"),
                     (12000.0,  " 12000"),
                     (16000.0,  " 16000"),
                     (22050.0,  " 22050"),
                     (24000.0,  " 24000"),
                     (32000.0,  " 32000"),
                     (44100.0,  " 44100"),
                     (48000.0,  " 48000"),
                     (88200.0,  " 88200"),
                     (96000.0,  " 96000"),
                     (192000.0, "192000"));

   type Wave_Types is
     (Sine,
      Square,
      Sawtooth,
      Triangle,
      WhiteNoise,
      PinkNoise,
      BrownNoise);

   wave_Type : Wave_Types := Sine;

   -----------------------------------------------------------------------------

   procedure Toggle_Options (started : Boolean);

end GA_Waves_Globals;
