with Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Conversion;

with Interfaces.C.Pointers;

with System;

with PortAudioAda; use PortAudioAda;

with GA_Waves_Globals; use GA_Waves_Globals;

package body GA_Waves_Callbacks is

   rndGen : Generator;

   function Sign (value : Long_Float) return Integer;

   package Float_Ptrs is
     new Interfaces.C.Pointers (Index              => Integer,
                                Element            => Float,
                                Element_Array      => Float_Array,
                                Default_Terminator => 0.0);

   use type Float_Ptrs.Pointer;
   subtype Float_Star is Float_Ptrs.Pointer;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address,
      Float_Star);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address,
      User_Data_Ptr);

   function Wave_Callback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result;
   pragma Export (C, Wave_Callback);

   ------------------------
   -- Amplitude_Callback --
   ------------------------

   procedure Amplitude_Callback
     (widget : access Gtk_Hscale_Record'Class)
   is
   begin
      gUserData.amplitude := Long_Float (Get_Value (widget));
   end Amplitude_Callback;

   --------------------------------
   -- Frames_Per_Buffer_Callback --
   --------------------------------

   procedure Frames_Per_Buffer_Callback
     (widget : access Gtk_Spin_Button_Record'Class)
   is
   begin
      Frames_Per_Buffer := Long_Float (Get_Value (widget));
   end Frames_Per_Buffer_Callback;

   ------------------------
   -- Frequency_Callback --
   ------------------------

   procedure Frequency_Callback
     (widget : access Gtk_Hscale_Record'Class)
   is
   begin
      gUserData.frequency := Long_Float (Get_Value (widget));
   end Frequency_Callback;

   --------------------------
   -- Sample_Rate_Callback --
   --------------------------

   procedure Sample_Rate_Callback
     (widget : access Gtk_Combo_Box_Record'Class)
   is
   begin
      for i in sample_Rates'Range loop
         if sample_Rates (i).txtRate = Get_Active_Text (widget) then
            Sample_Rate := sample_Rates (i).numRate;
         end if;
      end loop;

   end Sample_Rate_Callback;

   ----------
   -- Sign --
   ----------

   function Sign (value : Long_Float) return Integer
   is
   begin
      if value > 0.0 then
         return 1;
      elsif value = 0.0 then
         return 0;
      elsif value < 0.0 then
         return -1;
      end if;

      return 0;
   end Sign;

   -------------------
   -- Stop_Callback --
   -------------------

   procedure Start_Callback
     (widget : access Gtk_Widget_Record'Class)
   is
      err       : PA_Error;
      outParams : aliased PA_Stream_Parameters;
   begin
      Toggle_Options (started => True);

      outParams.device := PA_Get_Default_Output_Device;

      if outParams.device = paNoDevice then
         raise PortAudio_Exception
           with PA_Get_Error_Text (PA_Error_Code'Val (outParams.device));
      end if;

      outParams.channelCount := 2;
      outParams.sampleFormat := paFloat32;
      outParams.suggestedLatency :=
        PA_Get_Device_Info (outParams.device).defaultLowOutputLatency;
      outParams.hostApiSpecificStreamInfo := System.Null_Address;

      err := PA_Open_Stream (stream'Access,
                            null,
                            outParams'Access,
                            Sample_Rate,
                            IC.unsigned_long (Frames_Per_Buffer),
                            paNoFlag,
                            Wave_Callback'Access,
                            gUserData'Address);

      if err /= paNoError then
         raise PortAudio_Exception with PA_Get_Error_Text (err);
      end if;

      err := PA_Start_Stream (stream);

      if err /= paNoError then
         raise PortAudio_Exception with PA_Get_Error_Text (err);
      end if;

   end Start_Callback;

   -------------------
   -- Stop_Callback --
   -------------------

   procedure Stop_Callback
     (widget : access Gtk_Widget_Record'Class)
   is
      err : PA_Error;
   begin
      err := PA_Stop_Stream (stream);

      err := PA_Close_Stream (stream);

      Toggle_Options (started => False);
   end Stop_Callback;

   -------------------
   -- Wave_Callback --
   -------------------

   function Wave_Callback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result
   is
      pragma Unreferenced (inputBuffer);
      pragma Unreferenced (timeInfo);
      pragma Unreferenced (statusFlags);

      oBuff : Float_Star     := Convert (outputBuffer);
      uData : User_Data_Ptr  := Convert (userData);
      t     : Long_Float;

      procedure Sine_Gen;
      procedure Square_Gen;
      procedure Sawtooth_Gen;
      procedure WhiteNoise_Gen;
      procedure BrownNoise_Gen;

      procedure BrownNoise_Gen
      is
         t : Float;
         b : Float := 0.0;
      begin
         for i in 1 .. Integer (framesPerBuffer) loop

            loop
               t := Random (rndGen);

               b := b + t;

               if b < -8.0 or else b > 8.0 then
                  b := b - t;
               else
                  exit;
               end if;
            end loop;

            b := b * 0.0625;

            oBuff.all := t * Float (uData.all.amplitude);
            Float_Ptrs.Increment (oBuff);

            oBuff.all := t * Float (uData.all.amplitude);
            Float_Ptrs.Increment (oBuff);
         end loop;
      end BrownNoise_Gen;

      procedure Sawtooth_Gen
      is
      begin
         t := (Two_Pi * uData.all.frequency) / (Sample_Rate * 2.0);

         for i in 1 .. Integer (framesPerBuffer) loop
            oBuff.all := Float (uData.all.amplitude *
                                  Long_Float'Floor (Sin (uData.all.phase + t)));
            Float_Ptrs.Increment (oBuff);

            oBuff.all := Float (uData.all.amplitude *
                                  Long_Float'Floor (Sin (uData.all.phase + t)));
            Float_Ptrs.Increment (oBuff);

            uData.all.phase :=
              uData.all.phase +
                Two_Pi * uData.all.frequency / Sample_Rate;

            if uData.all.phase > Two_Pi then
               uData.all.phase := uData.all.phase - Two_Pi;
            end if;

         end loop;
      end Sawtooth_Gen;

      procedure Sine_Gen
      is
      begin
         t := (Two_Pi * uData.all.frequency) / (Sample_Rate * 2.0);

         for i in 1 .. Integer (framesPerBuffer) loop
            oBuff.all :=
              Float (uData.all.amplitude * Sin (uData.all.phase + t));
            Float_Ptrs.Increment (oBuff);

            oBuff.all :=
              Float (uData.all.amplitude * Sin (uData.all.phase + t));
            Float_Ptrs.Increment (oBuff);

            uData.all.phase :=
              uData.all.phase +
                Two_Pi * uData.all.frequency / Sample_Rate;

            if uData.all.phase > Two_Pi then
               uData.all.phase := uData.all.phase - Two_Pi;
            end if;

         end loop;
      end Sine_Gen;

      procedure Square_Gen
      is
      begin
         t := (Two_Pi * uData.all.frequency) / (Sample_Rate * 2.0);

         for i in 1 .. Integer (framesPerBuffer) loop
            oBuff.all :=
              Float (uData.all.amplitude *
                       Long_Float (Sign (Sin (uData.all.phase + t))));
            Float_Ptrs.Increment (oBuff);

            oBuff.all :=
              Float (uData.all.amplitude *
                       Long_Float (Sign (Sin (uData.all.phase + t))));
            Float_Ptrs.Increment (oBuff);

            uData.all.phase :=
              uData.all.phase +
                Two_Pi * uData.all.frequency / Sample_Rate;

            if uData.all.phase > Two_Pi then
               uData.all.phase := uData.all.phase - Two_Pi;
            end if;

         end loop;
      end Square_Gen;

      procedure WhiteNoise_Gen
      is
      begin
         for i in 1 .. Integer (framesPerBuffer) loop

            oBuff.all := Random (rndGen) * Float (uData.all.amplitude);
            Float_Ptrs.Increment (oBuff);

            oBuff.all := Random (rndGen) * Float (uData.all.amplitude);
            Float_Ptrs.Increment (oBuff);
         end loop;
      end WhiteNoise_Gen;

   begin
      case wave_Type is
         when Sine =>
            Sine_Gen;
         when Square =>
            Square_Gen;
         when Sawtooth =>
            Sawtooth_Gen;
         when WhiteNoise =>
            WhiteNoise_Gen;
         when BrownNoise =>
            BrownNoise_Gen;
         when others =>
            null;
      end case;

      return paContinue;
   end Wave_Callback;

   ------------------------
   -- Wave_Type_Callback --
   ------------------------

   procedure Wave_Type_Callback
     (widget : access Gtk_Combo_Box_Record'Class)
   is
      text : String := Get_Active_Text (widget);
   begin
      if text = "Sine" then
         wave_Type := Sine;
      elsif text = "Square" then
         wave_Type := Square;
      elsif text = "Sawtooth" then
         wave_Type := Sawtooth;
      elsif text = "Triangle" then
         wave_Type := Triangle;
      elsif text = "White Noise" then
         wave_Type := WhiteNoise;
      elsif text = "Pink Noise" then
         wave_Type := PinkNoise;
      elsif text = "Brown Noise" then
         wave_Type := BrownNoise;
      end if;
   end Wave_Type_Callback;

end GA_Waves_Callbacks;
