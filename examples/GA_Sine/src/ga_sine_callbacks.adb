with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Conversion;

with Interfaces.C.Pointers;

with System;

with PortAudioAda; use PortAudioAda;

with GA_Sine_Globals; use GA_Sine_Globals;

package body GA_Sine_Callbacks is

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

   function Sine_Callback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result;
   pragma Export (C, Sine_Callback);

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

   -------------------
   -- Sine_Callback --
   -------------------

   function Sine_Callback
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

   begin
      t := (Two_Pi * uData.all.frequency) / (Sample_Rate * 2.0);

      for i in 1 .. Integer (framesPerBuffer) loop
         oBuff.all := Float (uData.all.amplitude * Sin (uData.all.phase));
         Float_Ptrs.Increment (oBuff);

         oBuff.all := Float (uData.all.amplitude * Sin (uData.all.phase));
         Float_Ptrs.Increment (oBuff);

         uData.all.phase :=
           uData.all.phase +
             Two_Pi * uData.all.frequency / Sample_Rate;

         if uData.all.phase > Two_Pi then
            uData.all.phase := uData.all.phase - Two_Pi;
         end if;

      end loop;
      return paContinue;
   end Sine_Callback;

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
                            Sine_Callback'Access,
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

end GA_Sine_Callbacks;
