with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with System;

with PortAudioAda; use PortAudioAda;

with PaEx_Sine_Types; use PaEx_Sine_Types;
with PaEx_Sine_Callbacks; use PaEx_Sine_Callbacks;

procedure PaEx_Sine
is
   stream : aliased Pa_Stream_Ptr;
   err    : PA_Error;

begin
   Put ("PortAudio Test: output sine wave. SR = ");
   Put (Sample_Rate, 0, 0, 0);
   Put (", BufSize = ");
   Put (Frames_Per_Buffer, 0);
   New_Line;
   Put_Line (PA_Get_Version);

   -----------------------------------------------------------------------------

   --  initialize sinusoidal wavetable

   for i in 1 .. Table_Size loop
      data.sine (i) :=
        Sin (Float (i) / Float (Table_Size) * Ada.Numerics.Pi * 2.0);
   end loop;

   data.left_phase  := 1;
   data.right_phase := 1;

   err := PA_Initialize;

   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   outputParameters.device := PA_Get_Default_Output_Device;

   if outputParameters.device = paNoDevice then
      raise PortAudio_Exception;
   end if;

   outputParameters.channelCount := 2;
   outputParameters.sampleFormat := paFloat32;
   outputParameters.suggestedLatency :=
     PA_Get_Device_Info (outputParameters.device).defaultLowOutputLatency;
   outputParameters.hostApiSpecificStreamInfo := System.Null_Address;

   err := PA_Open_Stream
     (stream'Access,
      null,
      outputParameters'Access,
      Sample_Rate,
      Frames_Per_Buffer,
      paClipOff,
      paTestCallback'Access,
      data'Address);

   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   data.message := "No Message          ";
   err := PA_Set_Stream_Finished_Callback (stream, StreamFinished'Access);

   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   err := PA_Start_Stream (stream);
   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   delay Num_Seconds;

   err := PA_Stop_Stream (stream);
   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   err := PA_Close_Stream (stream);
   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   err := PA_Terminate;
   Put_Line ("Test finished.");

   -----------------------------------------------------------------------------

   return;

exception
   when PortAudio_Exception =>
      err := PA_Terminate;

      Put_Line ("Error occured while using the PortAudio stream");
      Put_Line ("Error code: " & PA_Error'Image (err));
      Put_Line ("Error message: " & PA_Get_Error_Text (err));

end PaEx_Sine;
