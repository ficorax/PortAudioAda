with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with System;

with PortAudioAda; use PortAudioAda;

with PaTest_TooManySines_Types; use PaTest_TooManySines_Types;
with PaTest_TooManySines_Callbacks; use PaTest_TooManySines_Callbacks;

procedure PaTest_TooManySines
is
   stream : aliased PA_Stream_Ptr;
   err    : PA_Error;

   load      : Long_Float;
   numStress : Integer;

begin
   Put ("PortAudio Test: output sine wave. SR = ");
   Put (Sample_Rate, 0, 0, 0);
   Put (", BufSize = ");
   Put (Frames_Per_Buffer, 0);
   Put (". MAX_LOAD = ");
   Put (Max_Load, 0, 0, 0);
   New_Line;

   err := PA_Initialize;

   if err /= paNoError then
      goto Error;
   end if;

   outputParameters.device := PA_Get_Default_Output_Device;

   if outputParameters.device = paNoDevice then
      Put ("Error: No default output device.");
      goto Error;
   end if;

   outputParameters.channelCount := 1;
   outputParameters.sampleFormat := paFloat32;
   outputParameters.suggestedLatency
     := PA_Get_Device_Info (outputParameters.device).defaultLowOutputLatency;
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
      goto Error;
   end if;

   err := PA_Start_Stream (stream);

   if err /= paNoError then
      goto Error;
   end if;

   --  Determine number of sines required to get to 50%

   loop
      data.numSines := data.numSines + 1;
      delay 0.1;

      load := PA_Get_Stream_Cpu_Load (stream);

      Put ("numSines = ");
      Put (data.numSines, 0);
      Put (", CPU load = ");
      Put (Float (load), 0, 6, 0);
      New_Line;

      exit when load >= 0.5;
   end loop;

   --  Calculate target stress value then ramp up to that level

   numStress := Integer (2.0 * Float (data.numSines) * Max_Load);

   while data.numSines < numStress loop
      delay 0.2;
      load := PA_Get_Stream_Cpu_Load (stream);

      Put ("STRESSING: numSines = ");
      Put (data.numSines, 0);
      Put (", CPU load = ");
      Put (Float (load), 0, 6, 0);
      New_Line;

      data.numSines := data.numSines + 1;
   end loop;

   Put_Line ("Suffer for 5 seconds.");
   delay 5.0;

   Put_Line ("Stop stream.");
   err := PA_Stop_Stream (stream);
   if err /= paNoError then
      goto Error;
   end if;

   err := PA_Close_Stream (stream);
   if err /= paNoError then
      goto Error;
   end if;

   err := PA_Terminate;
   Put_Line ("Test finished.");

   -----------------------------------------------------------------------------

   return;

   <<Error>>
   Put_Line ("Error occured while using the PortAudio stream");
   Put ("Error code: ");
   Put (PA_Error'Image (err));
   New_Line;
   Put ("Error message: ");
   Put (PA_Get_Error_Text (err));
   New_Line;
   err := PA_Terminate;
   pragma Unreferenced (err);

end PaTest_TooManySines;
