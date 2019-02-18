with Ada.Command_Line;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

with PortAudioAda; use PortAudioAda;

with Pa_MinLat_Types; use Pa_MinLat_Types;
with Pa_MinLat_Callbacks; use Pa_MinLat_Callbacks;

procedure Pa_MinLat
is
   package ACL renames Ada.Command_Line;

   err             : Pa_Error;
   stream          : aliased Pa_Stream_Ptr;
   framesPerBuffer : Integer := Default_Buffer_Size;
   outLatency      : Integer := 0;
   minLatency      : constant Integer := Default_Buffer_Size * 2;
   sampleRate      : Long_Float   := 44100.0;

   Finish          : Boolean := False;
begin
   Put_Line ("pa_minlat - Determine minimum latency for your computer.");
   Put_Line ("  usage:         pa_minlat {userBufferSize}");
   Put_Line ("  for example:   pa_minlat 64");
   Put_Line ("Adjust your stereo until you hear" &
             " a smooth tone in each speaker.");
   Put_Line ("Then try to find the smallest number" &
             " of frames that still sounds smooth.");
   Put_Line ("Note that the sound will stop momentarily" &
             " when you change the number of buffers.");

   --  Get bufferSize from command line.

   if ACL.Argument_Count > 0 then
      framesPerBuffer := Integer'Value (ACL.Argument (1));
   end if;
   Put ("Frames per buffer = ");
   Put (framesPerBuffer, 0);
   New_Line;

   data.left_phase  := 0.0;
   data.right_phase := 0.0;

   err := PA_Initialize;

   if err /= paNoError then
      raise PortAudio_Exception;
   end if;

   outLatency := Integer (sampleRate * 200.0 / 1000.0); -- 200 msec.

   --  Try different numBuffers in a loop.

   while not Finish loop
      outputParameters.device           := PA_Get_Default_Output_Device;
      outputParameters.channelCount     := 2;
      outputParameters.sampleFormat     := paFloat32;
      outputParameters.suggestedLatency :=
        Pa_Time (outLatency) / Pa_Time (sampleRate);
      outputParameters.hostApiSpecificStreamInfo := System.Null_Address;

      --  printf("%6.1f msec.\n", outLatency, . * 1000.0 );

      Put ("Latency = ");
      Put (outLatency, 0);
      Put (" frames = ");
      Put (Long_Float (outputParameters.suggestedLatency) * 1000.0, 0, 1, 0);
      Put_Line (" msec.");

      err := PA_Open_Stream
        (stream'Access,
         null,
         outputParameters'Access,
         sampleRate,
         IC.unsigned_long (framesPerBuffer),
         paClipOff,
         paMinLatCallback'Access,
         data'Address);

      if err /= paNoError or stream = null then
         raise PortAudio_Exception;
      end if;

      --  Start audio.

      err := PA_Start_Stream (stream);

      if err /= paNoError then
         raise PortAudio_Exception;
      end if;

      --  Ask user for a new nlatency.

      New_Line;
      Put_Line ("Move windows around to see if the sound glitches.");
      Put ("Latency now ");
      Put (outLatency, 0);
      Put (", enter new number of frames, or 'q' to quit: ");

      declare
         str : constant access String := new String'(Ada.Text_IO.Get_Line);
      begin
         if str.all (1) = 'q' then
            Finish := True;
         else
            outLatency := Integer'Value (str.all);

            if outLatency < minLatency then
               Put ("Latency below minimum of ");
               Put (minLatency, 0);
               Put_Line ("! Set to minimum!!!");
               outLatency := minLatency;
            end if;
         end if;
      exception
         when others =>
            Put_Line ("Put integer number or 'q' to quit");
      end;
      --  Stop sound until ENTER hit.
      err := PA_Stop_Stream (stream);
      if err /= paNoError then
         raise PortAudio_Exception;
      end if;

      err := PA_Close_Stream (stream);
      if err /= paNoError then
         raise PortAudio_Exception;
      end if;
   end loop;

   Put_Line ("A good setting for latency would be somewhat higher than");
   Put_Line ("the minimum latency that worked.");
   Put_Line ("PortAudio: Test finished.");

   err := PA_Terminate;

   -----------------------------------------------------------------------------

exception

   when PortAudio_Exception =>
      err := PA_Terminate;

      Put_Line ("Error occured while using the PortAudio stream");
      Put_Line ("Error code: " & Pa_Error'Image (err));
      Put_Line ("Error message: " & PA_Get_Error_Text (err));

end Pa_MinLat;
