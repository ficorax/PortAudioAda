with Interfaces.C;

with System;

with PortAudioAda; use PortAudioAda;

package Pa_MinLat_Callbacks is

   -----------------------------------------------------------------------------
   --  Very simple synthesis routine to generate two sine waves.

   function paMinLatCallback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result;
   pragma Export (C, paMinLatCallback);

end Pa_MinLat_Callbacks;
