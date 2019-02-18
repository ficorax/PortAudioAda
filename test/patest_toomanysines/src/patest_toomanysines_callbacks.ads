with Interfaces.C;

with System;

with PortAudioAda; use PortAudioAda;

package PaTest_TooManySines_Callbacks is

   -----------------------------------------------------------------------------
   --  This routine will be called by the PortAudio engine when audio is needed.
   --  It may called at interrupt level on some machines so don't do anything
   --  that could mess up the system like calling malloc() or free().

   function paTestCallback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result;
   pragma Export (C, paTestCallback);

end PaTest_TooManySines_Callbacks;
