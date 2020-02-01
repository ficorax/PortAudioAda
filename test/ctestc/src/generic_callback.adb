with Interfaces.C;
with System;

package body Generic_Callback is

   function paMinLatCallback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        IC.unsigned_long;
      timeInfo        : access PaStreamCallbackTimeInfo;
      statusFlags     :        PaStreamCallbackFlags;
      userData        :        System.Address)
      return PaStreamCallbackResult
     with Export => True, Convention => C;

   ---------------------
   -- PAA_Open_Stream --
   ---------------------

   function PAA_Open_Stream
     (stream           : access PaStream_Ptr;
      inputParameters  : access PaStreamParameters;
      outputParameters : access PaStreamParameters;
      sampleRate       :        Long_Float;
      framesPerBuffer  :        Long_Integer;
      streamFlags      :        PaStreamFlags;
      pre              :        Pre_Callback;
      post             :        Post_Callback;
      userData         :        User_Data)
      return PaError
   is
   begin

   end PAA_Open_Stream;

   ----------------------
   -- paMinLatCallback --
   ----------------------

   function paMinLatCallback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        Interfaces.C.unsigned_long;
      timeInfo        : access PaStreamCallbackTimeInfo;
      statusFlags     :        PaStreamCallbackFlags;
      userData        :        System.Address)
      return PaStreamCallbackResult
   is
   begin
      for i in 1 .. Integer (framesPerBuffer) loop
         null;
      end loop;

      return paContinue;
   end paMinLatCallback;

end Generic_Callback;
