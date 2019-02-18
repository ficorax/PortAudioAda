with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;

with Interfaces.C.Pointers;

with System;

with Pa_MinLat_Types; use Pa_MinLat_Types;

package body Pa_MinLat_Callbacks is

   package Float_Ptrs is
     new Interfaces.C.Pointers (Index              => Natural,
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
      paTestData_Ptr);

   ----------------------
   -- paMinLatCallback --
   ----------------------

   function paMinLatCallback
     (inputBuffer     :        System.Address;
      outputBuffer    :        System.Address;
      framesPerBuffer :        Interfaces.C.unsigned_long;
      timeInfo        : access PA_Stream_Callback_Time_Info;
      statusFlags     :        PA_Stream_Callback_Flags;
      userData        :        System.Address)
      return PA_Stream_Callback_Result
   is
      pragma Unreferenced (inputBuffer);
      pragma Unreferenced (timeInfo);
      pragma Unreferenced (statusFlags);

      oBuff          : Float_Star := Convert (outputBuffer);
      lData          : constant paTestData_Ptr := Convert (userData);
      left_phaseInc  : Float := 0.02;
      right_phaseInc : Float := 0.06;

      left_phase     : Float := lData.all.left_phase;
      right_phase    : Float := lData.all.right_phase;
   begin
      for i in 1 .. Integer (framesPerBuffer) loop
         left_phase := left_phase + left_phaseInc;
         if left_phase > Two_Pi then
            left_phase := left_phase - Two_Pi;
         end if;
         oBuff.all := Sin (left_phase);
         Float_Ptrs.Increment (oBuff);

         right_phase := right_phase + right_phaseInc;
         if right_phase > Two_Pi then
            right_phase := right_phase - Two_Pi;
         end if;
         oBuff.all := Sin (right_phase);
         Float_Ptrs.Increment (oBuff);

      end loop;

      lData.all.left_phase  := left_phase;
      lData.all.right_phase := right_phase;

      return paContinue;
   end paMinLatCallback;

end Pa_MinLat_Callbacks;
