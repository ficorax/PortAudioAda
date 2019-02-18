with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;

with Interfaces.C.Pointers;

with PaTest_TooManySines_Types; use PaTest_TooManySines_Types;

package body PaTest_TooManySines_Callbacks is

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
      paTestData_Ptr);

   --------------------
   -- paTestCallback --
   --------------------

   function paTestCallback
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

      oBuff : Float_Star := Convert (outputBuffer);
      lData : constant paTestData_Ptr := Convert (userData);
   begin
      for i in 1 .. Integer (framesPerBuffer) loop
         declare
            output   : Float := 0.0;
            phaseInc : Long_Float := 0.02;
            phase    : Long_Float;
         begin
            for j in 1 .. lData.all.numSines loop

               --  Advance phase of next oscillator.

               phase := lData.all.phases (j);
               phase := phase + phaseInc;

               if phase > Two_Pi then
                  phase := phase - Two_Pi;
               end if;

               phaseInc := phaseInc * 1.02;

               if phaseInc > 0.5 then
                  phaseInc := phaseInc * 0.5;
               end if;

               --  This is not a very efficient way to calc sines.

               output := output + Sin (Float (phase));
               lData.all.phases (j) := phase;

            end loop;

            oBuff.all := output / Float (lData.all.numSines);
            Float_Ptrs.Increment (oBuff);
         end;
      end loop;
      return paContinue;
   end paTestCallback;

end PaTest_TooManySines_Callbacks;
