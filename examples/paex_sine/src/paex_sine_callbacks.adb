with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C.Pointers;

with System;

with PaEx_Sine_Types; use PaEx_Sine_Types;

package body PaEx_Sine_Callbacks is

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
         oBuff.all := lData.all.sine (lData.all.left_phase);
         Float_Ptrs.Increment (oBuff);

         oBuff.all := lData.all.sine (lData.all.right_phase);
         Float_Ptrs.Increment (oBuff);

         lData.all.left_phase := lData.all.left_phase + 1;

         if lData.all.left_phase > Table_Size then
            lData.all.left_phase := lData.all.left_phase - Table_Size;
         end if;

         lData.all.right_phase := lData.all.right_phase + 3;

         if lData.all.right_phase > Table_Size then
            lData.all.right_phase := lData.all.right_phase - Table_Size;
         end if;

      end loop;
      return paContinue;
   end paTestCallback;

   --------------------
   -- StreamFinished --
   --------------------

   procedure StreamFinished (userData : System.Address) is
      lData : constant paTestData_Ptr := Convert (userData);
   begin
      Put ("Stream completed: ");
      Put (lData.all.message);
   end StreamFinished;

end PaEx_Sine_Callbacks;
