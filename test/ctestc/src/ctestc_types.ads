with Ada.Numerics;

with PortAudioAda; use PortAudioAda;

package Ctestc_Types is

   Pi                  : constant := Ada.Numerics.Pi;
   Two_Pi              : constant := Pi * 2.0;
   Default_Buffer_Size : constant := 32;

   type Float_Array
     is array (Natural range <>) of aliased Float;
   pragma Convention (C, Float_Array);

   type paTestData is
      record
         left_phase  : aliased Float;
         right_phase : aliased Float;
      end record;
   pragma Convention (C, paTestData);

   type paTestData_Ptr is access all paTestData;
   pragma Convention (C, paTestData_Ptr);
   pragma No_Strict_Aliasing (paTestData_Ptr);

   outputParameters : aliased PA_Stream_Parameters;
   data             : aliased paTestData;
   pragma Convention (C, data);

end Ctestc_Types;
