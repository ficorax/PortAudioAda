with Ada.Numerics;

with Interfaces.C;

with PortAudioAda; use PortAudioAda;

package PaEx_Sine_Types is

   Num_Seconds       : constant := 5.0;
   Sample_Rate       : constant := 44_100.0;
   Frames_Per_Buffer : constant := 64;

   Pi                : constant := Ada.Numerics.Pi;

   Table_Size        : constant := 200;

   type Float_Array
     is array (Integer range <>) of aliased Float;
   pragma Convention (C, Float_Array);

   type paTestData is
      record
         sine        : aliased Float_Array (1 .. Table_Size);
         left_phase  : aliased Integer;
         right_phase : aliased Integer;
         message     : aliased String (1 .. 20);
      end record;
   pragma Convention (C, paTestData);

   type paTestData_Ptr is access all paTestData;
   pragma Convention (C, paTestData_Ptr);
   pragma No_Strict_Aliasing (paTestData_Ptr);

   outputParameters : aliased PA_Stream_Parameters;
   data             : aliased paTestData;
   pragma Convention (C, data);

end PaEx_Sine_Types;
