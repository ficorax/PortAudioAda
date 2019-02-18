with Ada.Numerics;

with PortAudioAda; use PortAudioAda;

package PaTest_TooManySines_Types is

   Max_Sines         : constant := 500;
   Max_Load          : constant := 1.2;
   Sample_Rate       : constant := 44100.0;
   Frames_Per_Buffer : constant := 512;
   Pi                : constant := Ada.Numerics.Pi;
   Two_Pi            : constant := Pi * 2.0;

   type Float_Array
     is array (Integer range <>) of aliased Float;
   pragma Convention (C, Float_Array);

   type Double_Array is array (Integer range <>) of aliased Long_Float;

   type paTestData is
      record
         numSines  : aliased Integer;
         phases    : aliased Double_Array (1 .. Max_Sines);
      end record;
   pragma Convention (C, paTestData);

   type paTestData_Ptr is access all paTestData;
   pragma Convention (C, paTestData_Ptr);
   pragma No_Strict_Aliasing (paTestData_Ptr);

   outputParameters : aliased PA_Stream_Parameters;
   data             : aliased paTestData;
   pragma Convention (C, data);

end PaTest_TooManySines_Types;
