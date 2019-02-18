with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Text_IO;            use Ada.Text_IO;

with System;

with PortAudioAda;           use PortAudioAda;

procedure PA_Devices
is
   procedure Print_Supported_Standard_Sample_Rates
     (inputParameters  : access PA_Stream_Parameters;
      outputParameters : access PA_Stream_Parameters);

   procedure Print_Supported_Standard_Sample_Rates
     (inputParameters  : access PA_Stream_Parameters;
      outputParameters : access PA_Stream_Parameters)
   is
      standardSampleRates : constant array (Natural range <>) of Long_Float :=
        (8000.0,
         9600.0,
         11025.0,
         12000.0,
         16000.0,
         22050.0,
         24000.0,
         32000.0,
         44100.0,
         48000.0,
         88200.0,
         96000.0,
         192000.0
        );

      printCount : Integer;
      err        : PA_Error;
   begin
      printCount := 0;

      for i in standardSampleRates'Range loop
         err := PA_Is_Format_Supported (inputParameters,
                                        outputParameters,
                                        standardSampleRates (i));

        if err = paFormatIsSupported then
            if printCount = 0 then
               Put (ASCII.HT);
               Put (standardSampleRates (i), 8, 2, 0);
               printCount := 1;
            elsif printCount = 4 then
               Put (",");
               New_Line;
               Put (ASCII.HT);
               Put (standardSampleRates (i), 8, 2, 0);
               printCount := 1;
            else
               Put (", ");
               Put (standardSampleRates (i), 8, 2, 0);
               printCount := printCount + 1;
            end if;
        end if;
      end loop;

      if printCount = 0 then
         Put_Line ("None");
      else
         New_Line;
      end if;
   end Print_Supported_Standard_Sample_Rates;

   err        : PA_Error;
   numHostApi : PA_Host_Api_Index;
   numDevices : PA_Device_Index;
begin
   err := PA_Initialize;

   New_Line;

   if err /= paNoError then
      Put_Line ("ERROR: PA_Initialize returned " & err'Image);
      raise PortAudio_Exception;
   end if;

   Put_Line ("PortAudio version: " & PA_Get_Version);

   numHostApi := PA_Get_Host_API_Count;

   if numHostApi < 0 then
      Put_Line ("ERROR: Get_API_Count returned " & numHostApi'Image);
      raise PortAudio_Exception;
   end if;

   Put_Line ("Number of Host API:" & numHostApi'Image);

   numDevices := PA_Get_Device_Count;

   if numDevices < 0 then
      Put_Line ("ERROR: Get_Device_Count returned " & numDevices'Image);
      raise PortAudio_Exception;
   end if;

   Put_Line ("Number of devices:" & numDevices'Image);

   for i in 0 .. numDevices - 1 loop
      declare
         deviceInfo       : constant PA_Device_Info := PA_Get_Device_Info (i);
         defaultDisplayed : Boolean := False;
         inputParameters,
         outputParameters : aliased PA_Stream_Parameters;

      begin
         Put_Line
           ("--------------------------------------- device #" & i'Image);

         --  Mark global and API specific default devices

         if i = PA_Get_Default_Input_Device then
            Put ("[ Default Input");
            defaultDisplayed := True;
         elsif i = PA_Get_Host_Api_Info (deviceInfo.hostApi).defaultInputDevice
         then
            declare
               hostInfo : constant PA_Host_Api_Info
                 := PA_Get_Host_Api_Info (deviceInfo.hostApi);
            begin
               Put ("[ Default " & hostInfo.name & " Input");
               defaultDisplayed := True;
            end;
         end if;

         if i = PA_Get_Default_Output_Device then
            Put ((if defaultDisplayed then "," else "["));
            Put (" Default Output");
            defaultDisplayed := True;
         elsif i = PA_Get_Host_Api_Info (deviceInfo.hostApi).defaultOutputDevice
         then
            declare
               hostInfo : constant PA_Host_Api_Info
                 := PA_Get_Host_Api_Info (deviceInfo.hostApi);
            begin
               Put ((if defaultDisplayed then "," else "["));
               Put (" Default " & hostInfo.name & " Output");
               defaultDisplayed := True;
            end;
         end if;

         if defaultDisplayed then
            Put_Line (" ]");
         end if;

         --  print device info fields

         Put_Line ("Name                        = " & deviceInfo.name);
         Put_Line ("Host API                    = " &
                     PA_Get_Host_Api_Info (deviceInfo.hostApi).name);
         Put ("Max inputs = " & deviceInfo.maxInputChannels'Image);
         Put_Line (", Max outputs = " & deviceInfo.maxOutputChannels'Image);

         Put ("Default low input latency   = ");
         Put (Long_Float (deviceInfo.defaultLowInputLatency), 8, 4, 0);
         New_Line;
         Put ("Default low output latency  = ");
         Put (Long_Float (deviceInfo.defaultLowOutputLatency), 8, 4, 0);
         New_Line;
         Put ("Default high input latency  = ");
         Put (Long_Float (deviceInfo.defaultHighInputLatency), 8, 4, 0);
         New_Line;
         Put ("Default high output latency = ");
         Put (Long_Float (deviceInfo.defaultHighOutputLatency), 8, 4, 0);
         New_Line;

         Put ("Default sample rate         = ");
         Put (Long_Float (deviceInfo.defaultSampleRate), 8, 2, 0);
         New_Line;

         --  poll for standard sample rates

         inputParameters.device := i;
         inputParameters.channelCount := deviceInfo.maxInputChannels;
         inputParameters.sampleFormat := paInt16;
         inputParameters.suggestedLatency := 0.0;
         inputParameters.hostApiSpecificStreamInfo := System.Null_Address;

         outputParameters.device := i;
         outputParameters.channelCount := deviceInfo.maxOutputChannels;
         outputParameters.sampleFormat := paInt16;
         outputParameters.suggestedLatency := 0.0;
         outputParameters.hostApiSpecificStreamInfo := System.Null_Address;

         if inputParameters.channelCount > 0 then
            Put_Line ("Supported standard sample rates");
            Put (" for half-duplex 16 bit ");
            Put (inputParameters.channelCount, 0);
            Put_Line (" channel input = ");

            Print_Supported_Standard_Sample_Rates
              (inputParameters'Unchecked_Access,
               null);

         end if;

         if outputParameters.channelCount > 0 then
            Put_Line ("Supported standard sample rates");
            Put (" for half-duplex 16 bit ");
            Put (outputParameters.channelCount, 0);
            Put_Line (" channel output = ");

            Print_Supported_Standard_Sample_Rates
              (null,
               outputParameters'Unchecked_Access);
         end if;

         if inputParameters.channelCount > 0
           and then outputParameters.channelCount > 0
         then
            Put_Line ("Supported standard sample rates");
            Put (" for full-duplex 16 bit ");
            Put (inputParameters.channelCount, 0);
            Put (" channel input, ");
            Put (outputParameters.channelCount, 0);
            Put_Line (" channel output = ");

            Print_Supported_Standard_Sample_Rates
              (inputParameters'Unchecked_Access,
               outputParameters'Unchecked_Access);
         end if;
      end;
   end loop;

   err := PA_Terminate;

exception
   when PortAudio_Exception =>
      err := PA_Terminate;

      Put_Line ("Error number:  " & err'Image);
      Put_Line ("Error message: " & PA_Get_Error_Text (err));

end PA_Devices;
