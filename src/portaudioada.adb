--------------------------------------------------------------------------------
-- Copyright (c) 2019 Marek Kuziel
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--------------------------------------------------------------------------------

with Interfaces.C.Strings;

package body PortAudioAda is

   use Interfaces;

   package ICS renames Interfaces.C.Strings;

   type PaVersionInfo is
      record
         versionMajor           : Unsigned_32;
         versionMinor           : Unsigned_32;
         versionSubMinor        : Unsigned_32;
         versionControlRevision : ICS.chars_ptr;
         versionText            : ICS.chars_ptr;
      end record
     with Convention => C;

   type PaVersionInfo_Ptr is access constant PaVersionInfo;

   -----------------------------------------------------------------------------
   --  A structure containing information about a particular host API.
   type HostApiInfo is
      record
      --  this is struct version 1
         structVersion       : aliased Integer;

         --  The well known unique identifier of this host API
         --  See Also: PaHostApiTypeId
         typeId              : aliased PA_Host_Api_Type_Id;

         --  A textual description of the host API for display on user
         --  interfaces.
         name                : aliased ICS.chars_ptr;

         --  The number of devices belonging to this host API. This field may
         --  be used in conjunction with PA_HostApiDeviceIndexToDeviceIndex
         --  to enumerate all devices for this host API.
         --  See Also: PA_HostApiDeviceIndexToDeviceIndex
         deviceCount         : aliased Integer;

         --  The default input device for this host API. The value will be
         --  a device index ranging from 0 to (PA_GetDeviceCount - 1), or
         --  paNoDevice if no default input device is available.
         defaultInputDevice  : aliased PA_Device_Index;

         --  The default output device for this host API. The value will be
         --  a device index ranging from 0 to (PA_GetDeviceCount - 1), or
         --  paNoDevice if no default output device is available.
         defaultOutputDevice : aliased PA_Device_Index;

      end record
   with Convention => C;

   type HostApiInfo_Ptr is access all HostApiInfo
     with Convention => C;

   -----------------------------------------------------------------------------
   --  Structure used to return information about a host error condition.
   type HostErrorInfo is
      record
         hostApiType : PA_Host_Api_Type_Id;
         errorCode   : IC.long;
         errorText   : ICS.chars_ptr;
      end record
   with Convention => C;

   type HostErrorInfo_Ptr is access all HostErrorInfo
     with Convention => C;

   type Device_Info is
      record
         structVersion            : IC.int;
         name                     : ICS.chars_ptr;
         hostApi                  : PA_Host_Api_Index;
         maxInputChannels         : IC.int;
         maxOutputChannels        : IC.int;
         defaultLowInputLatency   : PA_Time;
         defaultLowOutputLatency  : PA_Time;
         defaultHighInputLatency  : PA_Time;
         defaultHighOutputLatency : PA_Time;
         defaultSampleRate        : IC.double;
      end record
   with Convention => C;

   type Device_Info_Ptr is access all Device_Info
     with Convention => C;

   --  A structure containing unchanging information about an open stream.
   --  See Also: PA_GetStreamInfo
   type Stream_Info is
      record
         --  this is struct version 1
         structVersion : aliased Integer;

         --  The input latency of the stream in seconds. This value provides
         --  the most accurate estimate of input latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to PA_OpenStream. The value of this
         --  field will be zero (0.) for output - only streams.
         --
         --  See Also: PaTime
         inputLatency  : aliased PA_Time;

         --  The output latency of the stream in seconds. This value provides
         --  the most accurate estimate of output latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to PA_OpenStream. The value of this
         --  field will be zero (0.) for input - only streams.
         --  See Also: PaTime
         outputLatency : aliased PA_Time;

         --  The sample rate of the stream in Hertz (samples per second). In
         --  cases where the hardware sample rate is inaccurate and PortAudio
         --  is aware of it, the value of this field may be different from the
         --  sampleRate parameter passed to PA_OpenStream. If information about
         --  the actual hardware sample rate is not available, this field will
         --  have the same value as the sampleRate parameter passed to
         --  PA_OpenStream.
         sampleRate    : aliased Long_Float;
      end record
     with Convention => C_Pass_By_Copy;

   type Stream_Info_Ptr is access all Stream_Info
     with Convention => C;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   function PA_GetVersionInfo return PaVersionInfo_Ptr
     with Import => True, Convention => C, Link_Name => "Pa_GetVersionInfo";

   ------------------------
   -- PA_Get_Device_Info --
   ------------------------

   function PA_Get_Device_Info
     (Device : PA_Device_Index)
      return PA_Device_Info
   is
      function Internal (device : PA_Device_Index)
                         return Device_Info_Ptr
        with Import => True, Convention => C, Link_Name => "Pa_GetDeviceInfo";

      rc  : constant Device_Info_Ptr := Internal (Device);
      str : constant String := IC.To_Ada (ICS.Value (rc.name));
      len : constant Natural := str'Length;
   begin
      return (len,
         Integer (rc.structVersion),
         str,
         rc.hostApi,
         Integer (rc.maxInputChannels),
         Integer (rc.maxOutputChannels),
         rc.defaultLowInputLatency,
         rc.defaultLowOutputLatency,
         rc.defaultHighInputLatency,
         rc.defaultHighOutputLatency,
         rc.defaultSampleRate);
   end PA_Get_Device_Info;

   --------------------
   -- Get_Error_Text --
   --------------------
   function PA_Get_Error_Text (Error_Code : PA_Error) return String
   is
      function Internal (error : PA_Error) return ICS.chars_ptr
        with Import => True, Convention => C, Link_Name => "Pa_GetErrorText";
   begin
      return ICS.Value (Internal (Error_Code));
   end PA_Get_Error_Text;

   -----------------------
   -- Get_Host_Api_Info --
   -----------------------

   function PA_Get_Host_Api_Info
     (Host_Api : PA_Host_Api_Index)
      return PA_Host_Api_Info
   is
      function Internal (hostApi : PA_Host_Api_Index)
                         return HostApiInfo_Ptr
        with Import => True, Convention => C, Link_Name => "Pa_GetHostApiInfo";

      rc  : constant HostApiInfo_Ptr := Internal (Host_Api);
      str : constant String := IC.To_Ada (ICS.Value (rc.name));
      len : constant Natural := str'Length;
   begin
      return (len,
              rc.structVersion,
              rc.typeId,
              str,
              rc.deviceCount,
              rc.defaultInputDevice,
              rc.defaultOutputDevice);
   end PA_Get_Host_Api_Info;

   ------------------------------
   -- Get_Last_Host_Error_Info --
   ------------------------------

   function PA_Get_Last_Host_Error_Info return Pa_Host_Error_Info
   is
      function Internal return HostErrorInfo_Ptr
        with
          Import => True,
          Convention => C,
          Link_Name => "Pa_GetLastHostErrorInfo";

      rc  : constant HostErrorInfo_Ptr := Internal;
      str : constant String := IC.To_Ada (ICS.Value (rc.errorText));
      len : constant Natural := str'Length;
   begin
      return (len,
              rc.hostApiType,
              Long_Integer (rc.errorCode),
              str);
   end PA_Get_Last_Host_Error_Info;

   ---------------
   -- Get_Major --
   ---------------
   function PA_Get_Major return Integer
   is
   begin
      return Integer (PA_GetVersionInfo.versionMajor);
   end PA_Get_Major;

   ---------------
   -- Get_Minor --
   ---------------
   function PA_Get_Minor return Integer
   is
   begin
      return Integer (PA_GetVersionInfo.versionMinor);
   end PA_Get_Minor;

   ------------------------
   -- PA_Get_Stream_Info --
   ------------------------

   function PA_Get_Stream_Info (Stream : PA_Stream_Ptr) return PaStreamInfo
   is
      function Internal (Stream : PA_Stream_Ptr) return Stream_Info_Ptr
        with Import => True, Convention => C, Link_Name => "Pa_GetStreamInfo";

     Rc : constant Stream_Info_Ptr := Internal (Stream);
   begin
      return (Rc.structVersion,
              Rc.inputLatency,
              Rc.outputLatency,
              Rc.sampleRate);
   end PA_Get_Stream_Info;

   -------------------
   -- Get_Sub_Minor --
   -------------------
   function PA_Get_Sub_Minor return Integer
   is
   begin
      return Integer (PA_GetVersionInfo.versionSubMinor);
   end PA_Get_Sub_Minor;

   -----------------
   -- Get_Version --
   -----------------
   function PA_Get_Version return Integer
   is
      Major, Minor, Sub_Minor : Unsigned_32;
   begin
      Major := PA_GetVersionInfo.versionMajor;
      Minor := PA_GetVersionInfo.versionMinor;
      Sub_Minor := PA_GetVersionInfo.versionSubMinor;

      return
        Integer (
                 Shift_Left (Major and 16#0000_00FF#, 16) or
                     Shift_Left (Minor and 16#0000_00FF#, 8) or
                     (Sub_Minor and 16#FF#));

   end PA_Get_Version;

   -----------------
   -- Get_Version --
   -----------------
   function PA_Get_Version return String
   is
   begin
      return IC.To_Ada (ICS.Value (PA_GetVersionInfo.versionText));
   end PA_Get_Version;

   ----------------------------------
   -- Get_Version_Control_Revision --
   ----------------------------------
   function PA_Get_Version_Control_Revision return String
   is
   begin
      return IC.To_Ada (ICS.Value (PA_GetVersionInfo.versionControlRevision));
   end PA_Get_Version_Control_Revision;

end PortAudioAda;
