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

with Interfaces.C;
with System;

package PortAudioAda is

   package IC renames Interfaces.C;

   --  Error codes returned by PortAudio functions
   type PA_Error_Code is
     (
      paNotInitialized,
      paUnanticipatedHostError,
      paInvalidChannelCount,
      paInvalidSampleRate,
      paInvalidDevice,
      paInvalidFlag,
      paSampleFormatNotSupported,
      paBadIODeviceCombination,
      paInsufficientMemory,
      paBufferTooBig,
      paBufferTooSmall,
      paNullCallback,
      paBadStreamPtr,
      paTimedOut,
      paInternalError,
      paDeviceUnavailable,
      paIncompatibleHostApiSpecificStreamInfo,
      paStreamIsStopped,
      paStreamIsNotStopped,
      paInputOverflowed,
      paOutputUnderflowed,
      paHostApiNotFound,
      paInvalidHostApi,
      paCanNotReadFromACallbackStream,
      paCanNotWriteToACallbackStream,
      paCanNotReadFromAnOutputOnlyStream,
      paCanNotWriteToAnInputOnlyStream,
      paIncompatibleStreamHostApi,
      paBadBufferPtr,
      paNoError,
      paStreamActive
     );

   for PA_Error_Code use
     (
      paNotInitialized                        => -10_000,
      paUnanticipatedHostError                => -9_999,
      paInvalidChannelCount                   => -9_998,
      paInvalidSampleRate                     => -9_997,
      paInvalidDevice                         => -9_996,
      paInvalidFlag                           => -9_995,
      paSampleFormatNotSupported              => -9_994,
      paBadIODeviceCombination                => -9_993,
      paInsufficientMemory                    => -9_992,
      paBufferTooBig                          => -9_991,
      paBufferTooSmall                        => -9_990,
      paNullCallback                          => -9_989,
      paBadStreamPtr                          => -9_988,
      paTimedOut                              => -9_987,
      paInternalError                         => -9_986,
      paDeviceUnavailable                     => -9_985,
      paIncompatibleHostApiSpecificStreamInfo => -9_984,
      paStreamIsStopped                       => -9_983,
      paStreamIsNotStopped                    => -9_982,
      paInputOverflowed                       => -9_981,
      paOutputUnderflowed                     => -9_980,
      paHostApiNotFound                       => -9_979,
      paInvalidHostApi                        => -9_978,
      paCanNotReadFromACallbackStream         => -9_977,
      paCanNotWriteToACallbackStream          => -9_976,
      paCanNotReadFromAnOutputOnlyStream      => -9_975,
      paCanNotWriteToAnInputOnlyStream        => -9_974,
      paIncompatibleStreamHostApi             => -9_973,
      paBadBufferPtr                          => -9_972,
      paNoError                               => 0,
      paStreamActive                          => 1
     );

   for PA_Error_Code'Size use 32;
   pragma Convention (C, PA_Error_Code);

   paFormatIsSupported : constant PA_Error_Code := paNoError;
   paStreamNotActive   : constant PA_Error_Code := paNoError;
   paStreamStopped     : constant PA_Error_Code := paStreamActive;
   paStreamNotStopped  : constant PA_Error_Code := paNoError;

   subtype PA_Error is PA_Error_Code;

   PortAudio_Exception : exception;

   type PA_Time is new IC.double;
   --  The type used to represent monotonic time in seconds. PaTime is used for
   --  the fields of the PaStreamCallbackTimeInfo argument to the
   --  PaStreamCallback and as the result of Get_Stream_Time.
   --
   --  PaTime values have unspecified origin.
   --
   --  See also: PaStreamCallback, PaStreamCallbackTimeInfo, Get_Stream_Time

   subtype PA_Sample_Format is IC.unsigned_long;
   --  A type used to specify one or more sample formats. Each value indicates
   --  a possible format for sound data passed to and from the stream callback,
   --  PA_ReadStream and PA_WriteStream.
   --
   --  The standard formats paFloat32, paInt16, paInt32, paInt24, paInt8 and
   --  aUInt8 are usually implemented by all implementations.
   --
   --  The floating point representation (paFloat32) uses +1.0 and -1.0 as
   --  the maximum and minimum respectively.
   --
   --  paUInt8 is an unsigned 8 bit format where 128 is considered "ground"
   --
   --  The paNonInterleaved flag indicates that a multichannel buffer is
   --  passed as a set of non-interleaved pointers.
   --
   --  See Also: PA_OpenStream, PA_OpenDefaultStream, PaDeviceInfo
   --  paFloat32, paInt16, paInt32, paInt24, paInt8, paUInt8,
   --  paCustomFormat, paNonInterleaved

   paFloat32        : constant PA_Sample_Format := 16#0000_0001#;
   paInt32          : constant PA_Sample_Format := 16#0000_0002#;
   paInt24          : constant PA_Sample_Format := 16#0000_0004#;
   paInt16          : constant PA_Sample_Format := 16#0000_0008#;
   paInt8           : constant PA_Sample_Format := 16#0000_0010#;
   paUInt8          : constant PA_Sample_Format := 16#0000_0020#;
   paCustomFormat   : constant PA_Sample_Format := 16#0001_0000#;

   paNonInterleaved : constant PA_Sample_Format := 16#8000_0000#;

   --  The type used to refer to audio devices. Values of this type usually
   --  range from 0 to (Get_Device_Count - 1), and may also take on the
   --  PaNoDevice and paUseHostApiSpecificDeviceSpecification values.
   --
   --  See Also: Get_Device_Count, paNoDevice,
   --  paUseHostApiSpecificDeviceSpecification
   subtype PA_Device_Index is Integer;

   --  A special PaDeviceIndex value indicating that no device is available,
   --  or should be used.
   --
   --  See Also: PaDeviceIndex
   paNoDevice : constant PA_Device_Index := -1;

   --  A special PaDeviceIndex value indicating that the device(s) to be used
   --  are specified in the host API specific stream info structure.
   --
   --  See Also: PaDeviceIndex
   paUseHostApiSpecificDeviceSpecification : constant PA_Device_Index := -2;

   --  The type used to enumerate to host APIs at runtime. Values of this type
   --  range from 0 to (Get_API_Count - 1).
   --
   --  See Get_API_Count
   type PA_Host_Api_Index is new Integer;

   --  Unchanging unique identifiers for each supported host API. This type is
   --  used in the PaHostApiInfo structure. The values are guaranteed to be
   --  unique and to never change, thus allowing code to be written that
   --  conditionally uses host API specific extensions.
   --
   --  New type ids will be allocated when support for a host API reaches
   --  "public alpha" status, prior to that developers should use the
   --  paInDevelopment type id.
   --
   --  See Also: PaHostApiInfo
   type PA_Host_Api_Type_Id is
     (
      paInDevelopment,
      paDirectSound,
      paMME,
      paASIO,
      paSoundManager,
      paCoreAudio,
      paOSS,
      paALSA,
      paAL,
      paBeOS,
      paWDMKS,
      paJACK,
      paWASAPI,
      paAudioScienceHPI
     );

   for PA_Host_Api_Type_Id use
     (
      paInDevelopment   => 0,
      paDirectSound     => 1,
      paMME             => 2,
      paASIO            => 3,
      paSoundManager    => 4,
      paCoreAudio       => 5,
      paOSS             => 7,
      paALSA            => 8,
      paAL              => 9,
      paBeOS            => 10,
      paWDMKS           => 11,
      paJACK            => 12,
      paWASAPI          => 13,
      paAudioScienceHPI => 14
     );
   pragma Convention (C, PA_Host_Api_Type_Id);

   --  A structure containing information about a particular host API.
   type PA_Host_Api_Info (Length : Natural) is
      record
      --  this is struct version 1
         structVersion       : Integer;

         --  The well known unique identifier of this host API
         --  See Also: PaHostApiTypeId
         typeId              : PA_Host_Api_Type_Id;

         --  A textual description of the host API for display on user
         --  interfaces.
         name                : String (1 .. Length);

         --  The number of devices belonging to this host API. This field may
         --  be used in conjunction with PA_HostApiDeviceIndexToDeviceIndex
         --  to enumerate all devices for this host API.
         --  See Also: PA_HostApiDeviceIndexToDeviceIndex
         deviceCount         : PA_Device_Index;

         --  The default input device for this host API. The value will be
         --  a device index ranging from 0 to (PA_GetDeviceCount - 1), or
         --  paNoDevice if no default input device is available.
         defaultInputDevice  : PA_Device_Index;

         --  The default output device for this host API. The value will be
         --  a device index ranging from 0 to (PA_GetDeviceCount - 1), or
         --  paNoDevice if no default output device is available.
         defaultOutputDevice : PA_Device_Index;

      end record;

   --  Structure used to return information about a host error condition.
   type Pa_Host_Error_Info (Length : Natural) is
      record
         --  the host API which returned the error code
         hostApiType : PA_Host_Api_Type_Id;

         --  the error code returned
         errorCode : Long_Integer;

         --  a textual description of the error if available, otherwise
         --  a zero-length string
         errorText : String (1 ..  Length);
      end record;

   --  A structure providing information and capabilities of PortAudio devices.
   --  Devices may support input, output or both input and output.
   type PA_Device_Info (Length : Natural) is
      record
         --  this is struct version 2
         structVersion            : Integer;

         --  device name
         name                     : String (1 .. Length);

         --  note this is a host API index, not a type id
         hostApi                  : PA_Host_Api_Index;

         --  max input channels
         maxInputChannels         : Integer;

         --  max output channels
         maxOutputChannels        : Integer;

         --  Default input latency values for interactive performance.
         defaultLowInputLatency   : PA_Time;

         --  Default output latency values for interactive performance.
         defaultLowOutputLatency  : PA_Time;

         --  Default input latency values for robust non-interactive
         --  (eg. playing sound files).
         defaultHighInputLatency  : PA_Time;

         --  Default output latency values for robust non-interactive
         --  (eg. playing sound files).
         defaultHighOutputLatency : PA_Time;

         --  default sample rate
         defaultSampleRate        : IC.double;
      end record;

   --  Parameters for one direction (input or output) of a stream.
   type PA_Stream_Parameters is
      record
         --  A valid device index in the range 0 to (Get_Device_Count-1)
         --  specifying the device to be used or the special constant
         --  paUseHostApiSpecificDeviceSpecification which indicates that
         --  the actual device(s) to use are specified in
         --  hostApiSpecificStreamInfo. This field must not be set to paNoDevice
         device : PA_Device_Index;

         --  The number of channels of sound to be delivered to the stream
         --  callback or accessed by Read_Stream or Write_Stream. It can range
         --  from 1 to the value of maxInputChannels in the PaDeviceInfo record
         --  for the device specified by the device parameter.
         channelCount : Integer;

         --  The sample format of the buffer provided to the stream callback,
         --  Read_Stream or Write_Stream. It may be any of the formats described
         --  by the PaSampleFormat enumeration.
         sampleFormat : PA_Sample_Format;

         --  The desired latency in seconds. Where practical, implementations
         --  should configure their latency based on these parameters, otherwise
         --  they may choose the closest viable latency instead. Unless the
         --  suggested latency is greater than the absolute upper limit for the
         --  device implementations should round the suggestedLatency up to the
         --  next practical value - ie to provide an equal or higher latency
         --  than suggestedLatency wherever possible. Actual latency values for
         --  an open stream may be retrieved using the inputLatency and
         --  outputLatency fields of the PaStreamInfo structure returned by
         --  Get_Stream_Info.
         --  See: default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
         suggestedLatency : PA_Time;

         --  An optional pointer to a host api specific data structure
         --  containing additional information for device setup and/or stream
         --  processing. hostApiSpecificStreamInfo is never required for correct
         --  operation, if not used it should be set to NULL.
         hostApiSpecificStreamInfo : System.Address;
      end record
     with Convention => C_Pass_By_Copy;

   --  A single PaStream can provide multiple channels of real-time streaming
   --  audio input and output to a client application. A stream provides access
   --  to audio hardware represented by one or more PaDevices. Depending on the
   --  underlying Host API, it may be possible to open multiple streams using
   --  the same device, however this behavior is implementation defined.
   --  Portable applications should assume that a PaDevice may be simultaneously
   --  used by at most one PaStream.
   --
   --  Pointers to PaStream objects are passed between PortAudio functions that
   --  operate on streams.
   --
   --  See: Open_Stream, Open_Default_Stream, Close_Stream, Start_Stream,
   --  Stop_Stream, Abort_Stream, Is_Stream_Active, Get_Stream_Time,
   --  Get_Stream_Cpu_Load
   type PaStream is new System.Address;

   type PA_Stream_Ptr is access all PaStream
     with Convention => C;

   -----------------------------------------------------------------------------
   --  Can be passed as the framesPerBuffer parameter to Open_Stream or
   --  Open_Default_Stream to indicate that the stream callback will accept
   --  buffers of any size.
   paFramesPerBufferUnspecified : constant IC.unsigned_long := 0;

   -----------------------------------------------------------------------------
   --  Flags used to control the behavior of a stream. They are passed as
   --  parameters to PA_OpenStream or PA_OpenDefaultStream. Multiple flags may
   --  be ORed together.
   --
   --  See Also: PA_OpenStream, PA_OpenDefaultStream
   --  See Also: paNoFlag, paClipOff, paDitherOff, paNeverDropInput,
   --  paPrimeOutputBuffersUsingStreamCallback, paPlatformSpecificFlags
   type PaStreamFlags is new IC.unsigned_long
     with Convention => C;

   paNoFlag         : constant PaStreamFlags := 0;

   --  Disable default clipping of out of range samples.
   --  See Also: PaStreamFlags
   paClipOff        : constant PaStreamFlags := 16#0000_0001#;

   --  Disable default dithering.
   --  See Also: PaStreamFlags
   paDitherOff      : constant PaStreamFlags := 16#0000_0002#;

   --  Flag requests that where possible a full duplex stream will not discard
   --  overflowed input samples without calling the stream callback. This flag
   --  is only valid for full duplex callback streams and only when used in
   --  combination with the paFramesPerBufferUnspecified (0) framesPerBuffer
   --  parameter. Using this flag incorrectly results in a paInvalidFlag error
   --  being returned from PA_OpenStream and PA_OpenDefaultStream.
   --
   --  See Also: PaStreamFlags, paFramesPerBufferUnspecified

   paNeverDropInput : constant PaStreamFlags := 16#0000_0004#;
   --  Call the stream callback to fill initial output buffers, rather than the
   --  default behavior of priming the buffers with zeros (silence). This flag
   --  has no effect for input-only and blocking read/write streams.
   --
   --  See Also: PaStreamFlags

   paPrimeOutputBuffersUsingStreamCallback : constant PaStreamFlags
     := 16#0000_0008#;

   --  A mask specifying the platform specific bits.
   --  See Also: PaStreamFlags
   paPlatformSpecificFlags : constant PaStreamFlags := 16#FFFF_0000#;

   --  Timing information for the buffers passed to the stream callback.
   type PA_Stream_Callback_Time_Info is
      record
         --  The time when the first sample of the input buffer was captured at
         --  the ADC input
         inputBufferAdcTime  : aliased PA_Time;

         --  The time when the stream callback was invoked
         currentTime         : aliased PA_Time;

         --  The time when the first sample of the output buffer will output
         --  the DAC
         outputBufferDacTime : aliased PA_Time;
      end record
   with Convention => C_Pass_By_Copy;

   --  Flag bit constants for the statusFlags to PaStreamCallback.
   --  See Also: paInputUnderflow, paInputOverflow, paOutputUnderflow,
   --  paOutputOverflow, paPrimingOutput
   type PA_Stream_Callback_Flags is new IC.unsigned_long
     with Convention => C;

   --  In a stream opened with paFramesPerBufferUnspecified, indicates that
   --  input data is all silence (zeros) because no real data is available.
   --  In a stream opened without paFramesPerBufferUnspecified, it indicates
   --  that one or more zero samples have been inserted into the input buffer
   --  to compensate for an input underflow.
   --  See Also: PaStreamCallbackFlags
   paInputUnderflow  : constant PA_Stream_Callback_Flags := 16#0000_0001#;

   --  In a stream opened with paFramesPerBufferUnspecified, indicates that
   --  data prior to the first sample of the input buffer was discarded due
   --  to an overflow, possibly because the stream callback is using too much
   --  CPU time. Otherwise indicates that data prior to one or more samples in
   --  the input buffer was discarded.
   --  See Also: PaStreamCallbackFlags
   paInputOverflow   : constant PA_Stream_Callback_Flags := 16#0000_0002#;

   --  Indicates that output data (or a gap) was inserted, possibly because
   --  the stream callback is using too much CPU time.
   --  See Also: PaStreamCallbackFlags
   paOutputUnderflow : constant PA_Stream_Callback_Flags := 16#0000_0004#;

   --  Indicates that output data will be discarded because no room is
   --  available.
   --  See Also: PaStreamCallbackFlags
   paOutputOverflow  : constant PA_Stream_Callback_Flags := 16#0000_0008#;

   --  Some of all of the output data will be used to prime the stream, input
   --  data may be zero.
   --  See Also: PaStreamCallbackFlags
   paPrimingOutput   : constant PA_Stream_Callback_Flags := 16#0000_0010#;

   --  Allowable return values for the PaStreamCallback.
   --  See Also: PaStreamCallback
   type PA_Stream_Callback_Result is
     (
      paContinue,
      --  Signal that the stream should continue invoking the callback and
      --  processing audio.

      paComplete,
      --  Signal that the stream should stop invoking the callback and finish
      --  once all output samples have played.

      paAbort
      --  Signal that the stream should stop invoking the callback and finish
      --  as soon as possible.
     ) with Convention => C;

   for PA_Stream_Callback_Result use
     (
      paContinue => 0,
      paComplete => 1,
      paAbort => 2
     );

   --  Functions of type PaStreamCallback are implemented by PortAudio clients.
   --  They consume, process or generate audio in response to requests from an
   --  active PortAudio stream.
   --
   --  When a stream is running, PortAudio calls the stream callback
   --  periodically. The callback function is responsible for processing buffers
   --  of audio samples  passed via the input and output parameters.
   --
   --  The PortAudio stream callback runs at very high or real-time priority. It
   --  is required to consistently meet its time deadlines. Do not allocate
   --   memory, access the file system, call library functions or call other
   --  functions  from the stream callback that may block or take an
   --  unpredictable amount of time to complete.
   --
   --  In order for a stream to maintain glitch-free operation the callback must
   --  consume and return audio data faster than it is recorded and/or played.
   --  PortAudio anticipates that each callback invocation may execute for
   --  a duration approaching the duration of frameCount audio frames at
   --  the stream  sample rate. It is reasonable to expect to be able to
   --  utilise 70% or more of the available CPU time in the PortAudio callback.
   --  However, due to buffer size  adaption and other factors, not all host
   --  APIs are able to guarantee audio stability under heavy CPU load with
   --  arbitrary fixed callback buffer sizes. When high callback CPU utilisation
   --  is required the most robust behavior can be achieved by using
   --  paFramesPerBufferUnspecified as the Open_Stream framesPerBuffer
   --  parameter.
   --
   --  @param input array of interleaved samples, the format, packing and
   --  number of channels used by the buffers are determined by parameters to
   --  Open_Stream.
   --
   --  @param output array of interleaved samples, the format, packing and
   --  number of channels used by the buffers are determined by parameters to
   --  Open_Stream.
   --
   --  @param frameCount The number of sample frames to be processed by the
   --  stream callback.
   --
   --  @param timeInfo Timestamps indicating the ADC capture time of the first
   --  sample in the input buffer, the DAC output time of the first sample in
   --  the output buffer and the time the callback was invoked.
   --
   --  See PaStreamCallbackTimeInfo and Get_Stream_Time
   --
   --  @param statusFlags Flags indicating whether input and/or output buffers
   --  have been inserted or will be dropped to overcome underflow or overflow
   --  conditions.
   --
   --  @param userData The value of a user supplied pointer passed to
   --  Open_Stream intended for storing synthesis data etc.
   --
   --  @return The stream callback should return one of the values in the
   --  PaStreamCallbackResult enumeration. To ensure that the callback continues
   --  to be called, it should return paContinue. Either paComplete or paAbort
   --  can be returned to finish stream processing, after either of these values
   --  is returned the callback will not be called again. If paAbort is returned
   --  the stream will finish as soon as possible. If paComplete is returned,
   --  the stream will continue until all buffers generated by the callback have
   --  been played. This may be useful in applications such as soundfile players
   --  where a specific duration of output is required. However, it is not
   --  necessary to utilize this mechanism as Stop_Stream, Abort_Stream or
   --  Close_Stream can also be used to stop the stream. The callback must
   --  always fill the entire output buffer irrespective of its return value.
   --
   --  See Also: PA_OpenStream, PA_OpenDefaultStream
   --
   --  Note
   --
   --   With the exception of PA_GetStreamCpuLoad() it is not permissible to
   --   call PortAudio API functions from within the stream callback.
   type PA_Stream_Callback is access function
     (Input        :        System.Address;
      Output       :        System.Address;
      Frame_Count  :        IC.unsigned_long;
      Time_Info    : access PA_Stream_Callback_Time_Info;
      Status_Flags :        PA_Stream_Callback_Flags;
      User_Data    :        System.Address)
      return PA_Stream_Callback_Result
     with Convention => C;

   --  Functions of type PaStreamFinishedCallback are implemented by PortAudio
   --  clients. They can be registered with a stream using the
   --  PA_SetStreamFinishedCallback function. Once registered they are called
   --  when the stream becomes inactive (ie once a call to PA_StopStream will
   --  not block).
   --  A stream will become inactive after the stream callback returns non-zero,
   --  or when PA_StopStream or PA_AbortStream is called. For a stream providing
   --  audio output, if the stream callback returns paComplete, or PA_StopStream
   --  is called, the stream finished callback will not be called until all
   --  generated sample data  has been played.
   --
   --  @param userData The userData parameter supplied to PA_OpenStream
   --
   --  See Also: PA_SetStreamFinishedCallback
   type PA_Stream_Finished_Callback is
     access procedure (User_Data : System.Address)
     with Convention => C;

   --  A structure containing unchanging information about an open stream.
   --  See Also: PA_GetStreamInfo
   type PaStreamInfo is
      record
         --  this is struct version 1
         structVersion : Integer;

         --  The input latency of the stream in seconds. This value provides
         --  the most accurate estimate of input latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to PA_OpenStream. The value of this
         --  field will be zero (0.) for output - only streams.
         --
         --  See Also: PaTime
         inputLatency  : PA_Time;

         --  The output latency of the stream in seconds. This value provides
         --  the most accurate estimate of output latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to PA_OpenStream. The value of this
         --  field will be zero (0.) for input - only streams.
         --  See Also: PaTime
         outputLatency : PA_Time;

         --  The sample rate of the stream in Hertz (samples per second). In
         --  cases where the hardware sample rate is inaccurate and PortAudio
         --  is aware of it, the value of this field may be different from the
         --  sampleRate parameter passed to PA_OpenStream. If information about
         --  the actual hardware sample rate is not available, this field will
         --  have the same value as the sampleRate parameter passed to
         --  PA_OpenStream.
         sampleRate    : Long_Float;
      end record;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   --  General functions
   -----------------------------------------------------------------------------

   function PA_Initialize return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_Initialize";
   --  Library initialization function - call this before using PortAudio.
   --  This function initializes internal data structures and prepares
   --  underlying host APIs for use.  With the exception of Get_Version,
   --  Get_Version+Text, and Get_Erro_rText, this function MUST be called
   --  before using any other PortAudio API functions.
   --
   --  If PA_Initialize is called multiple times, each successful
   --  call must be matched with a corresponding call to PA_Terminate.
   --  Pairs of calls to PA_Initialize/PA_Terminate may overlap, and are not
   --  required to be fully nested.
   --
   --  Note that if PA_Initialize returns an error code, PA_Terminate should
   --  NOT be called.
   --
   --  @return paNoError if successful, otherwise an error code indicating the
   --  cause of failure.
   --
   --  See Also: PA_Terminate

   function PA_Terminate return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_Terminate";
   --  Library termination function - call this when finished using PortAudio.
   --  This function deallocates all resources allocated by PortAudio since it
   --  was initialized by a call to PA_Initialize. In cases where PA_Initialize
   --  has been called multiple times, each call must be matched with a
   --  corresponding call to PA_Terminate. The final matching call to
   --  PA_Terminate will automatically close any PortAudio streams that are
   --  still open.
   --
   --  PA_Terminate MUST be called before exiting a program which uses
   --  PortAudio. Failure to do so may result in serious resource leaks, such
   --  as audio devices not being available until the next reboot.
   --
   --  @return paNoError if successful, otherwise an error code indicating the
   --  cause of failure.
   --
   --  See Also: PA_Initialize

   function PA_Get_Version return Integer;
   --  Retrieve the release number of the currently running PortAudio build.
   --  For example, for version "19.5.1" this will return 16#00130501#.

   function PA_Get_Version return String;
   --  Retrieve a textual description of the current PortAudio build, e.g.
   --  "PortAudio V19.5.0-devel, revision 1952M".
   --  The format of the text may change in the future. Do not try to parse
   --  the returned string.

   function PA_Get_Major return Integer;
   function PA_Get_Minor return Integer;
   function PA_Get_Sub_Minor return Integer;
   function PA_Get_Version_Control_Revision return String;

   function PA_Get_Error_Text (Error_Code : PA_Error) return String;
   --  Translate the supplied PortAudio error code into a human readable
   --  message.
   --
   --  @param errorCode error code to translate
   --
   --  @return Human readable message.

   function PA_Get_Sample_Size (Format : PA_Sample_Format) return Integer
     with Import => True, Convention => C, Link_Name => "PA_GetSampleSize";
   --  Retrieve the size of a given sample format in bytes.
   --
   --  @param format Sample format
   --
   --  @return The size in bytes of a single sample in the specified format, or
   --  paSampleFormatNotSupported if the format is not supported.

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   --  Host functions
   -----------------------------------------------------------------------------

   function PA_Get_Host_API_Count return PA_Host_Api_Index
     with Import => True, Convention => C, Link_Name => "PA_GetHostApiCount";
   --  Retrieve the number of available host APIs. Even if a host API is
   --  available it may have no devices available.
   --
   --  @return A non-negative value indicating the number of available host APIs
   --  or, a PaErrorCode (which are always negative) if PortAudio is not
   --  initialized  or an error is encountered.
   --
   --  See PaHostApiIndex

   function PA_Get_Default_Host_API return PA_Host_Api_Index
     with Import => True, Convention => C, Link_Name => "PA_GetDefaultHostApi";
   --  Retrieve the index of the default host API. The default host API will be
   --  the lowest common denominator host API on the current platform and is
   --  unlikely to provide the best performance.
   --
   --  @return A non-negative value ranging from 0 to (PA_GetHostApiCount - 1)
   --  indicating the default host API index or, a PaErrorCode (which are always
   --  negative) if PortAudio is not initialized or an error is encountered.

   function PA_Get_Host_Api_Info (Host_Api : PA_Host_Api_Index)
                                  return PA_Host_Api_Info;
   --  Retrieve a structure containing information about a specific host API.
   --
   --  @param hostApi A valid host API index ranging from 0 to
   --  (PA_GetHostApiCount - 1)
   --
   --  @return PaHostApiInfo structure describing a specific host API. If the
   --  hostApi parameter is out of range or an error is encountered, the
   --  function returns null.

   function PA_Host_Api_Type_Id_To_Host_Api_Index
     (Type_Id : PA_Host_Api_Type_Id)
      return PA_Host_Api_Index
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_HostApiTypeIdToHostApiIndex";
   --  Convert a static host API unique identifier, into a runtime host API
   --  index.
   --
   --  @param typeId A unique host API identifier belonging to the
   --  PaHostApiTypeId enumeration.
   --
   --  @return  A valid PaHostApiIndex ranging from 0 to
   --  (PA_Get_Host_Api_Count - 1) or, a PaErrorCode (which are always negative)
   --  if PortAudio is not initialized or an error is encountered.
   --
   --  The paHostApiNotFound error code indicates that the host API specified by
   --  the type parameter is not available.
   --
   --  See Also: PaHostApiTypeId

   function PA_Host_Api_Device_Index_To_Device_Index
     (Host_Api              : PA_Host_Api_Index;
      Host_Api_Device_Index : Integer)
      return PA_Device_Index
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_HostApiDeviceIndexToDeviceIndex";
   --  Convert a host-API-specific device index to standard PortAudio device
   --  index. This function may be used in conjunction with the deviceCount
   --  field of PaHostApiInfo to enumerate all devices for the specified host
   --  API.
   --
   --  @param hostApi A valid host API index ranging from 0 to
   --  (Get_Host_Api_Count - 1)
   --
   --  @param hostApiDeviceIndex A valid per-host device index in the range 0 to
   --  (Get_Host_Api_Info (hostApi).deviceCount - 1)
   --
   --  @return A non-negative PaDeviceIndex ranging from 0 to
   --  (Get_Device_Count - 1) or, a PaErrorCode (which are always negative) if
   --  PortAudio is not initialized or an error is encountered.
   --
   --  A paInvalidHostApi error code indicates that the host API index
   --  specified by the hostApi parameter is out of range.
   --
   --  A paInvalidDevice error code indicates that the
   --  hostApiDeviceIndex parameter is out of range.
   --
   --  See Also: PaHostApiInfo

   function PA_Get_Last_Host_Error_Info return Pa_Host_Error_Info;
   --  Return information about the last host error encountered. The error
   --  information returned by Get_Last_Host_Error_Info will never be modified
   --  asynchronously by errors occurring in other PortAudio owned threads such
   --  as the thread that manages the stream callback.)
   --
   --  This function is provided as a last resort, primarily to enhance
   --  debugging by providing clients with access to all available error
   --  information.
   --
   --  @return A structure constraining information about the host error.
   --  The values in this structure will only be valid if a PortAudio function
   --  has previously returned the paUnanticipatedHostError error code.

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   --  Device functions
   -----------------------------------------------------------------------------

   function PA_Get_Device_Count return PA_Device_Index
     with Import => True, Convention => C, Link_Name => "PA_GetDeviceCount";
   --  Retrieve the number of available devices. The number of available
   --  devices may be zero.
   --
   --  @return
   --    A non-negative value indicating the number of available devices
   --    or, a PaErrorCode (which are always negative) if PortAudio is not
   --    initialized or an error is encountered.

   function PA_Get_Default_Input_Device return PA_Device_Index
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_GetDefaultInputDevice";
   --  Retrieve the index of the default input device. The result can be
   --  used in the inputDevice parameter to PA_OpenStream().
   --
   --  @return The default input device index for the default host API, or
   --  paNoDevice if no default input device is available or an error
   --  was encountered.

   function PA_Get_Default_Output_Device return PA_Device_Index
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_GetDefaultOutputDevice";
   --  Retrieve the index of the default output device. The result can be used
   --  in the outputDevice parameter to Open_Stream.
   --  Note    On the PC, the user can specify a default device by setting an
   --  environment variable. For example, to use device #1.
   --     set PA_RECOMMENDED_OUTPUT_DEVICE=1
   --  The user should first determine the available device ids by using
   --  the supplied application "PA_devs".
   --
   --  @return The default output device index for the default host API, or
   --  paNoDevice if no default output device is available or an error
   --  was encountered.

   function PA_Get_Device_Info (Device : PA_Device_Index) return PA_Device_Info;
   --  Retrieve a pointer to a PaDeviceInfo structure containing information
   --  about the specified device.
   --  @param device A valid device index in the range 0 to
   --                (Get_Device_Count - 1)
   --  @return A pointer to an immutable PaDeviceInfo structure. If the device
   --          parameter is out of range the function returns NULL.
   --
   --  See Also:   PaDeviceInfo, PaDeviceIndex

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   --  Stream functions
   -----------------------------------------------------------------------------

   function PA_Is_Format_Supported
     (Input_Parameters  : access PA_Stream_Parameters;
      Output_Parameters : access PA_Stream_Parameters;
      Ssample_Rate      : Long_Float)
      return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_IsFormatSupported";
   --  Determine whether it would be possible to open a stream with the
   --  specified parameters.
   --
   --  @param inputParameters A structure that describes the input parameters
   --  used to open a stream. The suggestedLatency field is ignored. See
   --  PaStreamParameters for a description of these parameters. inputParameters
   --  must be null for output-only streams.
   --
   --  @param outputParameters A structure that describes the output parameters
   --  used to open a stream. The suggestedLatency field is ignored. See
   --  PaStreamParameters for a description of these parameters.
   --  outputParameters must be null for input-only streams.
   --
   --  @param sampleRate The required sampleRate. For full-duplex streams it is
   --  the sample rate for both input and output
   --
   --  @return Returns paFormatIsSupported if the format is supported, and an
   --  error code indicating why the format is not supported otherwise. The
   --  constant paFormatIsSupported is provided to compare with the return value
   --  for success.
   --
   --  See Also:paFormatIsSupported, PaStreamParameters

   function PA_Open_Stream (Stream            : access PA_Stream_Ptr;
                            Input_Parameters  : access PA_Stream_Parameters;
                            Output_Parameters : access PA_Stream_Parameters;
                            Sample_Rate       :        Long_Float;
                            Frames_Per_Buffer :        IC.unsigned_long;
                            Stream_Flags      :        PaStreamFlags;
                            Stream_Callback   :        PA_Stream_Callback;
                            User_Data         :        System.Address)
                            return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_OpenStream";
   --  Opens a stream for either input, output or both.
   --
   --  @param stream The address of a PaStream pointer which will receive
   --  a pointer to the newly opened stream.
   --
   --  @param inputParameters A structure that describes the input parameters
   --  used by the opened stream. See PaStreamParameters for a description of
   --  these parameters. inputParameters must be null for output-only streams.
   --
   --  @param outputParameters A structure that describes the output parameters
   --  used by the opened stream. See PaStreamParameters for a description of
   --  these parameters. outputParameters must be null for input-only streams.
   --
   --  @param sampleRate The desired sampleRate. For full-duplex streams it is
   --  the sample rate for both input and output
   --
   --  @param framesPerBuffer The number of frames passed to the stream callback
   --  function, or the preferred block granularity for a blocking read/write
   --  stream. The special value paFramesPerBufferUnspecified (0) may be used
   --  to request that the stream callback will receive an optimal (and possibly
   --  varying) number of frames based on host requirements and the requested
   --  latency settings.
   --
   --  Note
   --   With some host APIs, the use of non-zero framesPerBuffer for a
   --   callback stream may introduce an additional layer of buffering
   --   which could introduce additional latency. PortAudio guarantees
   --   that the additional latency will be kept to the theoretical minimum
   --   however, it is strongly recommended that a non-zero framesPerBuffer
   --   value only be used when your algorithm requires a fixed number of
   --   frames per stream callback.
   --
   --  @param streamFlags Flags which modify the behaviour of the streaming
   --  process. This parameter may contain a combination of flags ORed together.
   --  Some flags may only be relevant to certain buffer formats.
   --
   --  @param streamCallback A pointer to a client supplied function that is
   --  responsible for processing and filling input and output buffers. If this
   --  parameter is null the stream will be opened in 'blocking read/write'
   --  mode. In blocking mode, the client can receive sample data using
   --  Read_Stream and write sample data using Write_Stream, the number of
   --  samples that may be read or written without blocking is returned by
   --  Get_Stream_Read_Available and Get_Stream_Write_Available respectively.
   --
   --  @param userData A client supplied pointer which is passed to the stream
   --  callback function. It could for example, contain a pointer to instance
   --  data necessary for processing the audio buffers. This parameter is
   --  ignored if streamCallback is null.
   --
   --  @return Upon success PA_OpenStream returns paNoError and places a pointer
   --  to a valid PaStream in the stream argument. The stream is inactive
   --  (stopped).
   --  If a call to PA_OpenStream fails, a non-zero error code is returned see
   --  PaError for possible error codes) and the value of stream is invalid.
   --
   --  See Also: PaStreamParameters, PaStreamCallback, PA_ReadStream,
   --  PA_WriteStream, PA_GetStreamReadAvailable, PA_GetStreamWriteAvailable

   function PA_Open_Default_Stream
     (Stream              : access PA_Stream_Ptr;
      Num_Input_Channels  :        Integer;
      Num_Output_Channels :        Integer;
      Sample_Format       :        PA_Sample_Format;
      Sample_Rate         :        Long_Float;
      Frames_Per_Buffer   :        IC.unsigned_long;
      Stream_Callback     :        PA_Stream_Callback;
      User_Data           :        System.Address)
      return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_OpenDefaultStream";
   --  A simplified version of PA_OpenStream that opens the default input
   --  and/or output devices.
   --
   --  @param stream The address of a PaStream pointer which will receive
   --  a pointer to the newly opened stream.
   --
   --  @param numInputChannels The number of channels of sound that will be
   --  supplied to the stream callback or returned by Read_Stream. It can range
   --  from 1 to the value of maxInputChannels in the PaDeviceInfo record for
   --  the default input device. If 0 the stream is opened as an output-only
   --  stream.
   --
   --  @param numOutputChannels The number of channels of sound to be delivered
   --  to the stream callback or passed to Write_Stream. It can range from 1
   --  to the value of maxOutputChannels in the PaDeviceInfo record for the
   --  default output device. If 0 the stream is opened as an output-only
   --  stream.
   --
   --  @param sampleFormat he sample format of both the input and output buffers
   --  provided to the callback or passed to and from Read_Stream and
   --  Write_Stream. sampleFormat may be any of the formats described by the
   --  PaSampleFormat enumeration.
   --
   --  @param sampleRate Same as Open_Stream parameter of the same name.
   --
   --  @param framesPerBuffer Same as Open_Stream parameter of the same name.
   --
   --  @param streamCallback Same as Open_Stream parameter of the same name.
   --
   --  @param userData Same as Open_Stream parameter of the same name.
   --
   --  @return As for Open_Stream
   --
   --  See Also: PA_OpenStream, PaStreamCallback

   function PA_Close_Stream (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_CloseStream";
   --  Closes an audio stream. If the audio stream is active it discards any
   --  pending buffers as if PA_AbortStream had been called.
   --
   --  @param stream Stream to close to

   function PA_Set_Stream_Finished_Callback
     (Stream                   : PA_Stream_Ptr;
      Stream_Finished_Callback : PA_Stream_Finished_Callback)
      return PA_Error
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_SetStreamFinishedCallback";
   --  Register a stream finished callback function which will be called when
   --  the  stream becomes inactive. See the description of
   --  PaStreamFinishedCallback for  further details about when the callback
   --  will be called.
   --
   --  @param stream A pointer to a PaStream that is in the stopped state - if
   --  the stream is not stopped, the stream's finished callback will remain
   --  unchanged and an error code will be returned.
   --
   --  @param streamFinishedCallback A pointer to a function with the same
   --  signature as PaStreamFinishedCallback, that will be called when the
   --  stream becomes inactive. Passing null for this parameter will un-register
   --  a previously registered stream finished callback function.
   --
   --  @return On success returns paNoError, otherwise an error code indicating
   --  the cause of the error.
   --
   --  See Also: PaStreamFinishedCallback

   function PA_Start_Stream (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_StartStream";
   --  Commences audio processing.
   --
   --  @param stream A stream to start

   function PA_Stop_Stream (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_StopStream";
   --  Terminates audio processing. It waits until all pending audio buffers
   --  have been played before it returns.
   --
   --  @param stream A stream to stop

   function Abort_Stream (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_AbortStream";
   --  Terminates audio processing immediately without waiting for pending
   --  buffers to complete.
   --
   --  @param stream A stream to abort

   function PA_Is_Stream_Stopped (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_IsStreamStopped";
   --  Determine whether the stream is stopped.
   --  A stream is considered to be stopped prior to a successful call
   --  to PA_StartStream and after a successful call to PA_StopStream or
   --  PA_AbortStream. If a stream callback returns a value other than
   --  paContinue the stream is NOT considered to be stopped.
   --
   --  @param stream A stream to check.
   --
   --  @return Returns one paStreamStopped when the stream is stopped,
   --  paStreamNotStopped when the stream is running or, a PaErrorCode if
   --  PortAudio is not initialized or an error is encountered.
   --
   --  See Also: Stop_Stream, Abort_Stream, Is_Stream_Active

   function PA_Is_Stream_Active (Stream : PA_Stream_Ptr) return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_IsStreamActive";
   --  Determine whether the stream is active.
   --  A stream is active after a successful call to PA_StartStream, until it
   --  becomes inactive either as a result of a call to PA_StopStream or
   --  PA_AbortStream, or as a result of a return value other than paContinue
   --  from the stream callback. In the latter case, the stream is considered
   --  inactive after the last buffer has finished playing.
   --
   --  @param stream A stream to check
   --
   --  @return eturns one paStreamActive when the stream is active (ie playing
   --  or recording audio), paStreamNotActive when not playing or, a PaErrorCode
   --   if PortAudio is not initialized or an error is encountered.
   --
   --  See Also: Stop_Stream, Abort_Stream, Is_Stream_Stopped

   function PA_Get_Stream_Info (Stream : PA_Stream_Ptr) return PaStreamInfo;
   --  Retrieve a pointer to a PaStreamInfo structure containing information
   --  about the specified stream.
   --
   --  @param stream A pointer to an open stream previously created with
   --  Open_Stream.
   --
   --  @return A pointer to an immutable PaStreamInfo structure. If the stream
   --  parameter invalid, or an error is encountered, the function returns null.
   --
   --  Note
   --
   --   PortAudio manages the memory referenced by the returned pointer, the
   --   client must not manipulate or free the memory. The pointer is only
   --   guaranteed to be valid until the specified stream is closed.
   --
   --  See Also: PaStreamInfo

   function PA_Get_Stream_Time (Stream : PA_Stream_Ptr) return PA_Time
     with Import => True, Convention => C, Link_Name => "PA_GetStreamTime";
   --  Returns the current time in seconds for a stream according to the same
   --  clock used to generate callback PaStreamCallbackTimeInfo timestamps. The
   --  time values are monotonically increasing and have unspecified origin.
   --
   --  Get_Stream_Time returns valid time values for the entire life of
   --  the stream, from when the stream is opened until it is closed. Starting
   --  and stopping the stream does not affect the passage of time returned by
   --  Get_Stream_Time.
   --  This time may be used for synchronizing other events to the audio stream,
   --  for  example synchronizing audio to MIDI.
   --
   --  @return The stream's current time in seconds, or 0 if an error occurred.
   --
   --  See Also: PaTime, PaStreamCallback, PaStreamCallbackTimeInfo

   function PA_Get_Stream_Cpu_Load (Stream : PA_Stream_Ptr) return Long_Float
     with Import => True, Convention => C, Link_Name => "PA_GetStreamCpuLoad";
   --  Retrieve CPU usage information for the specified stream.
   --  The "CPU Load" is a fraction of total CPU time consumed by a callback
   --  stream's audio processing routines including, but not limited to the
   --  client supplied stream callback. This function does not work with
   --  blocking read/write streams.
   --
   --  This function may be called from the stream callback function or the
   --  application.
   --
   --  @return A floating point value, typically between 0.0 and 1.0, where 1.0
   --  indicates that the stream callback is consuming the maximum number of
   --  CPU cycles possible to maintain real-time operation. A value of 0.5
   --  would imply that PortAudio and the stream callback was consuming
   --  roughly 50% of the available CPU time. The return value may exceed
   --  1.0. A value of 0.0 will always be returned for a blocking read/write
   --  stream, or if an error occurs.

   function PA_Read_Stream (Stream : PA_Stream_Ptr;
                            Buffer : System.Address;
                            Frames : IC.unsigned_long)
                            return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_ReadStream";
   --  Read samples from an input stream. The function doesn't return until
   --  the entire buffer has been filled - this may involve waiting for the
   --  operating  system to supply the data.
   --
   --  @param stream  A pointer to an open stream previously created with
   --  Open_Stream.
   --
   --  @param buffer A pointer to a buffer of sample frames. The buffer contains
   --  samples in the format specified by the inputParameters.sampleFormat field
   --  used to open the stream, and the number of channels specified by
   --  inputParameters.numChannels. If non-interleaved samples were
   --  requested, buffer is a pointer to the first element of an array of
   --  non-interleaved buffer pointers, one for each channel.
   --
   --  @param frames The number of frames to be read into buffer. This parameter
   --  is not constrained to a specific range, however high performance
   --  applications will want to match this parameter to the
   --  framesPerBuffer parameter used when opening the stream.
   --
   --  @return On success PaNoError will be returned, or PaInputOverflowed if
   --  input data was discarded by PortAudio after the previous call and before
   --  this call.

   function PA_Write_Stream (Stream : PA_Stream_Ptr;
                             Buffer : System.Address;
                             Frames : IC.unsigned_long)
                             return PA_Error
     with Import => True, Convention => C, Link_Name => "PA_WriteStream";
   --  Write samples to an output stream. This function doesn't return until the
   --  entire buffer has been consumed - this may involve waiting for the
   --  operating system to consume the data.
   --
   --  @param stream A pointer to an open stream previously created with
   --  Open_Stream.
   --
   --  @param buffer A pointer to a buffer of sample frames. The buffer contains
   --  samples in the format specified by the outputParameters.sampleFormat
   --  field used to open the stream, and the number of channels specified by
   --  outputParameters.numChannels. If non-interleaved samples were
   --  requested, buffer is a pointer to the first element of an array of
   --  non-interleaved buffer pointers, one for each channel.
   --
   --  @param frames The number of frames to be written from buffer. This
   --  parameter is not constrained to a specific range, however high
   --  performance applications will want to match this parameter to the
   --  framesPerBuffer parameter used when opening the stream.
   --
   --  @return On success PaNoError will be returned, or paOutputUnderflowed if
   --  additional output data was inserted after the previous call and before
   --  this call.

   function PA_Get_Stream_Read_Available (Stream : PA_Stream_Ptr)
                                          return IC.long
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_GetStreamReadAvailable";
   --  Retrieve the number of frames that can be read from the stream without
   --  waiting.
   --
   --  @return Returns a non-negative value representing the maximum number of
   --  frames that can be read from the stream without blocking or busy waiting
   --  or, a PaErrorCode (which are always negative) if PortAudio is not
   --  initialized or an error is encountered.

   function PA_Get_Stream_Write_Available (Stream : PA_Stream_Ptr)
                                           return IC.long
     with
       Import => True,
       Convention => C,
       Link_Name => "PA_GetStreamWriteAvailable";
   --  Retrieve the number of frames that can be written to the stream without
   --  waiting.
   --
   --  @return Returns a non-negative value representing the maximum number of
   --  frames that can be written to the stream without blocking or busy waiting
   --  or, a PaErrorCode (which are always negative) if PortAudio is not
   --  initialized or an error is encountered.

end PortAudioAda;
