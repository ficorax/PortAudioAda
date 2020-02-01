with PortAudioAda.Thin; use PortAudioAda.Thin;

generic
   type Frame;
   type User_Data is private;
   with function Pre_Processing (frameIn : Frame;
                                 data    : User_Data) return Frame;
   with function Post_Processing (frameIn : Frame;
                                  data    : User_Data) return Frame;
package Generic_Callback is

   type Pre_Callback is access function (frameIn : Frame;
                                         data    : User_Data) return Frame;

   type Post_Callback is access function (frameIn : Frame;
                                          data    : User_Data) return Frame;

   function PAA_Open_Stream (stream           : access PaStream_Ptr;
                             inputParameters  : access PaStreamParameters;
                             outputParameters : access PaStreamParameters;
                             sampleRate       :        Long_Float;
                             framesPerBuffer  :        Long_Integer;
                             streamFlags      :        PaStreamFlags;
                             pre              :        Pre_Callback;
                             post             :        Post_Callback;
                             userData         :        User_Data)
                            return PaError;

end Generic_Callback;
