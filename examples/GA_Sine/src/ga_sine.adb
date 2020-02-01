with Glib; use Glib;

with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Button; use Gtk.Button;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Table; use Gtk.Table;
with Gtk.Window; use Gtk.Window;

with PortAudioAda; use PortAudioAda;

with GA_Sine_Callbacks; use GA_Sine_Callbacks;
with GA_Sine_Globals; use GA_Sine_Globals;
with GA_Sine_Package; use GA_Sine_Package;

procedure GA_Sine
is
   window : Gtk_Window;
   table  : Gtk.Table.Gtk_Table;
   label  : Gtk.Label.Gtk_Label;
   spin   : Gtk.Spin_Button.Gtk_Spin_Button;
   button : Gtk.Button.Gtk_Button;
   adjust : Gtk.Adjustment.Gtk_Adjustment;
   scale  : Gtk.Scale.Gtk_Hscale;
   combo  : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;

   err    : PA_Error;

begin

   err := PA_Initialize;

   if err /= paNoError then
      raise PortAudio_Exception with PA_Get_Error_Text (err);
   end if;

   Init;
   Gtk_New (window);
   Set_Title (window, "PortAudio Test: Sine");
   Set_Size_Request (window, -1, -1);

   Return_Handlers.Connect
     (window,
      "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access));

   Handlers.Connect (window,
                     "destroy",
                     Handlers.To_Marshaller (Destroy'Access));

   -----------------------------------------------------------------------------

   Gtk_New (table, 4, 3, True);
   Set_Row_Spacings (table, 4);
   Set_Col_Spacings (table, 10);

   --  Labels

   Gtk_New (label, "Frames per buffer");
   Attach (table, label, 0, 1, 0, 1);

   Gtk_New (label, "Sample rate");
   Attach (table, label, 0, 1, 1, 2);

   Gtk_New (label, "Amplitude");
   Attach (table, label, 0, 1, 2, 3);

   Gtk_New (label, "Frequency");
   Attach (table, label, 0, 1, 3, 4);

   --  Controls

   Gtk_New (spin, 0.0, 1024.0, 16.0);
   Attach (table, spin, 1, 2, 0, 1);
   Set_Alignment (spin, 1.0);
   Set_Value (spin, Gdouble (Frames_Per_Buffer));
   SpinHand.Connect
     (spin,
      "value_changed",
      SpinHand.To_Marshaller (Frames_Per_Buffer_Callback'Access));
   options (EO_frames_Per_Buffer).optionWidget := spin;
   options (EO_frames_Per_Buffer).optionUse    := OU_Before;

   Gtk_New (combo);
   Attach (table, combo, 1, 2, 1, 2);
   for i in sample_Rates'Range loop
      Append_Text (combo, sample_Rates (i).txtRate);
   end loop;
   Set_Active (combo, 8);
   CombHand.Connect
     (combo,
      "changed",
      CombHand.To_Marshaller (Sample_Rate_Callback'Access));
   options (EO_Sample_Rate).optionWidget := combo;
   options (EO_Sample_Rate).optionUse    := OU_Before;

   Gtk_New (adjust, 0.0, 0.0, 1.0, 0.01, 0.1);
   Gtk_New_Hscale (scale, adjust);
   Attach (table, scale, 1, 2, 2, 3);
   Set_Digits (scale, 2);
   Set_Value (scale, Gdouble (gUserData.amplitude));
   ScaleHand.Connect
     (scale,
      "value_changed",
      ScaleHand.To_Marshaller (Amplitude_Callback'Access));
   options (EO_Amplitude).optionWidget := spin;
   options (EO_Amplitude).optionUse    := OU_Before;

   Gtk_New (adjust, 0.0, 16.351598, 8372.018085, 0.01, 10.0);
   Gtk_New_Hscale (scale, adjust);
   Set_Digits (scale, 2);
   Attach (table, scale, 1, 2, 3, 4);
   Set_Value (scale, Gdouble (gUserData.frequency));
   ScaleHand.Connect
     (scale,
      "value_changed",
      ScaleHand.To_Marshaller (Frequency_Callback'Access));
   options (EO_Frequency).optionWidget := spin;
   options (EO_Frequency).optionUse    := OU_Before;

   --  Buttons

   Gtk_New (button, "Start");
   Attach (table, button, 2, 3, 0, 1);
   Handlers.Connect (button,
                     "clicked",
                     Handlers.To_Marshaller (Start_Callback'Access));

   Gtk_New (button, "Stop");
   Attach (table, button, 2, 3, 1, 2);
   Handlers.Connect (button,
                     "clicked",
                     Handlers.To_Marshaller (Stop_Callback'Access));

   Gtk_New (button, "Quit");
   Attach (table, button, 2, 3, 3, 4);

   Handlers.Connect (button,
                     "clicked",
                     Handlers.To_Marshaller (Destroy'Access));

   -----------------------------------------------------------------------------

   Add (window, table);

   Show_All (window);

   Main;

   err := PA_Terminate;

   if err /= paNoError then
      raise PortAudio_Exception with PA_Get_Error_Text (err);
   end if;

end GA_Sine;
