with Gtk.Button; use Gtk.Button;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Handlers;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Widget; use Gtk.Widget;

package GA_Waves_Callbacks is

   package SpinHand is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Spin_Button_Record);

   procedure Frames_Per_Buffer_Callback
     (widget : access Gtk_Spin_Button_Record'Class);

   package CombHand is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Combo_Box_Record);

   procedure Sample_Rate_Callback
     (widget : access Gtk_Combo_Box_Record'Class);

   procedure Wave_Type_Callback
     (widget : access Gtk_Combo_Box_Record'Class);

   package ScaleHand is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Hscale_Record);

   procedure Amplitude_Callback
     (widget : access Gtk_Hscale_Record'Class);

   procedure Frequency_Callback
     (widget : access Gtk_Hscale_Record'Class);

   package SpinBtn is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Button_Record);

   procedure Start_Callback
     (widget : access Gtk_Widget_Record'Class);

   procedure Stop_Callback
     (widget : access Gtk_Widget_Record'Class);

end GA_Waves_Callbacks;
