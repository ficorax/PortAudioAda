with Gtk.Main;

package body GA_Sine_Package is

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event)
     return Boolean
   is
      pragma Unreferenced (Event);
      pragma Unreferenced (Widget);
   begin
      --  If you return False in the "delete_event" signal handler,
      --  GtkAda will emit the "destroy" signal. Returning True means
      --  you don't want the window to be destroyed. This is useful
      --  for popping up 'are you sure you want to quit?' type
      --  dialogs.

      --  Change True to False and the main window will be destroyed
      --  with a "delete_event".

      return False;
   end Delete_Event;

   --   Another callback
   procedure Destroy (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

end GA_Sine_Package;
