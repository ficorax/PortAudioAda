with Glib; use Glib;
with Gdk.Event; use Gdk.Event;

with Gtk.Widget, Gtk.Handlers; use Gtk.Widget, Gtk.Handlers;

package GA_Waves_Package is

   package Handlers is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Widget_Record);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record,
      Return_Type => Boolean);

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   procedure Destroy (Widget : access Gtk_Widget_Record'Class);

end GA_Waves_Package;
