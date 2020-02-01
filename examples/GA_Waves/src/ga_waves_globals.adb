package body GA_Waves_Globals is

   --------------------
   -- Toggle_Options --
   --------------------

   procedure Toggle_Options (started : Boolean) is
   begin
      for i in Enum_Options loop
         declare
            option : Option_Use renames options (i).optionUse;
            widget : access Gtk_Widget_Record'Class renames
                       options (i).optionWidget;
         begin
            if started and then option = OU_Before then
               Set_Sensitive (widget, False);
            else
               Set_Sensitive (widget, True);
            end if;
         end;
      end loop;
   end Toggle_Options;

end GA_Waves_Globals;
