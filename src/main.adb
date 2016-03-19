with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;

procedure Main is

   package Commands is
      type Command is (Unknown, Show_Help, Show_Result, Create_New_File, Replace_File);
      package IO is new Ada.Text_IO.Enumeration_IO (Command);
   end;

   function Get_Command (Name : String) return Commands.Command is
   begin
      if Name = "help" then
         return Commands.Show_Help;
      elsif Name = "show" then
         return Commands.Show_Result;
      elsif Name = "create" then
         return Commands.Create_New_File;
      elsif Name = "replace" then
         return Commands.Replace_File;
      else
         return Commands.Unknown;
      end if;
   end;

   function Get_Command_Help (X : Commands.Command) return String is
   begin
      case X is
         when Commands.Show_Result =>
            return "show    <Scale> <File_Name>";
         when Commands.Create_New_File =>
            return "create  <Scale> <File_Name> <File_Name>";
         when Commands.Replace_File =>
            return "replace <Scale> <File_Name>";
         when others =>
            return "";
      end case;
   end;

   type Vector is array (Integer range <>) of Float;

   procedure Read (Name : String; X : out Vector; Last : out Integer; Min, Max : in out Float) is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
      F : File_Type;
   begin
      Open (F, In_File, Name);
      Last := X'First - 1;
      loop
         exit when End_Of_File (F);
         Last := Last + 1;
         Get (F, X (Last));
         Min := Float'Min (X (Last), Min);
         Max := Float'Max (X (Last), Max);
         exit when Last = X'Last;
      end loop;
      Close (F);
   end;

   procedure Write (Name : String; X : Vector) is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
      F : File_Type;
   begin
      Create (F, Out_File, Name);
      for I in X'Range loop
         Put (F, X (I));
         New_Line (F);
      end loop;
      Close (F);
   end;

   procedure Put (X : Vector) is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
      use Ada.Integer_Text_IO;
   begin
      for I in X'Range loop
         Put (I, 4);
         Put (" ");
         Put (X (I), 3, 3, 0);
         New_Line;
      end loop;
   end;

   function Normalize (Value : Float; Min, Max : Float) return Float is
   begin
      return (Value - Min) / (Max - Min);
   end;

   procedure Normalize (Min : Float; Max : Float; Scale : Float; Result : in out Vector) is
   begin
      for E of Result loop
         E := Normalize (E, Min, Max) * Scale;
      end loop;
   end;

   procedure Help is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Commands;
      use Commands.IO;
   begin
      for I in Command loop
         Put (I'Enum_Rep, 3);
         Put (" ");
         Put (I, Command'Width + 1);
         Put (Get_Command_Help (I));
         New_Line;
      end loop;
   end;

   X : Vector (1 .. 10000);
   Last : Integer;
   Min : Float := Float'Last;
   Max : Float := Float'First;

begin

   case Get_Command (Ada.Command_Line.Argument (1)) is
      when Commands.Show_Result =>
         declare
            use Ada.Text_IO;
            use Ada.Float_Text_IO;
            use Ada.Command_Line;
         begin
            if Argument_Count < 3 then
               raise Program_Error with "Argument_Count < 3";
            end if;
            Read (Argument (3), X, Last, Min, Max);
            Put (X (X'First .. Last));
            New_Line;
            Put ("Min ");
            Put (Min, 3, 3, 0);
            New_Line;
            Put ("Max ");
            Put (Max, 3, 3, 0);
            New_Line (2);
            Put_Line ("Normalize ");
            Normalize (Min, Max, Float'Value (Argument (2)), X (X'First .. Last));
            Put (X (X'First .. Last));
         end;

      when Commands.Create_New_File =>
         declare
            use Ada.Command_Line;
         begin
            if Argument_Count < 4 then
               raise Program_Error with "Argument_Count < 4";
            end if;
            Read (Argument (3), X, Last, Min, Max);
            Normalize (Min, Max, Float'Value (Argument (2)), X (X'First .. Last));
            Write (Argument (4), X (X'First .. Last));
         end;

      when Commands.Replace_File =>
         declare
            use Ada.Command_Line;
         begin
            if Argument_Count < 3 then
               raise Program_Error with "Argument_Count < 4";
            end if;
            Read (Argument (3), X, Last, Min, Max);
            Normalize (Min, Max, Float'Value (Argument (2)), X (X'First .. Last));
            Write (Argument (3), X (X'First .. Last));
         end;

      when Commands.Show_Help =>
         Help;

      when others =>
         declare
            use Ada.Text_IO;
         begin
            Put_Line ("Unsupported command");
            Help;
         end;

   end case;

end;

