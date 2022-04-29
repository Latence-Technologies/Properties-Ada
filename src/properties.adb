
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Properties is

    procedure Save(File_Path  : String;
                   Properties : Map_Type) is
        File : File_Type;
    begin
        Create(File, Out_File, File_Path);
    
        for Local_Cursor in Properties.Iterate loop
            declare
                Line : Unbounded_String := Property_Map.Key(Local_Cursor) & "=" & Property_Map.Element(Local_Cursor);
            begin
            
            Ada.Strings.Unbounded.Text_IO.Put_Line(File, Line);
            end;
        end loop;
        
        Close(File);
    end Save;

    function Load(File_Path : String) return Map_Type is
        File : File_Type;
        Map : Map_Type;
        Line : Unbounded_String;
        Key_Value_End : Natural;
        Value_Delimiter : Natural;
    begin
        Open(File, In_File, File_Path);

        loop
            exit when End_Of_File(File);
            Get_Line(File, Line);
            Key_Value_End := Index(Line, "#", 1);

            if Key_Value_End = 0 then
                Key_Value_End := Length(Line);
            elsif Key_Value_End = 1 then
                goto Continue;
            else
                Key_Value_End := Key_Value_End - 1;
            end if;

            Value_Delimiter := Index(Line, "=", Key_Value_End, Ada.Strings.Backward);

            if Value_Delimiter = 0 then
                goto Continue;
            end if;

            declare
                Key : Unbounded_String := Unbounded_Slice(Line, 1, Value_Delimiter - 1);
                Value : Unbounded_String := Unbounded_Slice(Line, Value_Delimiter + 1, Key_Value_End);
            begin
                Trim(Key, Ada.Strings.Both);
                Trim(Value, Ada.Strings.Both);
                Map.Include(Key, Value);
            end;
            <<Continue>>
        end loop;
        Close(File);

        return Map;
    end Load;
end Properties;