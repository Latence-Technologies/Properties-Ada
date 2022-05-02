
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

    function Find_Delimiter(Line : String) return Narual is
        Equal_Pos : Natural := Line'First;
        Colon_Pos : Natural := Line'First;
        Space_Pos : Natural := Line'First;
    begin
        loop
            Equal_Pos := Index(Line, "=", Equal_Pos, Ada.Strings.Forward);
            exit when Equal_Pos = 0;

            if Equal_Pos = Line'First then
                raise Syntax_Error with "Line starts with =, key must not be empty. You may escape = characters with \=";
            end if;

            exit when Line(Equal_Pos - 1) /= "\";
        end loop;

        loop
            Colon_Pos := Index(Line, ":", Line'First, Ada.Strings.Forward);
            exit when Colon_Pos = 0;

            if Colon_Pos = Line'First then
                raise Syntax_Error with "Line starts with :, key must not be empty. You may escape : characters with \:";
            end if;

            exit when Line(Colon_Pos - 1) /= "\";
        end loop;

        loop
            Colon_Pos := Index(Line, ":", Line'First, Ada.Strings.Forward);
            exit when Colon_Pos = 0;

            if Colon_Pos = Line'First then
                raise Syntax_Error with "Line starts with :, key must not be empty. You may escape : characters with \:";
            end if;

            exit when Line(Colon_Pos - 1) /= "\";
        end loop;


        if Equal_Pos = 0 and Colon_Pos = 0 and Space_Pos = 0 then
            -- no value delimiter, this is a key with no value (valid in .properties format)
            Map.Include(Line, "");
            goto Continue;
        end if;

        if Equal_Pos = 0 then
            Value_Delimiter := Colon_Pos;
        elsif Colon_Pos = 0 then
            Value_Delimiter := Equal_Pos;
        else
            Value_Delimiter := Natural'Min(Colon_Pos, Equal_Pos);
        end if;
    end Find_Delimiter;

    function Load(File_Path : String) return Map_Type is
        File : File_Type;
        Map : Map_Type;
        Line : Unbounded_String;
        Value_Delimiter : Natural;
    begin
        Open(File, In_File, File_Path);

        loop
            exit when End_Of_File(File);
            Get_Line(File, Line);
            Trim(Line, Ada.Strings.Left);

            if Line(Line'First) = "#" then
                goto Continue;
            end if;

            Value_Delimiter := Find_Delimiter(Line);

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