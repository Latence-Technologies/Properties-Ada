
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
        Value_Delimiter : Natural;
    begin
        Open(File, In_File, File_Path);

        loop
            exit when End_Of_File(File);
            Get_Line(File, Line);
            Trim(Line, Ada.Strings.Left);

            Ada.Text_IO.Put_Line("Reading line " & (+Line));

            -- if line is a comment
            -- note: a comment may not be on the same line as a key value pair
            -- e.g. key=value # comment
            -- is not valid. This behavior is described by
            -- https://en.wikipedia.org/wiki/.properties and is shared by the
            -- Ada Utility library.
            if Element(Line, 1) = '#' or else Element(Line, 1) = '!' then
                goto Continue;
            end if;

            Ada.Text_IO.Put_Line("Not a comment");

            Value_Delimiter := Find_Delimiter(Line);

            if Value_Delimiter = 0 then
                Map.Include(Line, +"");
                goto Continue;
            end if;

            Ada.Text_IO.Put_Line("Has value");

            declare
                Key : Unbounded_String := Unbounded_Slice(Line, 1, Value_Delimiter - 1);
                Value : Unbounded_String := Unbounded_Slice(Line, Value_Delimiter + 1, Length(Line));
            begin
                Key := Unescape(Unescaped_Right_Trim(Key));
                Trim(Value, Ada.Strings.Left);

                Ada.Text_IO.Put_Line("Unescaped key");
                while Is_Multiline(Value) loop
                    Ada.Text_IO.Put_Line(+Value & " is multiline");

                    if End_Of_File(File) then
                        raise Syntax_Error with "Multiline property at the end of the file";
                    end if;
                    Get_Line(File, Line);
                    Trim(Line, Ada.Strings.Left);
                    -- remove final \ character and append next line
                    Value := Unbounded_Slice(Value, 1, Length(Value) - 1) & Line;
                end loop;

                Value := Unescape(Value);
                Ada.Text_IO.Put_Line("Unescaped value");

                Map.Include(Key, Value);
            end;
            <<Continue>>
        end loop;
        Close(File);

        return Map;
    exception
        when others =>
            Close(File);
            raise;
    end Load;

    function Unescaped_Right_Trim(Line : Unbounded_String) return Unbounded_String is
        use type Ada.Strings.Trim_End;

        EndIndex : Natural := Length(Line);
    begin
        while Element(Line, EndIndex) = ' '
            and then (EndIndex = 1 or else Element(Line, EndIndex - 1) /= '\') loop
            EndIndex := EndIndex - 1;
        end loop;

        return Unbounded_Slice(Line, 1, EndIndex);
    end Unescaped_Right_Trim;

    function Find_Delimiter(Line : Unbounded_String) return Natural is
        Equal_Pos : Natural := 0;
        Colon_Pos : Natural := 0;
        Space_Pos : Natural := 0;
    begin
        loop -- find the first non escaped equal
            Equal_Pos := Index(Line, "=", Equal_Pos + 1, Ada.Strings.Forward);
            exit when Equal_Pos = 0;

            if Equal_Pos = 1 then
                raise Syntax_Error with "Line starts with =, key must not be empty. You may escape = characters with \=";
            end if;

            exit when Element(Line, Equal_Pos - 1) /= '\';
        end loop;

        loop -- find the first non escaped colon
            Colon_Pos := Index(Line, ":", Colon_Pos + 1, Ada.Strings.Forward);
            exit when Colon_Pos = 0;

            if Colon_Pos = 1 then
                raise Syntax_Error with "Line starts with :, key must not be empty. You may escape : characters with \:";
            end if;

            exit when Element(Line, Colon_Pos - 1) /= '\';
        end loop;

        loop -- find the first non escaped space (only used if other delimiters are not used)
            Space_Pos := Index(Line, " ", Space_Pos + 1, Ada.Strings.Forward);
            exit when Space_Pos = 0;

            if Space_Pos = 1 then
                -- should not be possible because of Trim
                raise Syntax_Error with "Internal error parsing .properties file, encountering space as first character after trim (should not be possible)";
            end if;

            exit when Element(Line, Space_Pos - 1) /= '\';
        end loop;

        if Equal_Pos = 0 and Colon_Pos /= 0 then
            return Colon_Pos;
        elsif Colon_Pos = 0 and Equal_Pos /= 0 then
            return Equal_Pos;
        elsif Equal_Pos /= 0 and Colon_Pos /= 0 then -- take the first
            return Natural'Min(Colon_Pos, Equal_Pos);
        else -- both Equal_Pos and Colon_Pos are 0
            return Space_Pos; -- use space as delimiter or 0 if no value (valid in .properties format)
        end if;
    end Find_Delimiter;

    function Unescape(Input : Unbounded_String) return Unbounded_String is
        Index : Positive := 1;
        Value : Unbounded_String := Input;
    begin
        while Index <= Length(Value) loop
            if Element(Value, Index) = '\' then
                Value := Delete(Value, Index, Index);
            else -- no need to increment if we deleted a char
                Index := Index + 1;
            end if;
        end loop;
        return Value;
    end Unescape;

    function Is_Multiline(Value : Unbounded_String) return Boolean is
        Escape_Count : Natural := 0;
        Index : Natural := Length(Value);
    begin
        while Index >= 1 and then Element(Value, Index) = '\' loop
            Escape_Count := Escape_Count + 1;
            Index := Index - 1;
        end loop;

        return Escape_Count mod 2 = 1; -- odd number of escape characters at the end
    end Is_Multiline;
end Properties;