
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories;

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Properties; use Properties;

package body Properties_Test is
    procedure Initialize (T : in out Test) is
    begin
        Set_Name (T, "Properties Ada Test");
        Ahven.Framework.Add_Test_Routine(T,
                                         Test_Properties_Load'Access,
                                         "Test_Properties_Load");
    end Initialize;

    function "+"(Value : String) return Unbounded_String renames To_Unbounded_String;
    function "+"(Value : Unbounded_String) return String renames To_String;

    procedure Assert_Equal is new Ahven.Assert_Equal (Data_Type => Unbounded_String,
                                                      Image     => To_String);

    procedure Assert_Equal is new Ahven.Assert_Equal (Data_Type => Integer,
                                                      Image     => Integer'Image);

    procedure Test_Properties_Load is
        File_Path : constant String := "tmp.properties";

        Test_Properties_Content : constant String := "# You are reading a comment in "".properties"" file." & LF &
                                                     "! The exclamation mark can also be used for comments." & LF &
                                                     "# Lines with ""properties"" contain a key and a value separated by a delimiting character." & LF &
                                                     "# There are 3 delimiting characters: '=' (equal), ':' (colon) and ' ' (space)." & LF &
                                                     "website = https://en.wikipedia.org/" & LF &
                                                     "language : English" & LF &
                                                     "topic .properties files" & LF &
                                                     "# A word on a line will just create a key with no value." & LF &
                                                     "empty" & LF &
                                                     "# White space that appears between the key, the value and the delimiter is ignored." & LF &
                                                     "# This means that the following are equivalent (other than for readability)." & LF &
                                                     "hello=hello" & LF &
                                                     "hello = hello" & LF &
                                                     "# Keys with the same name will be overwritten by the key that is the furthest in a file." & LF &
                                                     "# For example the final value for ""duplicateKey"" will be ""second""." & LF &
                                                     "duplicateKey = first" & LF &
                                                     "duplicateKey = second" & LF &
                                                     "# To use the delimiter characters inside a key, you need to escape them with a \." & LF &
                                                     "# However, there is no need to do this in the value." & LF &
                                                     "delimiterCharacters\:\=\ = This is the value for the key ""delimiterCharacters\:\=\ """ & LF &
                                                     "# Adding a \ at the end of a line means that the value continues to the next line." & LF &
                                                     "multiline = This line \" & LF &
                                                     "continues" & LF &
                                                     "spaceending = This line does not continue \ " & LF &
                                                     "notcontinue" & LF &
                                                     "# If you want your value to include a \, it should be escaped by another \." & LF &
                                                     "path = c:\\wiki\\templates" & LF &
                                                     "# This means that if the number of \ at the end of the line is even, the next line is not included in the value." & LF &
                                                     "# In the following example, the value for ""evenKey"" is ""This is on one line\""." & LF &
                                                     "evenKey = This is on one line\\" & LF &
                                                     "# This line is a normal comment and is not included in the value for ""evenKey""" & LF &
                                                     "# If the number of \ is odd, then the next line is included in the value." & LF &
                                                     "# In the following example, the value for ""oddKey"" is ""This is line one and\#This is line two""." & LF &
                                                     "oddKey = This is line one and\\\" & LF &
                                                     "# This is line two" & LF &
                                                     "# White space characters are removed before each line." & LF &
                                                     "# Make sure to add your spaces before your \ if you need them on the next line." & LF &
                                                     "# In the following example, the value for ""welcome"" is ""Welcome to Wikipedia!""." & LF &
                                                     "welcome = Welcome to \" & LF &
                                                     "          Wikipedia!" & LF &
                                                     "# If you need to add newlines and carriage returns, they need to be escaped using \n and \r respectively." & LF &
                                                     "# You can also optionally escape tabs with \t for readability purposes." & LF &
                                                     "valueWithEscapes = This is a newline\n and a carriage return\r and a tab\t." & LF &
                                                     "# You can also use Unicode escape characters (maximum of four hexadecimal digits)." & LF &
                                                     "# In the following example, the value for ""encodedHelloInJapanese"" is ""こんにちは""." & LF &
                                                     -- "encodedHelloInJapanese = \u3053\u3093\u306b\u3061\u306f" & LF &
                                                     "# But with more modern file encodings like UTF-8, you can directly use supported characters." & LF &
                                                     "helloInJapanese = こんにちは" & LF;
        Map : Properties.Map_Type;
    begin
        declare
            File : File_Type;
        begin
            Create(File, Out_File, File_Path);
            Ada.Text_IO.Put_Line(File, Test_Properties_Content);
            Close(File);
        end;
        Map := Properties.Load(File_Path);

        Assert_Equal(Integer(Properties.Property_Map.Length(Map)), 16, "Loaded properties map has incorrect size");
        Assert_Equal(Properties.Property_Map.Element(Map, +"website"), +"https://en.wikipedia.org/", "Incorrect properties map parsing for equal property");
        Assert_Equal(Properties.Property_Map.Element(Map, +"language"), +"English", "Incorrect properties map parsing for colon property");
        Assert_Equal(Properties.Property_Map.Element(Map, +"topic"), +".properties files", "Incorrect properties map parsing for space property");
        Assert_Equal(Properties.Property_Map.Element(Map, +"empty"), +"", "Incorrect properties map parsing for empty property");
        Assert_Equal(Properties.Property_Map.Element(Map, +"hello"), +"hello", "Incorrect trimming of properties");
        Assert_Equal(Properties.Property_Map.Element(Map, +"duplicateKey"), +"second", "Incorrect duplicate key parsing");
        Assert_Equal(Properties.Property_Map.Element(Map, +"delimiterCharacters:= "), +"This is the value for the key ""delimiterCharacters:= """, "Incorrect delimiter character escaping");
        Assert_Equal(Properties.Property_Map.Element(Map, +"multiline"), +"This line continues", "Incorrect multiline handling");
        Assert_Equal(Properties.Property_Map.Element(Map, +"spaceending"), +"This line does not continue  ", "Incorrect escaped space character handling");
        Assert_Equal(Properties.Property_Map.Element(Map, +"notcontinue"), +"", "Incorrect escaped space character handling");
        Assert_Equal(Properties.Property_Map.Element(Map, +"path"), +"c:\wiki\templates", "Incorrect handling of escaped backslash");
        Assert_Equal(Properties.Property_Map.Element(Map, +"evenKey"), +"This is on one line\", "Incorrect handling of even number of multiple backslash character at the end of the line");
        Assert_Equal(Properties.Property_Map.Element(Map, +"oddKey"), +"This is line one and\# This is line two", "Incorrect handling of odd number of multiple backslash character at the end of the line");
        Assert_Equal(Properties.Property_Map.Element(Map, +"welcome"), +"Welcome to Wikipedia!", "Incorrect trimming of spaces at the beginning of additional lines");
        Assert_Equal(Properties.Property_Map.Element(Map, +"valueWithEscapes"), +"This is a newline" & LF & " and a carriage return" & CR & " and a tab" & HT & ".", "Incorrect parsing of \n, \r and \t characters");
        -- Assert_Equal(Properties.Property_Map.Element(Map, +"encodedHelloInJapanese"), +"こんにちは", "Incorrect parsing of unicode escape characters");
        Assert_Equal(Properties.Property_Map.Element(Map, +"helloInJapanese"), +"こんにちは", "Failure to parse UTF-8 japanese characters");


        Ada.Directories.Delete_File(File_Path);
    exception
        when others =>
            Ada.Directories.Delete_File(File_Path);
            raise;
    end Test_Properties_Load;

    procedure Test_Properties_Save is
        File_Path : constant String := "tmp2.properties";
        Map : Properties.Map_Type;
    begin
        Properties.Save(File_Path, Map);
        Ada.Directories.Delete_File(File_Path);
    end Test_Properties_Save;
end Properties_Test;
