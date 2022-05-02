with Ada.Text_IO;


package body Properties_Test is
    procedure Initialize (T : in out Test) is
    begin
        Set_Name (T, "Properties Ada Test");
        Ahven.Framework.Add_Test_Routine(T,
                                         Test_Properties_Format'Access,
                                         "Test_Properties_Format");
    end Initialize;

    procedure Test_Properties_Format is
        Test_String : String := "Hello world";
    begin
        Ada.Text_IO.Put_Line(Index(Test_String, "A", 0)'Image);
    end Test_Properties_Format;
end Properties_Test;
