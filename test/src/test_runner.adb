with Ada.Text_IO;

with Ahven.Text_Runner;
with Ahven.Framework;

with Properties_Test;

--
-- Launches the execution of all tests
--
procedure Test_Runner is
    S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite("All");
begin
    Ahven.Framework.Add_Test(S, new Properties_Test.Test);
    Ahven.Text_Runner.Run(S);
end Test_Runner;
