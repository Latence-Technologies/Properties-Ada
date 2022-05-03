with Ahven.Framework;

--
-- Execute tests on the Properties package
--
package Properties_Test is
    type Test is new Ahven.Framework.Test_Case with null record;
    procedure Initialize (T : in out Test);
private
    procedure Test_Properties_Load;
    procedure Test_Properties_Save;
end Properties_Test;
