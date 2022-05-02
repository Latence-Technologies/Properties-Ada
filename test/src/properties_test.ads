with Ahven.Framework;

package Properties_Test is
    type Test is new Ahven.Framework.Test_Case with null record;
    procedure Initialize (T : in out Test);
private
    procedure Test_Properties_Format;
end Properties_Test;
