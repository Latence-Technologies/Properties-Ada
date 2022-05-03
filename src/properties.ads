
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

--
-- Package to interact with .properties files. Offers functionalities for 
-- saving, loading, querying and editing .properties files.
--
package Properties is

    --
    -- Thrown when a given .properties file has an invalid syntax
    --
    Syntax_Error : exception;

    package Property_Map is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Unbounded_String,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");

    subtype Map_Type is Property_Map.Map;

    --
    -- Saves the specified .properties file from a Property_Map. If the file
    -- does not exist it is created, otherwise it is overwritten.
    --
    -- @param Properties map of properties to save
    -- @param File_Path path of the file
    --
    procedure Save(Properties : Map_Type;
                   File_Path  : String);

    --
    -- Loads the content of a .properties file into a Property_Map
    --
    -- @param File_Path path to the file to load
    -- @return content of the properties file as a map of property
    -- @raises Ada.IO_Exceptions.NAME_ERROR if the file does not exist
    -- @raises Syntax_Error if the properties file has an invalid syntax
    --
    function Load(File_Path : String) return Map_Type;

    --
    -- Retrieves the number of entries in the map
    --
    -- @param Map map to retrive the number of entries from
    --
    function Count(Map : Map_Type) return Integer;

    --
    -- Checks if the map contains the specified key
    --
    -- @param Map map to query
    -- @param Key key to check for existence
    --
    function Contains(Map : Map_Type; Key : Unbounded_String) return Boolean;

    --
    -- Checks if the map contains the specified key
    --
    -- @param Map map to query
    -- @param Key key to check for existence
    --
    function Contains(Map : Map_Type; Key : String) return Boolean;

    --
    -- Retrieves the value associated to the specified key in the map
    --
    -- @param Map map to query
    -- @param Key key associated to value
    -- @return value associated to the key
    -- @raises Constraint_Error if the key is not in the map
    --
    function Get(Map : Map_Type; Key : Unbounded_String) return Unbounded_String;

    --
    -- Sets the specified key to the specified value in the properties map. If
    -- the key is already present, overrides it
    --
    -- @param Map map to query
    -- @param Key key to set
    -- @param Value value to set
    --
    procedure Set(Map : in out Map_Type; Key : Unbounded_String; Value : Unbounded_String);

    --
    -- Retrieves the value associated to the specified key in the map
    --
    -- @param Map map to query
    -- @param Key key associated to value
    -- @return value associated to the key
    -- @raises Constraint_Error if the key is not in the map
    --
    function Get(Map : Map_Type; Key : String) return String;

    --
    -- Sets the specified key to the specified value in the properties map. If
    -- the key is already present, overrides it
    --
    -- @param Map map to query
    -- @param Key key to set
    -- @param Value value to set
    --
    procedure Set(Map : in out Map_Type; Key : String; Value : String);

    --
    -- Retrieves the value associated to the specified key in the map
    --
    -- @param Map map to query
    -- @param Key key associated to value
    -- @return value associated to the key
    -- @raises Constraint_Error if the key is not in the map
    --
    function Get(Map : Map_Type; Key : String) return Unbounded_String;

    --
    -- Sets the specified key to the specified value in the properties map. If
    -- the key is already present, overrides it
    --
    -- @param Map map to query
    -- @param Key key to set
    -- @param Value value to set
    --
    procedure Set(Map : in out Map_Type; Key : String; Value : Unbounded_String);

    --
    -- Retrieves the value associated to the specified key in the map
    --
    -- @param Map map to query
    -- @param Key key associated to value
    -- @return value associated to the key
    -- @raises Constraint_Error if the key is not in the map
    --
    function Get(Map : Map_Type; Key : Unbounded_String) return String;

    --
    -- Sets the specified key to the specified value in the properties map. If
    -- the key is already present, overrides it
    --
    -- @param Map map to query
    -- @param Key key to set
    -- @param Value value to set
    --
    procedure Set(Map : in out Map_Type; Key : Unbounded_String; Value : String);
private
    function Unescaped_Right_Trim(Line : Unbounded_String) return Unbounded_String;

    function Find_Delimiter(Line : Unbounded_String) return Natural;

    function Escape(Input : Unbounded_String) return Unbounded_String;

    function Unescape(Input : Unbounded_String) return Unbounded_String;

    function Is_Multiline(Value : Unbounded_String) return Boolean;

    function "+"(Value : String) return Unbounded_String renames To_Unbounded_String;
    function "+"(Value : Unbounded_String) return String renames To_String;
end Properties;