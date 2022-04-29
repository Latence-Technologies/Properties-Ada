
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

--
-- Package to load and save .properties files
--
package Properties is

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
    -- @param File_Path path of the file
    -- @param Properties map of properties to save
    --
    procedure Save(File_Path  : String;
                   Properties : Map_Type);

    --
    -- Loads the content of a .properties file into a Property_Map
    --
    -- @param File_Path path to the file to load
    -- @return content of the properties file as a map of property
    -- @raises Ada.IO_Exceptions.NAME_ERROR if the file does not exist
    --
    function Load(File_Path : String) return Map_Type;
end Properties;