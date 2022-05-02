# Properties-Ada

Properties Ada is a lightweight simplistic Ada library for parsing .properties 
files. It currently support most of .properties features such as

  - `#` and `!` line comments
  - `=`, `:` and space delimiters
  - Multiline values ending with `\` and escaped `\` characters (`\\`)
  - `\n`, `\r` and `\t` values
  - Empty values

However, it does not support escaped unicode characters in the form of`\u0000`.

## Install

To install Properties-Ada, use the following commands:

```ada
gprbuild -p properties_ada.gpr
gprinstall -p -f properties_ada.gpr
```

## Run tests

If you want to run the unit tests, run the following commands:

```ada
gprbuild -p test/properties_ada_test.gpr
bin/test_runner
```


## Usage

### Loading a .properties file

The following code snippet describe how to load a `.properties` file and extract
its data. Note that `Properties.Map_Type` is actually an 
`Ada.Containers.Hashed_Maps` that maps `Unbounded_String` to `Unbounded_String`
underneath. Therefore you may interact with the map without using the provided 
`Get` and `Set`. Those are helper methods meant to make the usage of the library
with `String` easier. If you want to retrieve the amount of entries in the map
or iterate through the keys however, you will have to use functionalities of
`Ada.Containers.Hashed_Maps`.

```ada
with Ada.Text_IO;
with Properties;

procedure Main is
    Map : Properties.Map_Type;
begin
    Map := Properties.Load("your_file.properties");
    
    Ada.Text_IO.Put_Line("key1 = """ & Properties.Get(Map, "key1") & """");
    Ada.Text_IO.Put_Line("key2 = """ & Properties.Get(Map, "key2") & """");
end Main;

```

### Saving a .properties file

The following code snippet describes how to populate a map and write it into
a `.properties` file.

```ada
with Properties;

procedure Main is
    Map : Properties.Map_Type;
begin
    Properties.Set(Map, "key", "value");
    Properties.Set(Map, "hello", "world");
    Properties.Set(Map, "escaped=characters", "are:fully\supported");

    Properties.Save(Map, "saved.properties");
end Main;
```


## Latence Technologies

This Ada library is offered by LatenceTech, a Montreal based startup specialized 
in low-latency optimization. Our website is https://latencetech.com/