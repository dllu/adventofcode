with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;

procedure Day01a is

procedure Usage is
    Prog_Name : String := Ada.Command_Line.Command_Name;
begin
    Ada.Text_IO.Put_Line("usage: " & Prog_Name & " <file>");
    GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Process(Filename : String) return Integer is
    Input_File : Ada.Text_IO.File_Type;
    Digit_Array : constant array (0..9) of String(1..1) :=
        ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
    Number : Integer := 0;
    Line : Ada.Strings.Unbounded.Unbounded_String;
    Last, Sum : Natural := 0;
    Current_Digit, Left_Digit, Right_Digit : String(1..1);
    Min_Index, Max_Index, Left_Index, Right_Index, Temp_Index : Natural := 0;
begin
    Ada.Text_IO.Open(File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Filename);
    while not (Ada.Text_IO.End_Of_File(Input_File)) loop
        Line := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(Input_File));
        Min_Index := Natural'Last;
        Max_Index := Natural'First;
        Left_Index := 0;
        Right_Index := 0;
        for I in Digit_Array'First..Digit_Array'Last loop
            Current_Digit := Digit_Array(I);
            Left_Index := Ada.Strings.Unbounded.Index(Line, Current_Digit, 1);
            if Left_Index /= 0 and Left_Index < Min_Index then
                Min_Index := Left_Index;
                Left_Digit := Current_Digit;
            end if;
        end loop;
        for I in Digit_array'First..Digit_Array'Last loop
            Current_Digit := Digit_Array(I);
            Temp_Index := 0;
            loop
                Temp_Index := Ada.Strings.Unbounded.Index(Line, Current_Digit, Temp_Index + 1);
                exit when Temp_Index = 0 or Temp_Index = Ada.Strings.Unbounded.Length(Line);
                Right_Index := Temp_Index;
            end loop;
            if Right_Index > Max_Index then
                Max_Index := Right_Index;
                Right_Digit := Current_Digit;
            end if;
        end loop;
        Number := Integer'Value(Left_Digit & Right_Digit);
        Sum := Sum + Number;
    end loop;
    Ada.Text_IO.Close(Input_File);
    return Sum;
end Process;

    Result : Integer := 0;
begin
    if Ada.Command_Line.Argument_Count < 1 then
        Usage;
    end if;
    Result := Process(Ada.Command_Line.Argument(1));
    Ada.Text_IO.Put_Line("result =" & Integer'Image(Result));
end Day01a;
