with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;

procedure Day01b is

procedure Usage is
    Prog_Name : String := Ada.Command_Line.Command_Name;
begin
    Ada.Text_IO.Put_Line("usage: " & Prog_Name & " <file>");
    GNAT.OS_Lib.OS_Exit(1);
end Usage;

function T(Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

function Word_To_Number(Word : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
    Word_Array : array(0..9) of Ada.Strings.Unbounded.Unbounded_String :=
        (T("zero"), T("one"), T("two"), T("three"), T("four"), T("five"),
         T("six"), T("seven"), T("eight"), T("nine"));
    Digit_Array : constant array(0..9) of Ada.Strings.Unbounded.Unbounded_String :=
        (T("0"), T("1"), T("2"), T("3"), T("4"), T("5"), T("6"), T("7"),
         T("8"), T("9"));
begin
    for I in Word_Array'First..Word_Array'Last loop
        if Ada.Strings.Unbounded.To_String(Word) = Ada.Strings.Unbounded.To_String(Word_Array(I)) then
            return Digit_Array(I);
        end if;
    end loop;
    return T("none");
end Word_To_Number;

function Process(Filename : String) return Integer is
    Input_File : Ada.Text_IO.File_Type;
    Digit_Array : constant array (0..19) of Ada.Strings.Unbounded.Unbounded_String :=
        (T("0"), T("1"), T("2"), T("3"), T("4"), T("5"), T("6"), T("7"),
         T("8"), T("9"), T("zero"), T("one"), T("two"), T("three"), T("four"),
         T("five"), T("six"), T("seven"), T("eight"), T("nine"));
    Number : Integer := 0;
    Last, Sum : Natural := 0;
    Line, Current_Digit, Left_Digit, Right_Digit : Ada.Strings.Unbounded.Unbounded_String;
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
        for I in Digit_array'First..Digit_Array'Last loop
            Current_Digit := Digit_Array(I);
            Left_Index := Ada.Strings.Unbounded.Index(Line, Ada.Strings.Unbounded.To_String(Current_Digit), 1);
            if Left_Index /= 0 and Left_Index < Min_Index then
                Min_Index := Left_Index;
                if Ada.Strings.Unbounded.Length(Current_Digit) > 1 then
                    Left_Digit := Word_To_Number(Current_Digit);
                else
                    Left_Digit := Current_Digit;
                end if;
            end if;
        end loop;
        for I in Digit_array'First..Digit_Array'Last loop
            Current_Digit := Digit_Array(I);
            Temp_Index := 0;
            loop
                Temp_Index :=
                    Ada.Strings.Unbounded.Index(Line, Ada.Strings.Unbounded.To_String(Current_Digit), Temp_Index + 1);
                exit when Temp_Index = 0;
                Right_Index := Temp_Index;
            end loop;
            if Right_Index > Max_Index then
                Max_Index := Right_Index;
                if Ada.Strings.Unbounded.Length(Current_Digit) > 1 then
                    Right_Digit := Word_To_Number(Current_Digit);
                else
                    Right_Digit := Current_Digit;
                end if;
            end if;
        end loop;
        Number := Integer'Value(Ada.Strings.Unbounded.To_String(Left_Digit) &
            Ada.Strings.Unbounded.To_String(Right_Digit));
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
end Day01b;
