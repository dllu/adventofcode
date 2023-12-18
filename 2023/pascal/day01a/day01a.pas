PROGRAM Day01a(INPUT, OUTPUT);
{$mode objFPC}

USES StrUtils, SysUtils;

VAR
    FileName : STRING;
    Res      : INTEGER;

PROCEDURE Usage;
BEGIN
    WriteLn('usage: ', ExtractFileName(paramStr(0)), ' <file>');
    Halt(1)
END;

FUNCTION Process(FileName : STRING) : INTEGER;
CONST
    Digits     : ARRAY [1..10] OF STRING =
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
VAR
    InFile     : TEXT;
    FileLine   : STRING;
    Sum        : INTEGER = 0;
    MinIndex   : INTEGER;
    MaxIndex   : INTEGER;
    LeftIndex  : INTEGER;
    RightIndex : INTEGER;
    I          : INTEGER;
    J          : INTEGER;
    DigitStr   : STRING;
    LeftDigit  : INTEGER;
    RightDigit : INTEGER;
BEGIN
    Assign(InFile, FileName);
    Reset(InFile);
    WHILE NOT Eof(InFile) DO
    BEGIN
        ReadLn(InFile, FileLine);
        MinIndex := MAXINT;
        MaxIndex := -2;
        LeftDigit := 0;
        RightDigit := 0;
        FOR I := 1 TO 10 DO
        BEGIN
            DigitStr := Digits[I];
            LeftIndex := NPos(DigitStr, FileLine, 1);
            IF LeftIndex <> 0 THEN
            BEGIN
                IF LeftIndex < MinIndex THEN
                BEGIN
                    MinIndex := LeftIndex;
                    LeftDigit := I - 1
                END;
            END;
            J := 0;
            REPEAT
                J := J + 1;
                RightIndex := NPos(DigitStr, FileLine, J)
            UNTIL RightIndex = 0;
            RightIndex := NPos(DigitStr, FileLine, J - 1);
            IF RightIndex <> 0 THEN
            BEGIN
                IF RightIndex > MaxIndex THEN
                BEGIN
                    MaxIndex := RightIndex;
                    RightDigit := I - 1;
                END;
            END;
        END;
        Sum := Sum + LeftDigit * 10 + RightDigit;
    END;
    Close(InFile);
    Process := Sum
END;

BEGIN
    IF paramCount() < 1 THEN
        Usage;
    FileName := paramStr(1);
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
