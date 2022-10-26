{$mode objfpc} // directive to be used for defining classes

program tale_of_pascal;
uses Crt, Math, Classes, SysUtils, Fgl, Windows, Logging, Mapping, Actions;

type Direction = (up, down, left, right);

var x : integer = 21;
    y : integer = 6;
    lx : integer = 11;
    ly : integer = 2;
    dir : Direction = down;
    desc_line_count : integer = 0;

procedure WriteVarMap;
begin
    // Interop to Action Lang
    SetVarMap('player_lx', lx);
    SetVarMap('player_ly', ly);
    
    SetVarMap('player_x', x);
    SetVarMap('player_y', y);
end;

procedure ReadVarMap;
begin
    // Interop back w/ Action Lang
    x := GetVarMap('player_x');
    y := GetVarMap('player_y');
end;

procedure DrawScr;
var player_tile : string;
    ch : char;
    len : integer;
    last_word : integer = 0;
    desc_lines : integer = 0;
    desc, desc_line : string;
    desc_words: TStringList;
    i, j : integer;
begin
    player_tile := map_collision[y][x + 1];
    desc := FetchTileType(player_tile).description;
    desc_words := TStringList.Create;
    { desc_words.Delimiter := ' ';
    desc_words.StrictDelimiter := true;
    desc_words.DelimitedText := desc; }

    CursorAt(lx, ly);
    DrawTile(lx, ly, map_text[ly]);

    case dir of
        up: ch := '^';
        down: ch := 'v';
        left: ch := '<';
        right: ch := '>';
    end;

    CursorAt(x, y);
    DrawTile(ch, 'player');
    
    CursorAt(0, map_height + 1);
    WriteLn(x, ', ', y, '   ');
    
    // Split description into words (unless empty)
    // Doesn't use Delimiter because of "quotes"
    if Length(desc) > 0 then
    begin
        j:= 1;
        i:= 1;
        len := length (desc);
        repeat
            while (desc[i] <> ' ') and (i <= len) do inc(i);
            desc_words.Add(copy(desc, j, i - j));
            inc(i);
            j := i
        until i > len;
    end;

    // Redraw tooltip contents
    TextBackground(FetchTileType('tooltip').bg);
    TextColor(FetchTileType('tooltip').fg);
    for j := 0 to tooltip_height do
    begin
        CursorAt(map_width + tooltip_gap + 1, j + 1);
        desc_line := '';
        len := 0;

        if last_word < desc_words.Count then
        begin
            if j = 0 then
            begin
                desc_line := 'Description: ';
                len := Length(desc_line)
            end
            else
            begin
                desc_line := '';

                while (len < tooltip_width) and (last_word < desc_words.Count) do
                begin
                    { NOTE: Wastes one character if this is the last word in a line.
                    A potential optimization could detect that and avoid adding a space at the end of the line }
                    if (len + Length(desc_words[last_word]) >= tooltip_width) then break;
                    desc_line := Concat(desc_line, desc_words[last_word], ' ');
                    len := len + Length(desc_words[last_word]) + 1;
                    inc (last_word)
                end;

                if last_word = desc_words.Count then desc_lines := j;
            end;
        end
        else if j > desc_line_count then break; // Stop redrawing past the last line of the old description

        Write(desc_line, StringOfChar(' ', tooltip_width - len));
    end;

    desc_line_count := desc_lines;
    
    // Reset cursor
    TextBackground(Black);
    TextColor(White);
    CursorAt(0, 20);
end;

function GetInput() : char;
var input: char;
begin
    // Wait until there is one input that is an arrow key (* any other inputs are disregarded *)
    repeat
        input:=ReadKey;
        
        if input = #100 then DebugVarMap;
        if input = #27 then exit(input) // If pressed ESC, exit immediately
    until (input = #0);

    input:=ReadKey;

    // Stores old position values
    lx := x;
    ly := y;
    
    // Moves according to input (Up Down Left Right)
    case input of
        #72: begin
            dec(y);
            dir := up;
        end;
        #80: begin
            inc(y);
            dir := down;
        end;
        #75: begin
            dec(x);
            dir := left;
        end;
        #77: begin
            inc(x);
            dir := right;
        end;
    end;
    
    WriteVarMap;
    TouchAction.DetectTouches(x, y);
    ReadVarMap;

    // Pointer-based Touch action code
    { touch_action := touch_actions[x][y];
    if touch_action <> nil then begin
        WriteLn('Heh ', touch_action^.x, ' and ', touch_action^.y);
    end; }

    // Out-of-bounds / Collision Detection
    if (x < 0) or (x > map_width) or (y < 0) or (y > map_height) or (FetchTileType(map_collision[y][x + 1]).flags and 1 = 1) then
    begin
        x := lx;
        y := ly;
    end;

    TextBackground(Black);
    TextColor(White);

    exit(input)
end;

procedure InitializeConsole;
var Coord: TCoord;
    Rect: TSmallRect;
begin
    TextBackground(Black);
    TextColor(White);
    CursorOff;
    ClrScr;
  
    Rect.Left := 1;
    Rect.Top := 1;
    Rect.Right := 100;  // notice horiz scroll bar once the following executes
    Rect.Bottom := 30;
    Coord.X := Byte(Rect.Right + 1 - Rect.Left);
    Coord.y := Byte(Rect.Bottom + 1 - Rect.Top);
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
    SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), False, Rect);
end;

var input : char;

begin
    WriteVarMap;
    InitializeConsole;
    DrawMap;
    ReadVarMap;

    repeat
        DrawScr;
        input := GetInput();
    until (input = #27); // Esc Key
    
    ClrScr;
    WriteLn('Goodbye o/');
end.