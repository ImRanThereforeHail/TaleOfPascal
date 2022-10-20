{$mode objfpc} // directive to be used for defining classes

program tale_of_pascal;
uses Crt, Math, Classes, SysUtils, Fgl;

type TileType = record fg, bg, flags : integer; description : string end;

type TileDB = specialize TFPGMap<string, TileType>;

type Direction = (up, down, left, right);

const tooltip_width : integer = 30;
    tooltip_height : integer = 6;
    tooltip_gap : integer = 5;

var input: char;
    x : integer = 20;
    y : integer = 5;
    dir : Direction = down;
    lx : integer = 11;
    ly : integer = 2;
    i, j : integer;
    last_desc_line_old : integer = 0;
    map_width, map_height : integer;
    map_text, map_collision, tile_db_text: TStringList;
    tile_db : TileDB;

procedure CursorAt(x, y : integer);
begin
    GoToXY(x + 1, y + 1);
end;

// Draw tile based on a character and the tile id
procedure DrawTile(tile : char; tile_id : string);
begin
    TextBackground(tile_db[tile_id].bg);
    TextColor(tile_db[tile_id].fg);
    Write(tile)
end;

// Draw tile based on x and y positions, and a line string
procedure DrawTile(tile_x, tile_y : integer; map_line : string);
begin
    DrawTile(map_line[tile_x + 1], map_collision[tile_y][tile_x + 1]);
end;

procedure DrawMap;
var ch : char;
begin
    TextBackground(Black);
    TextColor(Black);
    
    for j := 0 to map_height do
    begin
        for i := 0 to map_width do DrawTile(i, j, map_text[j]);
        // Write description
        TextBackground(Black);
        TextColor(Black);
        
        WriteLn;
    end;

    TextBackground(tile_db['tooltip'].bg);
    TextColor(tile_db['tooltip'].fg);

    // Draw tooltip frame and background
    for j := 0 to tooltip_height do
    begin
        CursorAt(map_width + tooltip_gap, j);

        if (j = 0) or (j = tooltip_height) then ch := '-'
        else ch := ' ';
        Write('|', StringOfChar(ch, tooltip_width), '|');
    end;
end;

procedure Setup;
var parts : TStringList;
    lin : string;
    tile_type : TileType;
begin
    CursorOff;
    {Assign(map_file, 'map.txt');
    Reset(map_file);
    ReadLn(map_file, map_text);}

    map_text := TStringList.Create;
    map_text.LoadFromFile('map.txt');

    map_collision := TStringList.Create;
    map_collision.LoadFromFile('map_col.txt');

    tile_db_text := TStringList.Create;
    tile_db_text.LoadFromFile('tile_db.txt');

    tile_db := TileDB.Create;
    parts := TStringList.Create;
    parts.StrictDelimiter := true;
    parts.Delimiter := ';';

    for i := 0 to tile_db_text.Count - 1 do
    begin
        parts.DelimitedText := tile_db_text[i];

        tile_type.fg := StrToInt(parts[1]);
        tile_type.bg := StrToInt(parts[2]);
        tile_type.flags := StrToInt(parts[3]);
        tile_type.description := parts[4];

        tile_db.Add(parts[0], tile_type);
    end;

    for lin in map_text do map_width := Max(map_width, Length(lin) - 1);
    map_height := map_text.Count - 1;

    ClrScr;
    DrawMap;
end;

procedure DrawScr;
var player_tile : string;
ch : char;
len : integer;
last_word : integer = 0;
last_desc_line_new : integer = 0;
desc, desc_line : string;
desc_words: TStringList;
begin
    player_tile := map_collision[y][x + 1];
    desc := tile_db[player_tile].description;
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
    
    GoToXY(1, 15);
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
    TextBackground(tile_db['tooltip'].bg);
    TextColor(tile_db['tooltip'].fg);
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

                if last_word = desc_words.Count then last_desc_line_new := j;
            end;
        end
        else if j > last_desc_line_old then break; // Stop redrawing past the last line of the old description

        Write(desc_line, StringOfChar(' ', tooltip_width - len));
    end;

    last_desc_line_old := last_desc_line_new;
    
    // Reset cursor
    TextBackground(Black);
    TextColor(White);
    CursorAt(0, 20);
end;

procedure GetInput;
begin
    // Waits for a key input from user
    input:=ReadKey;
    // Stores old position values
    lx := x;
    ly := y;
    
    // Moves according to input (Up Down Left Right)
    case input of
        #72: 
        begin
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
    
    // Out-of-bounds / Collision Detection
    if (x < 0) or (x > map_width) or (y < 0) or (y > map_height) or (tile_db[map_collision[y][x + 1]].flags and 1 = 1) then
    begin
        x := lx;
        y := ly;
    end
end;

begin
    Setup;
    repeat
        DrawScr;
        GetInput;        
    until (input = #27); // Esc Key
    
    ClrScr;
    WriteLn('Goodbye o/');
end.

{ 
Color Constants:
    Black = 0;
    Blue = 1;
    Green = 2;
    Cyan = 3;
    Red = 4;
    Magenta = 5;
    Brown = 6;
    LightGray = 7;
    DarkGray = 8;
    LightBlue = 9;
    LightGreen = 10;
    LightCyan = 11;
    LightRed = 12;
    LightMagenta = 13;
    Yellow = 14;
    White = 15;
}