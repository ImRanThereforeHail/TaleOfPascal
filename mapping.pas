{$mode objfpc} // directive to be used for defining classes

unit Mapping;

interface
uses Crt, Classes, SysUtils, Math, Fgl, Logging;

const tooltip_width : integer = 30;
    tooltip_height : integer = 6;
    tooltip_gap : integer = 5;

type TileType = record fg, bg, flags : integer; description : string end;
type TileDB = specialize TFPGMap<string, TileType>;

var map_width, map_height : integer;
    map_text, map_collision : TStringList;
    tile_db : TileDB;

procedure CursorAt(x, y : integer);
procedure DrawTile(tile : char; tile_id : string);
procedure DrawTile(tile_x, tile_y : integer; map_line : string);
procedure DrawMap;

implementation

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
var i, j : integer;
begin
    TextBackground(Black);
    TextColor(Black);
    
    for j := 0 to map_height do
    begin
        for i := 0 to Length(map_text[j]) - 1 do DrawTile(i, j, map_text[j]);
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

        if (j = 0) or (j = tooltip_height) then Write(StringOfChar('-', tooltip_width + 2))
        else Write('|', StringOfChar(' ', tooltip_width), '|');
    end;
end;

procedure SetupMap;
var i : integer;
begin
    map_text := TStringList.Create;
    map_text.LoadFromFile('map.txt');

    map_collision := TStringList.Create;
    map_collision.LoadFromFile('map_col.txt');

    for i := 0 to map_text.Count - 1 do map_width := Max(map_width, Length(map_text[i]) - 1);
    map_height := map_text.Count - 1;
end;

procedure SetupTileDatabase;
var parts, tile_db_text : TStringList;
    tile_type : TileType;
    i : integer;
begin
    tile_db := TileDB.Create;
    parts := TStringList.Create;
    parts.StrictDelimiter := true;
    parts.Delimiter := ';';

    tile_db_text := TStringList.Create;
    tile_db_text.LoadFromFile('tile_db.txt');
    
    for i := 0 to tile_db_text.Count - 1 do
    begin
        parts.DelimitedText := tile_db_text[i];

        tile_type.fg := StrToInt(parts[1]);
        tile_type.bg := StrToInt(parts[2]);
        tile_type.flags := StrToInt(parts[3]);
        tile_type.description := parts[4];

        tile_db.Add(parts[0], tile_type);
    end;
end;

initialization
SetupTileDatabase;
SetupMap;
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