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
    x : integer = 21;
    y : integer = 6;
    dir : Direction = down;
    lx : integer = 11;
    ly : integer = 2;
    i, j : integer;
    last_desc_line_old : integer = 0;
    map_width, map_height : integer;
    map_text, map_collision : TStringList;
    tile_db : TileDB;
    log_file : TextFile;

procedure SetupLog;
begin
    Assign(log_file, 'log.txt');
    Rewrite(log_file);
end;

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

type Action = class
public
    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); virtual;
    procedure Run; virtual;
end;

procedure Action.LoadFrom(parts : TStringList; _parent : Action; _indent : integer); begin end;

procedure Action.Run; begin end;

type ActionList = specialize TFPGList<Action>;

type ParentAction = class(Action)
public
    indent : integer;
    parent : ParentAction;
    actions : ActionList;
    
    constructor Create;
    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

procedure ParentAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    Self.indent := _indent;
    Self.parent := _parent as ParentAction;
end;

constructor ParentAction.Create;
begin
    actions := ActionList.Create;
end;

procedure ParentAction.Run;
var a : Action;
begin
    for a in actions do a.Run;
end;

type TouchAction = class(ParentAction)
public
    x, y : integer;
    is_added : boolean;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
    procedure OnTouch;
end;

type TouchActionList = specialize TFPGList<TouchAction>;
var touch_actions : TouchActionList;

procedure TouchAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;
    
    Self.x := StrToInt(parts[1]);
    Self.y := StrToInt(parts[2]);
end;

procedure TouchAction.Run;
begin
    if not is_added then
    begin
        is_added := true;
        touch_actions.Add(self);
    end;
end;

procedure TouchAction.OnTouch;
var a : Action;
begin
    for a in actions do a.Run;
end;

type ConditionalAction = class(ParentAction)
public
    expression : string;

    procedure Run; override;
end;

procedure ConditionalAction.Run;
var a : Action;
begin
    // Evaluate condition
    if true then for a in actions do a.Run;
end;

type SetTileAction = class(Action)
public
    x, y : integer;
    tile_text, tile_type : char;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

procedure SetTileAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;
    
    Self.x := StrToInt(parts[1]);
    Self.y := StrToInt(parts[2]);
    
    Self.tile_text := parts[3][1];
    Self.tile_type := parts[3][2];
end;

procedure SetTileAction.Run;
var str_temp : string;
begin
    inherited;
    // WriteLn('Setting tile!');
    str_temp := map_collision[y];
    str_temp[x + 1] := tile_type;
    map_collision[y] := str_temp;

    str_temp := map_text[y];
    str_temp[x + 1] := tile_text;
    map_text[y] := str_temp;

    CursorAt(x, y);
    DrawTile(x, y, str_temp);
end;

procedure DrawMap;
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

        if (j = 0) or (j = tooltip_height) then Write(StringOfChar('-', tooltip_width + 2))
        else Write('|', StringOfChar(' ', tooltip_width), '|');
    end;
end;

procedure SetupTileDatabase;
var parts, tile_db_text : TStringList;
    tile_type : TileType;
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

procedure SetupActions;
var actions_text, action_parts : TStringList;
    action_line : string;
    indent : integer;
    line_action : Action;
    touch_action : TouchAction;
    root_action : ParentAction;
    set_tile_action : SetTileAction;
    parent_action : ParentAction;
begin
    actions_text := TStringList.Create;
    actions_text.LoadFromFile('actions.txt');

    action_parts := TStringList.Create;
    action_parts.Delimiter := ' ';
    action_parts.StrictDelimiter := true;

    root_action := ParentAction.Create;
    root_action.indent := -1;

    parent_action := root_action;

    touch_actions := TouchActionList.Create;
    indent := 0;

    for i := 0 to actions_text.Count - 1 do
    begin
        action_line := actions_text[i];

        // Count and remove indent from this line
        indent := 0;
        while action_line[1] = ' ' do
        begin
            inc(indent);
            Delete(action_line, 1, 1);
        end;

        // Skip empty lines
        if Length(action_line) = 0 then continue;

        // Pop parents when the parents are more indented than the current line
        while parent_action.indent >= indent do parent_action := parent_action.parent;

        action_parts.DelimitedText := action_line;
        WriteLn(log_file, '<', indent, '/', parent_action.indent, '> ', action_parts[0], ': ', action_parts[1], ', ', action_parts[2], ', '); Flush(log_file); // Debug Log

        case action_parts[0] of
            'touch': line_action := TouchAction.Create;
            'set_tile': line_action := SetTileAction.Create;
        end;

        line_action.LoadFrom(action_parts, parent_action, indent);
        parent_action.actions.Add(line_action);

        if line_action is ParentAction then parent_action := line_action as ParentAction;
    end;

    // WriteLn(log_file, 'Root has ', root_action.actions.Count, ' actions.'); Flush(log_file); // Debug Log
    root_action.Run;
end;

procedure Setup;
var map_line : string;
begin
    {Assign(map_file, 'map.txt');
    Reset(map_file);
    ReadLn(map_file, map_text);}

    map_text := TStringList.Create;
    map_text.LoadFromFile('map.txt');

    map_collision := TStringList.Create;
    map_collision.LoadFromFile('map_col.txt');

    SetupTileDatabase;
    SetupActions;

    for map_line in map_text do map_width := Max(map_width, Length(map_line) - 1);
    map_height := map_text.Count - 1;

    CursorOff;
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
var touch_action : TouchAction;
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
    
    for touch_action in touch_actions do
    begin
        if (touch_action.x = x) and (touch_action.y = y) then begin 
            touch_action.OnTouch;
            break;
        end;
    end;
    
    // Pointer-based Touch action code
    { touch_action := touch_actions[x][y];
    if touch_action <> nil then begin
        WriteLn('Heh ', touch_action^.x, ' and ', touch_action^.y);
    end; }

    // Out-of-bounds / Collision Detection
    if (x < 0) or (x > map_width) or (y < 0) or (y > map_height) or (tile_db[map_collision[y][x + 1]].flags and 1 = 1) then
    begin
        x := lx;
        y := ly;
    end;

    TextBackground(Black);
    TextColor(White)
end;

begin
    SetupLog;
    Setup;
    repeat
        DrawScr;
        GetInput;        
    until (input = #27); // Esc Key
    
    ClrScr;
    WriteLn('Goodbye o/');
    Close(log_file);
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