{$mode objfpc} // directive to be used for defining classes

unit Actions;

interface
uses Crt, Math, Classes, SysUtils, Fgl, Variants, Logging, Mapping;

type VarMap = specialize TFPGMap<string, Variant>;

type ActionValue = interface
    function GetValue() : Variant;
end;

type ConstantValue = class(TInterfacedObject, ActionValue)
public
    var value : Variant;

    constructor Create(_value : Variant);
    function GetValue() : Variant;
end;

type VarValue = class(TInterfacedObject, ActionValue)
public
    var id : string;

    constructor Create(_id : string);
    function GetValue() : Variant;
end;

{ type ValueOperation = class
    var a, b : ActionValue;

    function Compute() : Variant;
end; }

type ActionLoadException = Class(Exception);

type Action = class
public
    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); virtual;
    procedure Run; virtual;
end;

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

type TouchAction = class(ParentAction)
public
    is_added : boolean;
    x, y : integer;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
    procedure OnTouch;
    
    class procedure DetectTouches(px, py : integer); static;
end;

type FunctionAction = class(ParentAction)
public
    is_added : boolean;
    function_name : string;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
    procedure Call;
end;

type ConditionalAction = class(ParentAction)
public
    a, b : ActionValue;
    op : string;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

type SetAction = class(Action)
public
    a, b, c : ActionValue;
    op, op2 : string;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

type CallAction = class(Action)
public
    function_name : string;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

type SetTileAction = class(Action)
public
    x, y : integer;
    tile_text, tile_type : char;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

function ComputeOperation(a, b : Variant; op : string) : Variant;

function ActionValueFrom(str : string) : ActionValue;

type TouchActionList = specialize TFPGList<TouchAction>;
var touch_actions : TouchActionList;

type FunctionActionMap = specialize TFPGMap<string, FunctionAction>;
var function_actions : FunctionActionMap;

var var_map : VarMap;

procedure SetupActions;

implementation
{ ************************************** )
  <<<<<<<<  IMPLEMENTATION START >>>>>>>
( -------------------------------------- }

constructor ConstantValue.Create(_value : Variant);
begin
    value := _value;
end;

constructor VarValue.Create(_id : string);
begin
    id := _id;
end;

function ConstantValue.GetValue() : Variant;
begin
    exit(value)
end;

function VarValue.GetValue() : Variant;
begin
    exit(var_map[id])
end;

function ActionValueFrom(str : string) : ActionValue;
var i : integer;
    f : single;
begin
    if str[1] = '$' then result := VarValue.Create(Copy(str, 2))
    else
    begin
        if (pos('.', str) > 0) and TryStrToFloat(str, f) then result := ConstantValue.Create(f)
        else if TryStrToInt(str, i) then result := ConstantValue.Create(i)
        else result := ConstantValue.Create(str);
    end;
end;

function ComputeOperation(a, b : Variant; op : string) : Variant;
begin
    result := b;

    case op of
        '+': result := a + b;
        '-': result := a - b;
        '*': result := a * b;
        '/': result := a / b;
        '==': result := a = b;
        '!=': result := a <> b;
        '>': result := a > b;
        '<': result := a < b;
        '<=': result := a <= b;
        '>=': result := a >= b;
        'max': begin
            if a > b then result := a
            else result := b;
        end;
        'min': begin
            if a < b then result := a
            else result := b;
        end;
        'at': result := a[b];
    end;
end;

procedure Action.LoadFrom(parts : TStringList; _parent : Action; _indent : integer); begin end;
procedure Action.Run; begin end;

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
var act : Action;
begin
    for act in actions do act.Run;
end;

procedure FunctionAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    if _indent > 0 then raise ActionLoadException.Create('Function actions must be in root level.');
    inherited;

    Self.function_name := parts[1];
end;

procedure FunctionAction.Run;
begin
    if not is_added then
    begin
        is_added := true;
        function_actions.Add(function_name, self);
    end;
end;

procedure FunctionAction.Call;
var act : Action;
begin
    for act in actions do act.Run;
end;

procedure TouchAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;
    
    if not TryStrToInt(parts[1], Self.x) or not TryStrToInt(parts[2], Self.y) then raise ActionLoadException.Create('Touch action requires two integer parameters.');
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
var act : Action;
begin
    for act in actions do act.Run;
end;

class procedure TouchAction.DetectTouches(px, py : integer);
var touch_action : TouchAction;
begin
    for touch_action in touch_actions do
    begin
        if (touch_action.x = px) and (touch_action.y = py) then begin 
            touch_action.OnTouch;
            break;
        end;
    end;
end;

procedure ConditionalAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;

    a := ActionValueFrom(parts[1]);
    op := parts[2];
    b := ActionValueFrom(parts[3]);
end;

procedure ConditionalAction.Run;
var act : Action;
    passed : boolean;
begin
    passed := true;
    passed := ComputeOperation(a.GetValue(), b.GetValue(), op) = true;

    WriteLn(log_file, 'Condition ', a.GetValue(), ' ', op, ' ', b.GetValue(), ' resulted in ', passed); Flush(log_file);
    // Evaluate condition
    if passed then for act in actions do act.Run;
end;

procedure SetAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;
    a := ActionValueFrom(parts[1]);
    op := parts[2];
    b := ActionValueFrom(parts[3]);
    if parts.Count >= 6 then begin
        op2 := parts[4];
        c := ActionValueFrom(parts[5]);
    end;
end;

procedure SetAction.Run;
var i : integer;
    a_val : Variant;
    a_res : Variant;
    b_res : Variant;
begin
    a_val := a.GetValue();
    
    if op2 <> '' then b_res := ComputeOperation(b.GetValue(), c.GetValue(), op2)
    else b_res := b.GetValue();

    if not var_map.TryGetData(a_val, a_res) then a_res := Null;
    var_map[a.GetValue()] := ComputeOperation(a_res, b_res, Copy(op, 1, Length(op) - 1));
    { for i := 0 to var_map.Count - 1 do
    begin
        WriteLn(log_file, 'var: ', var_map.Keys[i], ' = ', var_map.Data[i]); Flush(log_file); // Debug Log
    end; }
end;

procedure CallAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
begin
    inherited;

    function_name := parts[1];
end;

procedure CallAction.Run;
begin
    function_actions[function_name].Call;
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

procedure SetupActions;
var actions_text, action_parts : TStringList;
    action_line : string;
    indent : integer;
    line_action : Action;
    parent_action : ParentAction;
    i, j : integer;
begin
    actions_text := TStringList.Create;
    actions_text.LoadFromFile('actions.txt');

    action_parts := TStringList.Create;
    action_parts.Delimiter := ' ';
    action_parts.StrictDelimiter := true;

    parent_action := ParentAction.Create;
    parent_action.indent := -1;

    function_actions := FunctionActionMap.Create;
    touch_actions := TouchActionList.Create;
    var_map := VarMap.Create;

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

        // Debug Log
        Write(log_file, '<', indent, '/', parent_action.indent, '> ', action_parts[0]);
        for j := 1 to action_parts.Count - 1 do Write(log_file, ' ', action_parts[j]);
        WriteLn(log_file); Flush(log_file);

        case action_parts[0] of
            'func': line_action := FunctionAction.Create;
            'touch': line_action := TouchAction.Create;
            'if': line_action := ConditionalAction.Create;
            'set': line_action := SetAction.Create;
            'call': line_action := CallAction.Create;
            'set_tile': line_action := SetTileAction.Create;
            else line_action := Action.Create;
        end;

        line_action.LoadFrom(action_parts, parent_action, indent);
        parent_action.actions.Add(line_action);

        if line_action is ParentAction then parent_action := line_action as ParentAction;
    end;

    // Return to Root Action
    while parent_action.indent >= 0 do parent_action := parent_action.parent;
    parent_action.Run;
end;

initialization
DecimalSeparator := '.';
SetupActions;
end.