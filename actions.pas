{$mode objfpc} // directive to be used for defining classes

unit Actions;

interface
uses Crt, Math, Classes, SysUtils, character, Fgl, Variants, Logging, Mapping;

type VarMap = specialize TFPGMap<string, Variant>;

type ActionValue = interface
    function GetValue() : Variant;
end;

type ActionValueList = specialize TFPGList<ActionValue>;

type ConstantValue = class(TInterfacedObject, ActionValue)
public
    var value : Variant;

    constructor Create(_value : Variant);
    function GetValue() : Variant;
end;

type VarValue = class(TInterfacedObject, ActionValue)
public
    var id : ActionValue;

    constructor Create(_id : ActionValue);
    function GetValue() : Variant;
end;

{ type ValueOperation = class
    var a, b : ActionValue;

    function Compute() : Variant;
end; }

type ActionLoadException = Class(Exception);

type Action = class(TInterfacedObject)
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

    has_else_action : boolean;
    else_action : Action;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
    procedure ExecuteTest;
end;

type GhostAction = interface ['{6a1ffa4f-cb09-4e26-a59a-2b22efc23d90}'] end;

type SetAction = class(Action)
public
    a, b, c, op, op2 : ActionValue;

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
    x, y, tile_text, tile_type : ActionValue;

    procedure LoadFrom(parts : TStringList; _parent : Action; _indent : integer); override;
    procedure Run; override;
end;

type PrintAction = class(Action)
public
    fields : ActionValueList;

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
procedure DebugVarMap;
procedure SetVarMap(id : string; value : Variant);
function GetVarMap(id : string) : Variant;

procedure SetupActions;

implementation
{ ************************************** )
  <<<<<<<<  IMPLEMENTATION START >>>>>>>
( -------------------------------------- }

constructor ConstantValue.Create(_value : Variant);
begin
    value := _value;
end;

constructor VarValue.Create(_id : ActionValue);
begin
    id := _id;
end;

function ConstantValue.GetValue() : Variant;
begin
    exit(value)
end;

function VarValue.GetValue() : Variant;
begin
    if not var_map.TryGetData(id.GetValue(), Result) then exit('');
end;

function ActionValueFrom(str : string) : ActionValue;
var i : integer;
    f : single;
begin
    if str[1] = '$' then result := VarValue.Create(ActionValueFrom(Copy(str, 2)))
    else
    begin
        if str = 'true' then result := ConstantValue.Create(True)
        else if str = 'false' then result := ConstantValue.Create(False)
        else if (pos('.', str) > 0) and (Length(str) > 1) and TryStrToFloat(str, f) then result := ConstantValue.Create(f)
        else if TryStrToInt(str, i) then result := ConstantValue.Create(i)
        else result := ConstantValue.Create(str);
    end;
end;

function ComputeOperation(a, b : Variant; op : string) : Variant;
var cast_type : Integer;
begin
    result := b;
    case VarToStr(op) of
        '+': result := a + b;
        '-': result := a - b;
        '*': result := a * b;
        '/': result := a / b;
        '%': result := a mod b;
        '==': result := a = b;
        '!=': result := a <> b;
        '>': result := a > b;
        '<': result := a < b;
        '<=': result := a <= b;
        '>=': result := a >= b;
        'and': result := a and b;
        'or': result := a or b;
        'xor': result := a xor b;
        'max': begin
            if a > b then result := a
            else result := b;
        end;
        'min': begin
            if a < b then result := a
            else result := b;
        end;
        'as': begin
            cast_type := varUnknown;
            case VarToStr(b) of
                'string': cast_type := varString;
                'int': cast_type := varInteger;
                'char': begin
                    cast_type := varString;
                    a := char(byte(a));
                end;
                'bool': cast_type := varBoolean;
                'byte': cast_type := varByte;
                'short': cast_type := varShortInt;
                'long': cast_type := varInt64;
                'float': cast_type := varSingle;
                'double': cast_type := varDouble;
            end;

            VarCast(result, a, cast_type);
        end;
        'at': begin
            if VarIsType(a, varString) and (Length(VarToStr(a)) > b) then result := VarToStr(a)[b + 1]
            else result := '';
        end;
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
begin
    ExecuteTest;
end;

procedure ConditionalAction.ExecuteTest;
var act : Action;
    passed : boolean;
begin
    passed := ComputeOperation(a.GetValue(), b.GetValue(), op) = true;

    // WriteLn(log_file, 'Condition ', a.GetValue(), ' ', op, ' ', b.GetValue(), ' resulted in ', passed); Flush(log_file);
    
    // Evaluate condition
    if passed then
        for act in actions do
            act.Run
    else if has_else_action then else_action.Run;
end;

procedure SetAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
var empty_var : Variant;
begin
    inherited;
    a := ActionValueFrom(parts[1]);
    op := ActionValueFrom(parts[2]);
    b := ActionValueFrom(parts[3]);
    
    VarClear(empty_var);
    op2 := ConstantValue.Create(empty_var);
    c := ConstantValue.Create(empty_var);
    
    if parts.Count >= 5 then op2 := ActionValueFrom(parts[4]);
    if parts.Count >= 6 then c := ActionValueFrom(parts[5]);
end;

procedure SetAction.Run;
var a_val, b_val, c_val, op_val, op2_val : Variant;
    a_res, b_res : Variant;
    at_temp : string;
begin
    a_val := a.GetValue();
    b_val := b.GetValue();
    c_val := c.GetValue();
    op_val := op.GetValue();
    op2_val := op2.GetValue();
    
    if op_val = 'at' then
    begin
        if op2_val = '=' then
        begin
            at_temp := VarToStr(var_map[a_val]);
            at_temp[b_val] := c_val;
            var_map[a_val] := at_temp;
        end;
        exit
    end;
    
    if VarIsType(b_val, varString) and (b_val = 'tile') then b_res := map_text[c_val][op2_val + 1]
    else if VarIsType(b_val, varString) and (b_val = 'tile_type') then b_res := map_collision[c_val][op2_val + 1]
    else if VarIsType(b_val, varString) and (b_val = 'len') then b_res := Length(VarToStr(op2_val))
    else if not VarIsEmpty(op2_val) then b_res := ComputeOperation(b_val, c_val, op2_val)
    else b_res := b_val;

    if not var_map.TryGetData(a_val, a_res) then a_res := Null;

    var_map[a_val] := ComputeOperation(a_res, b_res, Copy(op_val, 1, Length(op_val) - 1));
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
    Self.x := ActionValueFrom(parts[1]);
    Self.y := ActionValueFrom(parts[2]);
    
    Self.tile_text := ActionValueFrom(parts[3]);
    Self.tile_type := ActionValueFrom(parts[4]);

    // WriteLn(log_file, 'set tile: ', Self.x.GetValue(), ' ', Self.y.GetValue(), ' ', Self.tile_text.GetValue(), ' ', Self.tile_type.GetValue()); Flush(log_file);
end;

procedure SetTileAction.Run;
var str_temp : string;
var tile_val : string;
var x_val, y_val : integer;
begin
    inherited;
    tile_val := tile_type.GetValue();
    x_val := x.GetValue();
    y_val := y.GetValue();

    if (x_val > map_width) or (y_val > map_height) then exit();

    // Set tile type
    str_temp := map_collision[y_val];
    str_temp[x_val + 1] := tile_val[1];
    map_collision[y_val] := str_temp;

    // Set tile text
    tile_val := tile_text.GetValue();
    str_temp := map_text[y_val];
    str_temp[x_val + 1] := tile_val[1];
    map_text[y_val] := str_temp;

    CursorAt(x_val, y_val);
    DrawTile(x_val, y_val, str_temp);
end;

procedure PrintAction.LoadFrom(parts : TStringList; _parent : Action; _indent : integer);
var i : Integer;
begin
    fields := ActionValueList.Create;
    for i := 1 to parts.Count - 1 do fields.Add(ActionValueFrom(parts[i]));
end;

procedure PrintAction.Run;
var val : ActionValue;
    msg : string;
begin
    msg := '';
    for val in fields do msg := msg + VarToStr(val.GetValue());
    WriteLn(log_file, msg); Flush(log_file);
end;

procedure DebugVarMap;
var i : Integer;
begin
    WriteLn(log_file, '[----------VAR MAP DEBUG----------]');
    for i := 0 to var_map.Count - 1 do WriteLn(log_file, var_map.Keys[i], ': ', var_map.Data[i]);
    WriteLn(log_file, '[---------------------------------]');

    // Flush at the end only
    Flush(log_file);
end;

procedure SetVarMap(id : string; value : Variant);
begin
    var_map[id] := value;
end;

function GetVarMap(id : string) : Variant;
begin
    result := var_map[id];
end;

procedure SetupActions;
var actions_text, action_parts : TStringList;
    action_line : string;
    indent : integer;
    line_action : Action;
    cond_action : ConditionalAction;
    parent_action : ParentAction;
    i, j : integer;
    is_ghost : Boolean;
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

    WriteLn(log_file, '------------------- [actions processing begin] -------------------'); Flush(log_file);

    for i := 0 to actions_text.Count - 1 do
    begin
        action_line := actions_text[i];

        // Count and remove indent from this line
        indent := 0;
        while (Length(action_line) > 0) and IsWhiteSpace(action_line[1]) do
        begin
            inc(indent);
            Delete(action_line, 1, 1);
        end;
        
        // Commenting (it's just like this line!)
        if pos('//', action_line) > 0 then action_line := Copy(action_line, 1, pos('//', action_line) - 1);
        
        // Skip empty lines
        if (Length(action_line) = 0) or (LeftStr(action_line, 2) = '//') then continue;

        // Pop parents when the parents are more indented than the current line
        while parent_action.indent >= indent do
        begin
            if parent_action is ConditionalAction then cond_action := parent_action as ConditionalAction;
            parent_action := parent_action.parent;
        end;

        action_parts.DelimitedText := action_line;

        // Debug Log
        Write(log_file, '<', indent, '/', parent_action.indent, '> ', action_parts[0]);
        for j := 1 to action_parts.Count - 1 do Write(log_file, ' ', action_parts[j]);
        WriteLn(log_file); Flush(log_file);

        is_ghost := false;

        case action_parts[0] of
            'func': line_action := FunctionAction.Create;
            'touch': line_action := TouchAction.Create;
            'set': line_action := SetAction.Create;
            'call': line_action := CallAction.Create;
            'print': line_action := PrintAction.Create;
            'set_tile': line_action := SetTileAction.Create;
            'if': line_action := ConditionalAction.Create;
            'elif': begin
                line_action := ConditionalAction.Create;
                cond_action.else_action := line_action;
                cond_action.has_else_action := true;
                is_ghost := true;
            end;
            'else': begin
                line_action := ParentAction.Create;
                cond_action.else_action := line_action;
                cond_action.has_else_action := true;
                is_ghost := true;
            end;
            else line_action := Action.Create;
        end;

        line_action.LoadFrom(action_parts, parent_action, indent);
        
        if not is_ghost then parent_action.actions.Add(line_action);
        if line_action is ParentAction then parent_action := line_action as ParentAction;
    end;

    WriteLn(log_file, '------------------- [actions processing done] -------------------'); Flush(log_file);

    // Return to Root Action
    while parent_action.indent >= 0 do parent_action := parent_action.parent;
    parent_action.Run;
end;

initialization
DefaultFormatSettings.DecimalSeparator := '.';
SetupActions;
end.