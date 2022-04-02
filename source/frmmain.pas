{ +--------------------------------------------------------------------------+ }
{ | MM7DRead v0.3 * Status reader program for MM7D device                    | }
{ | Copyright (C) 2020-2022 Pozsár Zsolt <pozsar.zsolt@szerafingomba.hu>     | }
{ | frmmain.pas                                                              | }
{ | Main form                                                                | }
{ +--------------------------------------------------------------------------+ }

//   This program is free software: you can redistribute it and/or modify it
// under the terms of the European Union Public License 1.1 version.

//   This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.

unit frmmain;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, ValEdit, StrUtils, untcommonproc;

type
  { TForm1 }
  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel16: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Button7: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Shape1: TShape;
    Shape15: TShape;
    Shape2: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape8: TShape;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    ValueListEditor1: TValueListEditor;
    procedure Button7Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  inifile: string;

const
  CNTNAME = 'MM7D';
  CNTVER = '0.3';

resourcestring
  MESSAGE01 = 'Cannot read configuration file!';
  MESSAGE02 = 'Cannot write configuration file!';
  MESSAGE03 = 'Cannot read data from this URL!';
  MESSAGE04 = 'Not compatible controller!';
  MESSAGE05 = 'name';
  MESSAGE06 = 'value';
  MESSAGE07 = 'IP address:';
  MESSAGE08 = 'MAC address:';
  MESSAGE09 = 'Serial number:';
  MESSAGE10 = 'Sw. version:';
  MESSAGE11 = 'Mode:';
  MESSAGE12 = 'T limits:';
  MESSAGE13 = 'RH limits:';
  MESSAGE14 = 'RGL limit:';
  MESSAGE15 = 'T:';
  MESSAGE16 = 'RH:';
  MESSAGE17 = 'RGL:';
  MESSAGE18 = 'G LED:';
  MESSAGE19 = 'Y LED:';
  MESSAGE20 = 'R LED:';
  MESSAGE21 = 'Your IP address is not allowed!';
  MESSAGE22 = 'Authentication error!';

implementation

{$R *.lfm}
{ TForm1 }

// add URL to list
procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  line: byte;
  thereis: boolean;
begin
  thereis := False;
  if ComboBox1.Items.Count > 0 then
    for line := 0 to ComboBox1.Items.Count - 1 do
      if ComboBox1.Items.Strings[line] = ComboBox1.Text then
        thereis := True;
  if (not thereis) and (ComboBox1.Items.Count < 64) then
    ComboBox1.Items.AddText(ComboBox1.Text);
end;

// remove URL from list
procedure TForm1.SpeedButton3Click(Sender: TObject);
var
  line: byte;
begin
  if ComboBox1.Items.Count > 0 then
  begin
    for line := 0 to ComboBox1.Items.Count - 1 do
      if ComboBox1.Items.Strings[line] = ComboBox1.Text then
        break;
    ComboBox1.Items.Delete(line);
    ComboBox1Change(Sender);
  end;
end;

// event of ComboBox1
procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if length(ComboBox1.Text) = 0 then
  begin
    SpeedButton2.Enabled := False;
    SpeedButton3.Enabled := False;
    Button7.Enabled := False;
  end
  else
  begin
    SpeedButton2.Enabled := True;
    SpeedButton3.Enabled := True;
    Button7.Enabled := True;
  end;
end;

function checkcompatibility(Name, version: string): byte;
begin
  Result := 0;
  if getdatafromdevice(Form1.ComboBox1.Text, 0, Form1.Edit1.Text) then
  begin
    if untcommonproc.Value.Count = 2 then
      if (untcommonproc.Value.Strings[0] <> Name) or
        (untcommonproc.Value.Strings[1] <> version) then
        Result := 1;
  end
  else
    Result := 2;
end;

// refresh displays
procedure TForm1.Button7Click(Sender: TObject);
var
  i: integer;
const
  s1a: string = '<br>';
  s1b: string = '<td>';
  s1c: string = '</td>';
  s2: string = 'IP address:';
  s3: string = 'MAC address:';
  s4: string = 'Hardware serial number:';
  s5: string = 'Software version:';
  s6: string = '<td>Mode:</td>';
  s7: string = '<td>Temperature limit values:</td>';
  s8: string = '<td>Humidity limit values:</td>';
  s9: string = '<td>Gas level limit value:</td>';
  s10: string = '<td>Temperature:</td>';
  s11: string = '<td>Relative humidity:</td>';
  s12: string = '<td>Relative gas level:</td>';
  s13: string = '<td>Green:</td>';
  s14: string = '<td>Yellow:</td>';
  s15: string = '<td>Red:</td>';

begin
  // clear pages
  Label3.Caption := '0 °C';
  Label4.Caption := '0 %';
  Label18.Caption := '0 %';
  Shape4.Brush.Color := clGreen;
  Shape8.Brush.Color := clOlive;
  Shape5.Brush.Color := clMaroon;
  ValueListEditor1.Cols[1].Clear;
  Memo1.Clear;
  // check compatibility
  case checkcompatibility(CNTNAME, CNTVER) of
    0: StatusBar1.Panels.Items[0].Text := Value.Strings[0] + ' v' + Value.Strings[1];
    1:
    begin
      ShowMessage(MESSAGE04);
      StatusBar1.Panels.Items[0].Text := '';
      exit;
    end;
    2: StatusBar1.Panels.Items[0].Text := '';
  end;
  // get values
  if getdatafromdevice(ComboBox1.Text, 1, Edit1.Text) then
  begin
    // get IP address
    for i := 0 to Value.Count - 1 do
      if findpart(s2, Value.Strings[i]) <> 0 then
        break;
    if Value.Strings[i] = 'Not allowed client IP address!' then
    begin
      ShowMessage(MESSAGE21);
      exit;
    end;
    if Value.Strings[i] = 'Authentication error!' then
    begin
      ShowMessage(MESSAGE22);
      exit;
    end;
    Value.Strings[i] := stringreplace(Value.Strings[i], s1a, '', [rfReplaceAll]);
    Value.Strings[i] := stringreplace(Value.Strings[i], s2, '', [rfReplaceAll]);
    Value.Strings[i] := rmchr1(Value.Strings[i]);
    ValueListEditor1.Cells[1, 1] := Value.Strings[i];
    // get MAC address
    for i := 0 to Value.Count - 1 do
      if findpart(s3, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i] := stringreplace(Value.Strings[i], s1a, '', [rfReplaceAll]);
    Value.Strings[i] := stringreplace(Value.Strings[i], s3, '', [rfReplaceAll]);
    Value.Strings[i] := rmchr1(Value.Strings[i]);
    ValueListEditor1.Cells[1, 2] := Value.Strings[i];
    // get serial number
    for i := 0 to Value.Count - 1 do
      if findpart(s4, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i] := stringreplace(Value.Strings[i], s1a, '', [rfReplaceAll]);
    Value.Strings[i] := stringreplace(Value.Strings[i], s4, '', [rfReplaceAll]);
    Value.Strings[i] := rmchr1(Value.Strings[i]);
    ValueListEditor1.Cells[1, 3] := Value.Strings[i];
    // get software version
    for i := 0 to Value.Count - 1 do
      if findpart(s5, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i] := stringreplace(Value.Strings[i], s1a, '', [rfReplaceAll]);
    Value.Strings[i] := stringreplace(Value.Strings[i], s5, '', [rfReplaceAll]);
    Value.Strings[i] := rmchr1(Value.Strings[i]);
    ValueListEditor1.Cells[1, 4] := Value.Strings[i];
    // get operation mode
    for i := 0 to Value.Count - 1 do
      if findpart(s6, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := rmchr1(Value.Strings[i + 1]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr1(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 5] := Value.Strings[i + 1];
    // get temperature limits
    for i := 0 to Value.Count - 1 do
      if findpart(s7, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], '&deg;',
      '°', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 6] := Value.Strings[i + 1];
    // get humidity limits
    for i := 0 to Value.Count - 1 do
      if findpart(s8, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], '&nbsp;',
      '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 7] := Value.Strings[i + 1];
    // get gas level limit
    for i := 0 to Value.Count - 1 do
      if findpart(s9, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 8] := Value.Strings[i + 1];
    // get temperature
    for i := 0 to Value.Count - 1 do
      if findpart(s10, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], '&deg;',
      '°', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 9] := Value.Strings[i + 1];
    // get humidity
    for i := 0 to Value.Count - 1 do
      if findpart(s11, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 10] := Value.Strings[i + 1];
    // get gas level
    for i := 0 to Value.Count - 1 do
      if findpart(s12, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr3(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 11] := Value.Strings[i + 1];
    // get status of green LED
    for i := 0 to Value.Count - 1 do
      if findpart(s13, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr1(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 12] := Value.Strings[i + 1];
    // get status of yellow LED
    for i := 0 to Value.Count - 1 do
      if findpart(s14, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr1(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 13] := Value.Strings[i + 1];
    // get status of red LED
    for i := 0 to Value.Count - 1 do
      if findpart(s15, Value.Strings[i]) <> 0 then
        break;
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1b, '', [rfReplaceAll]);
    Value.Strings[i + 1] := stringreplace(Value.Strings[i + 1], s1c, '', [rfReplaceAll]);
    Value.Strings[i + 1] := rmchr1(Value.Strings[i + 1]);
    ValueListEditor1.Cells[1, 14] := Value.Strings[i + 1];
    ValueListEditor1.AutoSizeColumns;
    // write to display
    Label3.Caption := ValueListEditor1.Cells[1, 9];
    Label4.Caption := ValueListEditor1.Cells[1, 10];
    Label18.Caption := ValueListEditor1.Cells[1, 11];
    if ValueListEditor1.Cells[1, 12] = 'ON' then
      Shape4.Brush.Color := clLime
    else
      Shape4.Brush.Color := clGreen;
    if ValueListEditor1.Cells[1, 13] = 'ON' then
      Shape8.Brush.Color := clYellow
    else
      Shape8.Brush.Color := clOlive;
    if ValueListEditor1.Cells[1, 14] = 'ON' then
      Shape5.Brush.Color := clRed
    else
      Shape5.Brush.Color := clMaroon;
  end
  else
  begin
    ShowMessage(MESSAGE03);
    exit;
  end;
  // get log
  if getdatafromdevice(ComboBox1.Text, 2, Edit1.Text) then
  begin
    // write log
    Memo1.Clear;
    for i := 0 to Value.Count - 1 do
      if findpart('<tr><td><pre>', Value.Strings[i]) <> 0 then
      begin
        Value.Strings[i] := rmchr3(Value.Strings[i]);
        Value.Strings[i] := stringreplace(Value.Strings[i], '<tr><td><pre>',
          '', [rfReplaceAll]);
        Value.Strings[i] := stringreplace(Value.Strings[i],
          '</pre></td><td><pre>', #9, [rfReplaceAll]);
        Value.Strings[i] := stringreplace(Value.Strings[i], '</pre></td></tr>',
          '', [rfReplaceAll]);
        Memo1.Lines.Insert(0, Value.Strings[i]);
      end;
    Memo1.SelStart := 0;
  end
  else
  begin
    ShowMessage(MESSAGE03);
    exit;
  end;
end;

// events of Form1
procedure TForm1.FormCreate(Sender: TObject);
var
  b: byte;
begin
  makeuserdir;
  getlang;
  getexepath;
  Form1.Caption := APPNAME + ' v' + VERSION;
  Label6.Caption := Form1.Caption;
  // load configuration
  inifile := untcommonproc.userdir + DIR_CONFIG + 'mm7dread.ini';
  if FileSearch('mm7dread.ini', untcommonproc.userdir + DIR_CONFIG) <> '' then
    if not loadconfiguration(inifile) then
      ShowMessage(MESSAGE01);
  Edit1.Text := untcommonproc.uids;
  for b := 0 to 63 do
    if length(urls[b]) > 0 then
      ComboBox1.Items.Add(untcommonproc.urls[b]);
  // others
  untcommonproc.Value := TStringList.Create;
  ValueListEditor1.Cells[0, 0] := MESSAGE05;
  ValueListEditor1.Cells[1, 0] := MESSAGE06;
  ValueListEditor1.Cells[0, 1] := MESSAGE07;
  ValueListEditor1.Cells[0, 2] := MESSAGE08;
  ValueListEditor1.Cells[0, 3] := MESSAGE09;
  ValueListEditor1.Cells[0, 4] := MESSAGE10;
  ValueListEditor1.Cells[0, 5] := MESSAGE11;
  ValueListEditor1.Cells[0, 6] := MESSAGE12;
  ValueListEditor1.Cells[0, 7] := MESSAGE13;
  ValueListEditor1.Cells[0, 8] := MESSAGE14;
  ValueListEditor1.Cells[0, 9] := MESSAGE15;
  ValueListEditor1.Cells[0, 10] := MESSAGE16;
  ValueListEditor1.Cells[0, 11] := MESSAGE17;
  ValueListEditor1.Cells[0, 12] := MESSAGE18;
  ValueListEditor1.Cells[0, 13] := MESSAGE19;
  ValueListEditor1.Cells[0, 14] := MESSAGE20;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  b: byte;
begin
  for b := 0 to 63 do
    untcommonproc.urls[b] := '';
  if ComboBox1.Items.Count > 0 then
    for b := 0 to ComboBox1.Items.Count - 1 do
      untcommonproc.urls[b] := ComboBox1.Items.Strings[b];
  untcommonproc.uids := Edit1.Text;
  if not saveconfiguration(inifile) then
    ShowMessage(MESSAGE02);
  untcommonproc.Value.Free;
end;

end.
