(* C2PP
  ***************************************************************************

  My libraries for Delphi

  Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2025-02-09T11:03:59.158+01:00
  Signature : 8f838bc9f20323ecf6e9d7ea07e97bc2c39f44be
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Olf.RTL.Streams, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TForm1 = class(TForm, IOlfLoadSaveStreamWithSize)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
  public
    procedure LoadFromStreamWithSize(const AStream: TStream);
    procedure SaveToStreamWithSize(const AStream: TStream);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  ms: tmemorystream;
begin
  ms := tmemorystream.create;
  try
    SaveToStreamWithSize(ms);
    ms.Position := 0;
    LoadFromStreamWithSize(ms);
  finally
    ms.free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tguid.NewGuid.ToString;
end;

procedure TForm1.LoadFromStreamWithSize(const AStream: TStream);
var
  s: string;
begin
  s := LoadStringFromStream(AStream);
  Memo1.lines.add(s);
end;

procedure TForm1.SaveToStreamWithSize(const AStream: TStream);
begin
  SaveStringToStream(Edit1.Text, AStream);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
