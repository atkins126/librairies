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
  File last update : 2025-02-09T11:03:59.108+01:00
  Signature : 0a2d60cb7c821e99fdc2c4b8af3e7ace2dacc025
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Olf.RTL.GenRandomID;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.add('Base 2 => ' + TOlfRandomIDGenerator.getidbase2
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.lines.add('Base 10 => ' + TOlfRandomIDGenerator.getidbase10
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.lines.add('Base 36 => ' + TOlfRandomIDGenerator.getIDBase36
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.lines.add('Base 62 => ' + TOlfRandomIDGenerator.getIDBase62
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

end.
