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
  File last update : 2025-02-09T11:03:59.114+01:00
  Signature : a5478b33e3cc2d42297638f96d2febe0f75bea00
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
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    btnSHL: TButton;
    btnSHR: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    procedure btnSHLClick(Sender: TObject);
    procedure btnSHRClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.RTL.Maths.Conversions;

procedure TForm2.btnSHLClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shl Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shl ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

procedure TForm2.btnSHRClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shr Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shr ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

end.
