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
  File last update : 2025-02-09T11:03:59.090+01:00
  Signature : c54d26f104e081f6d1055996cb371af0a8a9e5d3
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
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  IdHashMessageDigest,
  u_md5;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.Add(Edit1.text);
  Memo1.lines.Add(md5(Edit1.text));
  Memo1.lines.Add('--------------------');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.text := 'A record that implements the MD5 hash type.';
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
