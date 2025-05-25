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
  File last update : 2025-02-09T11:03:59.160+01:00
  Signature : 8611d2c940c6dbade7ed8323cf43e07b7ea530d4
  ***************************************************************************
*)

unit FMXForm;

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
  FMX.Objects;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Timer1.tag := -1;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var
  bmp: tbitmap;
begin
  Timer1.tag := Timer1.tag + 1;
  if (Timer1.tag >= length(SVGCursorSVGSamples)) then
    Timer1.tag := 0;

  bmp := SVGToBitmap(trunc(Image1.Width), trunc(Image1.Height),
    SVGCursorSVGSamples[Timer1.tag], Image1.Bitmap.BitmapScale);
  try
    Image1.Bitmap.Assign(bmp);
  finally
    bmp.free;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
