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
  Signature : 2eb7fc666a214af5251f37285727233d900d5047
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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm5 = class(TForm)
    FlowLayout1: TFlowLayout;
    Image1: TImage;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples;

procedure TForm5.Button1Click(Sender: TObject);
var
  SVGIndex: TSVGCursorSVGSamplesIndex;
  img: TImage;
  // bmp: TBitmap;
begin
  Button1.visible := false;

  // register SVG images to the bitmap from SVG class
  // ***** do it only one time in a unit 'initialization' or main form creation *****
  for SVGIndex := low(TSVGCursorSVGSamplesIndex)
    to high(TSVGCursorSVGSamplesIndex) do
    TOlfSVGBitmapList.AddItemAt(ord(SVGIndex),
      TSVGCursorSVGSamples.SVG(SVGIndex));

  // add all image on screen
  for SVGIndex := low(TSVGCursorSVGSamplesIndex)
    to high(TSVGCursorSVGSamplesIndex) do
  begin
    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Fit;
    // bmp := TOlfSVGBitmapList.Bitmap(ord(SVGIndex), round(img.Width),
    // round(img.height), Image1.Bitmap.BitmapScale);
    // img.Bitmap.Assign(bmp);
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(ord(SVGIndex), round(img.Width),
      round(img.height), Image1.Bitmap.BitmapScale));
  end;

  ShowMessage(TOlfSVGBitmapList.Count.tostring);
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Image1.visible := false;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
