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
  File last update : 2025-02-09T11:03:59.185+01:00
  Signature : d6e196b2d004a7a61ff32e687cef6d98d46e5545
  ***************************************************************************
*)

unit Olf.FMX.Streams;

interface

uses
  FMX.Graphics,
  System.SysUtils,
  System.Classes;

procedure SaveBitmapToStream(ABitmap: TBitmap; AToStream: TStream);
function LoadBitmapFromStream(AFromStream: TStream): TBitmap;

implementation

procedure SaveBitmapToStream(ABitmap: TBitmap; AToStream: TStream);
var
  ms: TMemoryStream;
  size: int64;
begin
  if not assigned(AToStream) then
    raise exception.create('Need an existing stream to save the bitmap !');

  if not assigned(ABitmap) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
  begin
    size := 0;
    AToStream.WriteData(size);
  end
  else
  begin
    ms := TMemoryStream.create;
    try
      ABitmap.SaveToStream(ms);
      size := ms.size;
      AToStream.WriteData(size);
      if (size > 0) then
      begin
        ms.Position := 0;
        AToStream.CopyFrom(ms, size);
      end;
    finally
      ms.free;
    end;
  end;
end;

function LoadBitmapFromStream(AFromStream: TStream): TBitmap;
var
  ms: TMemoryStream;
  size: int64;
begin
  if not assigned(AFromStream) then
    raise exception.create('Need an existing stream to load the bitmap !');

  if (AFromStream.ReadData(size) <> sizeof(size)) then
    result := nil
  else if (size < 1) then
    result := nil
  else
  begin
    ms := TMemoryStream.create;
    try
      ms.CopyFrom(AFromStream, size);
      ms.Position := 0;
      result := TBitmap.create;
      result.LoadFromStream(ms);
    finally
      ms.free;
    end;
  end;
end;

end.
