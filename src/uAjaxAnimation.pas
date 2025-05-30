﻿(* C2PP
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
  File last update : 2025-02-09T11:03:59.201+01:00
  Signature : 4de3e8d02afecbacc3e28d5a3ad983caaeb12fc8
  ***************************************************************************
*)

unit uAjaxAnimation;

// (c) Patrick Prémartin / Olf Software 02/2017

interface

uses
  fmx.objects;

procedure ajax_animation_set(zone: TRectangle);
procedure ajax_animation_on;
procedure ajax_animation_off(reinitialiser: boolean = false);

implementation

var
  compteur: integer;
  zoneAjax: TRectangle;

procedure ajax_animation_set(zone: TRectangle);
begin
  zoneAjax := zone;
end;

procedure ajax_animation_on;
begin
  if Assigned(zoneAjax) then
  begin
    if (compteur < 1) then
    begin
      compteur := 1;
      zoneAjax.visible := true;
      zoneAjax.BringToFront;
    end
    else
      inc(compteur);
  end;
end;

procedure ajax_animation_off(reinitialiser: boolean = false);
begin
  if Assigned(zoneAjax) then
  begin
    if reinitialiser then
    begin
      compteur := 0;
      zoneAjax.visible := false;
    end
    else if (compteur > 1) then
      dec(compteur)
    else
    begin
      compteur := 0;
      zoneAjax.visible := false;
    end;
  end;
end;

initialization

compteur := 0;
zoneAjax := nil;

end.
