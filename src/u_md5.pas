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
  File last update : 2025-02-09T11:03:59.204+01:00
  Signature : c8d6acff10a8095fefd5237e292b79075d171813
  ***************************************************************************
*)

unit u_md5;

interface

/// <summary>Get MD5 code from a string</summary>
/// <param name="AString">String to encode</param>
/// <returns>The function return the MD5 of the AString string.</returns>
/// <remarks>
/// Before Delphi 10 Seattle this function uses IdHashMessageDigest from Iny.
/// Since Delphi 10 Seattle it uses System.Hash.THashMD5 from Embarcadero.
/// </remarks>
function MD5(const AString: String): String;

implementation

uses
{$IF CompilerVersion>=30.0}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
{$ENDIF}
  System.SysUtils;

function MD5(const AString: String): String;
{$IF CompilerVersion>=30.0}
{$ELSE}
var
  ch: string;
{$ENDIF}
begin
{$IF CompilerVersion>=30.0}
  result := THashMD5.GetHashString(AString).ToLower;
{$ELSE}
  with TIdHashMessageDigest5.Create do
  begin
    ch := HashStringAsHex(AString);
    Free;
  end;
  result := ch.ToLower;
{$ENDIF}
end;

end.
