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
  Signature : eadafcbc47edef84cbf0b5d0bdc5ff5b6b81a3c5
  ***************************************************************************
*)

unit uGetDeviceName;

interface

function getDeviceName: string;

implementation

{$IF Defined(MSWINDOWS)}

uses system.sysutils;
{$ELSEIF Defined(IOS)}

uses
  iOSapi.CocoaTypes, iOSapi.Foundation, Macapi.ObjectiveC, Macapi.helpers;

type
  UIDeviceClass = interface(NSObjectClass)
    ['{A2DCE998-BF3A-4AB0-9B8D-4182B341C9DF}']
    function currentDevice: Pointer; cdecl;
  end;

  UIDevice = interface(NSObject)
    ['{70BB371D-314A-4BA9-912E-2EF72EB0F558}']
    function localizedModel: NSString; cdecl;
    function model: NSString; cdecl;
    function name: NSString; cdecl;
    function systemName: NSString; cdecl;
    function systemVersion: NSString; cdecl;
    function uniqueIdentifier: NSString; cdecl;
  end;

  TUIDevice = class(TOCGenericImport<UIDeviceClass, UIDevice>)
  end;
{$ELSEIF Defined(MACOS)}

uses Macapi.Foundation, Macapi.helpers;
{$ELSEIF Defined(ANDROID)}

uses Androidapi.JNI.Os, Androidapi.JNI.Provider, Androidapi.helpers;
{$ENDIF}

function getDeviceName: string;
{$IF Defined(IOS)}
var
  hote: UIDevice;
{$ELSEIF Defined(MACOS)}
var
  hote: NSHost;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  Result := GetEnvironmentVariable('COMPUTERNAME');
  if ('' = Result) then
    Result := GetEnvironmentVariable('HOSTNAME');
{$ELSEIF Defined(IOS)}
  hote := TUIDevice.Create;
  Result := NSStrToStr(hote.name);
{$ELSEIF Defined(MACOS)}
  hote := tnshost.Create;
  Result := NSStrToStr(hote.localizedName);
{$ELSEIF Defined(ANDROID)}
  Result := JStringToString(TJSettings_Secure.JavaClass.getString
    (TAndroidHelper.ContentResolver, StringToJString('bluetooth_name')));
  if ('' = Result) then
    Result := JStringToString(TJSettings_Global.JavaClass.getString
      (TAndroidHelper.ContentResolver, StringToJString('device_name')));
  if ('' = Result) then
    Result := JStringToString(tjbuild.JavaClass.model);
  if ('' = Result) then
    Result := JStringToString(tjbuild.JavaClass.DISPLAY);
{$ELSE}
  Result := GetEnvironmentVariable('HOSTNAME');
{$ENDIF}
end;

end.
