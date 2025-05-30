(* C2PP
  ***************************************************************************

  My libraries for Delphi

  Copyright 1990-2025 Patrick Pr�martin under AGPL 3.0 license.

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
  Signature : 6b0832905442f4e902a11abefc5b5b4d0dcfee1b
  ***************************************************************************
*)

unit uKeyboardSpecialKeyTest;
{
  **********
  * Test d'appui des touches sp�ciales du clavier
  **********

  Liste des modifications :
  04/04/2017, Patrick Pr�martin (Olf Software) : version Windows/Mac du programme (utilisable en VCL et Firemonkey)
  28/08/2023, pprem : updated for iOS/Android and Linux
}

interface

type
  tKeyboardSpecialKeyTestPosition = (Left, Right, All, Any);

  tKeyboardSpecialKeyTest = class
    class function isAltDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isCtrlDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isShiftDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isWindowsDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isCommandDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
  end;

implementation

{$IF Defined(MSWINDOWS)}

uses windows;
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}

uses macapi.appkit;
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}

type
  tKeyboardSpecialKeyTestKeycode = (Alt, Shift, Ctrl, windows, command);

function isKeyDown(key: tKeyboardSpecialKeyTestKeycode;
  Position: tKeyboardSpecialKeyTestPosition): Boolean;
{$IF Defined(MSWINDOWS)}
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}
begin
  result := false;
{$IF Defined(MSWINDOWS)}
  case Position of
    Left:
      case key of
        Alt:
          result := (getKeyState(VK_LMENU) < 0);
        Shift:
          result := (getKeyState(VK_LSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_LCONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0);
      end;
    Right:
      case key of
        Alt:
          result := (getKeyState(VK_RMENU) < 0);
        Shift:
          result := (getKeyState(VK_RSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_RCONTROL) < 0);
        windows:
          result := (getKeyState(VK_RWIN) < 0);
      end;
    Any:
      case key of
        Alt:
          result := (getKeyState(VK_MENU) < 0);
        Shift:
          result := (getKeyState(VK_SHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_CONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0) or (getKeyState(VK_RWIN) < 0);
      end;
    All:
      case key of
        Alt:
          result := (getKeyState(VK_LMENU) < 0) and (getKeyState(VK_RMENU) < 0);
        Shift:
          result := (getKeyState(VK_LSHIFT) < 0) and
            (getKeyState(VK_RSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_LCONTROL) < 0) and
            (getKeyState(VK_RCONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0) and (getKeyState(VK_RWIN) < 0);
      end;
  end;
{$ELSEIF Defined(IOS)}
  // non g�r�
{$ELSEIF Defined(MACOS)}
  // non g�r�
//  Event := TNSEvent.Create;
//  if (Event.modifierFlags <> 0) then
//    result := true
//  else
//    result := false;
{$ELSEIF Defined(ANDROID)}
  // non g�r�
{$ELSEIF Defined(LINUX)}
  // non g�r�
{$ENDIF}
end;

{ tKeyboardSpecialKeyTest }

class function tKeyboardSpecialKeyTest.isAltDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Alt, Position);
end;

class function tKeyboardSpecialKeyTest.isCommandDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
{$IF Defined(MSWINDOWS)}
  result := isWindowsDown(Position);
{$ELSEIF Defined(IOS)}
result := false;
{$ELSEIF Defined(MACOS)}
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.command, Position);
{$ELSEIF Defined(ANDROID)}
result := false;
{$ELSEIF Defined(LINUX)}
result := false;
{$ENDIF}
end;

class function tKeyboardSpecialKeyTest.isCtrlDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Ctrl, Position);
end;

class function tKeyboardSpecialKeyTest.isShiftDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Shift, Position);
end;

class function tKeyboardSpecialKeyTest.isWindowsDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
{$IF Defined(MSWINDOWS)}
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.windows, Position);
{$ELSEIF Defined(IOS)}
result := false;
{$ELSEIF Defined(MACOS)}
  result := isCommandDown(Position);
{$ELSEIF Defined(ANDROID)}
result := false;
{$ELSEIF Defined(LINUX)}
result := false;
{$ENDIF}
end;

end.
