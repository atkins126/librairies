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
  Signature : 5136a832e25a19e574fcbfb9bab42cae5450a2ee
  ***************************************************************************
*)

unit u_vibrate;

interface

(*
  Permet le d�clenchement du vibreur du smartphone.
  bas� sur http://community.embarcadero.com/blogs/entry/how-to-vibrate-ios-and-android-phones-using-firemonkey-and-xe8

  Pour Android :
  - activer l'autorisation "vibrate" ou "vibrer" dans les autorisations de l'application

  Pour iOS :
  - ajouter le framework "AudioToolbox" dans le SDK Manager avec comme chemin "$(SDKROOT)/System/Library/Frameworks"

  Pour les autres plateformes :
  - ne rien faire puisqu'elles ne sont pas g�r�es.

  Liste des mises � jour :
  23/05/2016, Patrick Pr�martin : version initiale
*)

/// <summary>Use vibrator's smartphone or tablet (Android / iOS)</summary>
/// <param name="milliseconds">Number of milliseconds the device vibrate for Android. On iOs, there is no delay.</param>
procedure vibrate(milliseconds: cardinal = 500);

implementation

{$IFDEF ANDROID}

uses
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNIBridge;
{$ENDIF}
{$IFDEF IOS}

uses
  IOSapi.MediaPlayer,
  IOSapi.CoreGraphics,
  FMX.Platform,
  FMX.Platform.IOS,
  IOSapi.UIKit,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  IOSapi.Cocoatypes,
  Macapi.CoreFoundation,
  IOSapi.Foundation,
  IOSapi.CoreImage,
  IOSapi.QuartzCore,
  IOSapi.CoreData;

Const
  libAudioToolbox =
    '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  kSystemSoundID_vibrate = $FFF;

Procedure AudioServicesPlaySystemSound(inSystemSoundID: integer); Cdecl;
  External libAudioToolbox Name _PU + 'AudioServicesPlaySystemSound';
{$ENDIF}

procedure vibrate(milliseconds: cardinal);
{$IFDEF ANDROID}
Var
  Vibrator: JVibrator;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Vibrator := TJVibrator.Wrap
    ((SharedActivityContext.getSystemService
    (TJContext.JavaClass.VIBRATOR_SERVICE) as ILocalObject).GetObjectID);
  Vibrator.vibrate(milliseconds);
{$ENDIF}
{$IFDEF IOS}
  AudioServicesPlaySystemSound(kSystemSoundID_vibrate);
{$ENDIF}
end;

end.
