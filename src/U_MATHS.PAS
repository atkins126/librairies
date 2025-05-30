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
  File last update : 2025-02-09T11:03:59.187+01:00
  Signature : 5b41d14e166eac517f353fe51d06b47af23a22c6
  ***************************************************************************
*)

Unit U_Maths;               
 { mise � jour : 16 - 12 - 1990. }
Interface
Function Puissance_Ent (X : Real; N : Integer): Real;
         { calcul de x^n avec x r��l et n entier }
Function Sin_Trapeze (A, B : Real; N : Integer): Real;
         { calcul l'int�grale du sinus entre A et B avec N divisions par la
         m�thode des trap�zes. }

{ Fin de l'interface. }

Implementation
Function Puissance_Ent (X : Real; N : Integer): Real;
Var Y : Real;
Begin { puissance_ent }
  Y := 1.0;
  While Not (N=0) Do
    Begin
      If Odd(N)
      Then
        Begin
          Y := Y * X;
          Dec (N);
        End
      Else
        Begin
          X := Sqr (X);
          N := N Div 2
        End
      {endif}
    End;
  Puissance_Ent := Y
End; { puissance_ent }

Function Sin_Trapeze (A, B : Real; N : Integer): Real;
Var I    : Integer;
    P, Y : Real;
Begin { sin_trapeze }
  P := (B-A)/N;
  Y := (Sin (A)+Sin (B))/2.0;
  For I := 1 To (N-1) Do
    Begin
      A := A+P;
      Y := Y+Sin (A)
    End;
  {endfor}
  Sin_Trapeze := P*Y
End; { sin_trapeze }

End. { Maths }

