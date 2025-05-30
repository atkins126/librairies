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
  Signature : 89afb2815a58673cac84917676948b1b79035cde
  ***************************************************************************
*)

Unit U_Position;             
{ mise � jour : 16 - 02 - 1990. }
{
Auteur: Pr�martin Patrick.

Cette unit� contient des fonctions compl�mentaires � la fonction POS du
Turbo Pascal 4.0 de Borland.
}
Interface
{ Partie des fonctions. }
Function Len (Chaine : String): Byte;
         { renvoie la longueur de la chaine }
Function Pos_Left (Carac : Char; Chaine : String): Byte;
         { donne la premi�re position de Carac dans Chaine par la gauche. }
Function Pos_Right (Carac : Char; Chaine : String): Byte;
         { donne la premi�re position de Carac dans Chaine par la droite. }
Function Pos_Left_N (Carac : Char; Chaine : String; N : Byte): Byte;
         { donne la n-i�me position de Carac � partir de la gauche. }
Function Pos_Right_N (Carac : Char; Chaine : String; N : Byte): Byte;
         { donne la n-i�me position de Carac � partir de la droite. }
Function Pos_Left_Debut (Carac : Char; Chaine : String; Debut : Byte): Byte;
         { position de Carac � partir du Debut-i�me caract�re de la chaine vers
         la droite. }
Function Pos_Right_Debut (Carac : Char; Chaine : String; Debut : Byte): Byte;
         { position de Carac � partir de Debut-i�me caract�re de la chaine vers
         la gauche. }
Function Pos_Left_Debut_N (Carac:Char; Chaine:String; Debut, N:Byte):Byte;
         { donne la n-i�me position de Carac dans Chaine en partant du Debut-
         i�me caract�re et en allant vers la droite. }
Function Pos_Right_Debut_N (Carac:Char; Chaine:String; Debut, N:Byte):Byte;
         { donne la n-i�me position de Carac dans Chaine en partant du Debut-
         i�me caract�re et en allant vers la gauche. }
Function Non_Pos_Left (Carac : Char; Chaine : String): Byte;
         { cherche la position du premier caract�re diff�rent de carac �
         partir de la gauche de la chaine. }
Function Non_Pos_Right (Carac : Char; Chaine : String): Byte;
         { cherche la position du premier caract�re diff�rent de carac �
         partir de la droite de la chaine. }

{ Fin de l'interface. }

Implementation
Function Len (Chaine : String): Byte;
Begin { len }
  Len := Ord (Chaine [0])
End; { len }

Function Pos_Left_Debut (Carac : Char; Chaine : String; Debut : Byte): Byte;
Var I, Longueur_Chaine : Byte;
Begin { pos_left_debut }
  Longueur_Chaine := Len (Chaine);
  If (Debut < 1) Then Debut := 1;
  If (Longueur_Chaine >= Debut)
  Then
    Begin
      I := Debut;
      While ((I < Longueur_Chaine) And (Chaine [I] <> Carac)) Do Inc (I);
      If (Chaine [I] = Carac)
      Then
        Pos_Left_Debut := I
      Else
        Pos_Left_Debut := 0
      {endif}
    End
  Else
    Pos_Left_Debut := 0
  {endif}
End; { pos_left_debut }

Function Pos_Right_Debut (Carac : Char; Chaine : String; Debut : Byte): Byte;
Var I , Longueur_Chaine : Byte;
Begin { pos_right_debut }
  Longueur_Chaine := Len (Chaine);
  If (Debut < 1) Then Debut := 1;
  If (Longueur_Chaine >= Debut)
  Then
    Begin
      I := Debut;
      While ((I > 1) And (Chaine [I] <> Carac)) Do Dec (I);
      If (Chaine [I] = Carac)
      Then
        Pos_Right_Debut := I
      Else
        Pos_Right_Debut := 0
      {endif}
    End
  Else
    Pos_Right_Debut := 0
  {endif}
End; { pos_right_debut }

Function Pos_Left (Carac : Char; Chaine : String): Byte;
Begin { pos_left }
  Pos_Left := Pos_Left_Debut (Carac, Chaine, 1)
End; { pos_left }

Function Pos_Right (Carac : Char; Chaine : String): Byte;
Begin { pos_right }
  Pos_Right := Pos_Right_Debut (Carac, Chaine, Len (Chaine))
End; { pos_right }

Function Pos_Left_Debut_N (Carac:Char; Chaine:String; Debut, N:Byte): Byte;
Var I    : Byte;
    Sort : Boolean;
Begin { pos_left_debut_n }
  Sort := False;
  If (Debut < 1) Then Debut := 1;
  I := Debut-1;
  While ((N > 0) And (Not Sort)) Do
    Begin
      I := Pos_Left_Debut (Carac, Chaine, I+1);
      Sort := (I=0);
      Dec (N)
    End;
  Pos_Left_Debut_N := I
End; { pos_left_debut_n }

Function Pos_Right_Debut_N (Carac:Char; Chaine:String; Debut, N:Byte): Byte;
Var I    : Byte;
    Sort : Boolean;
Begin { pos_right_debut_n }
  If (Debut < 1) Then Debut := 1;
  I := Pos_Right_Debut (Carac, Chaine, Debut);
  Sort := (I=0);
  Dec (N);
  While ((N > 0) And (Not Sort)) Do
    Begin
      I := Pos_Right_Debut (Carac, Chaine, I-1);
      Sort := (I=0);
      Dec (N)
    End;
  Pos_Right_Debut_N := I
End; { pos_right_debut_n }

Function Pos_Left_N (Carac : Char; Chaine : String; N : Byte): Byte;
Begin { pos_left_n }
  Pos_Left_N := Pos_Left_Debut_N (Carac, Chaine, 1, N)
End; { pos_left_n }

Function Pos_Right_N (Carac : Char; Chaine : String; N : Byte): Byte;
Begin { pos_right_n }
  Pos_Right_N := Pos_Right_Debut_N (Carac, Chaine, Len (Chaine), N)
End; { pos_right_n }

Function Non_Pos_Left (Carac : Char; Chaine : String): Byte;
Var I, Longueur_Chaine : Byte;
Begin { non_pos_left }
  Longueur_Chaine := Len (Chaine);
  If (Longueur_Chaine > 0)
  Then
    Begin
      I := 1;
      While ((I < Longueur_Chaine) And (Chaine [I] = Carac)) Do Inc (I);
      If (Chaine [I] <> Carac)
      Then
        Non_Pos_Left := I
      Else
        Non_Pos_Left := 0
      {endif}
    End
  Else
    Non_Pos_Left := 0
  {endif}
End; { non_pos_left }

Function Non_Pos_Right (Carac : Char; Chaine : String): Byte;
Var I, Longueur_Chaine : Byte;
Begin { non_pos_right }
  Longueur_Chaine := Len (Chaine);
  If (Longueur_Chaine > 0)
  Then
    Begin
      I := Longueur_Chaine;
      While ((I > 1) And (Chaine [I] = Carac)) Do Dec (I);
      If (Chaine [I] <> Carac)
      Then
        Non_Pos_Right := I
      Else
        Non_Pos_Right := 0
      {endif}
    End
  Else
    Non_Pos_Right := 0
  {endif}
End; { non_pos_right }

End. { Position }

