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
  Signature : 74118b6b224050607c5751a9e08a1e51d0b8d96c
  ***************************************************************************
*)

unit u_pack;
{
Auteur: Patrick Premartin
Date: 28/12/1991. 25/06/1992.
Langage: Turbo Pascal 6.0
But: Proc�dures de compactage/d�compactage de donn�es.
}
interface
procedure pack (deb : pointer; nb_deb : word; var res : pointer; var nb_res : word);
procedure unpack (deb : pointer; var res : pointer; var nb_res : word);

implementation
type
    prec = ^trec;
    trec = array [1..65535] of byte;

procedure pack (deb : pointer; nb_deb : word; var res : pointer; var nb_res : word);
var
   tampon,
   page : prec;
   i,
   j : word;
   k : byte;
begin
     page := deb;
     getmem (tampon, nb_deb);
     if (tampon <> nil)
     then
         begin
              tampon^ [1] := nb_deb div 256;
              tampon^ [2] := nb_deb mod 256;
              i := 2; { Index dans donn�es de d�part � compacter. } {page}
              j := 3; { Index dans donn�es compact�es d'arriv�e. } {tampon}
              k := 0; { Nombre d'it�ration d'une m�me valeur. }
              while not ((i > nb_deb) or (j > nb_deb)) do
                    begin
                         if ((page^ [i] = page^ [pred (i)]) and (k < 255))
                         then
                             inc (k)
                         else
                             if ((page^ [pred (i)] = 27) or (k > 1))
                             then
                                 begin
                                      tampon^ [j] := 27;
                                      inc (j);
                                      tampon^ [j] := page^ [pred (i)];
                                      inc (j);
                                      tampon^ [j] := k;
                                      inc (j);
                                      k := 0;
                                 end
                             else
                                 if (k = 1)
                                 then
                                     begin
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                          k := 0;
                                     end
                                 else
                                     begin
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                     end;
                                 {endif}
                             {endif}
                         {endif}
                         inc (i);
                    end;
              {endwhile}
              if ((page^ [pred (i)] = 27) or (k > 1))
              then
                  begin
                       tampon^ [j] := 27;
                       inc (j);
                       tampon^ [j] := page^ [pred (i)];
                       inc (j);
                       tampon^ [j] := k;
                       inc (j);
                       k := 0;
                  end
              else
                  if (k = 1)
                  then
                      begin
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                           k := 0;
                      end
                  else
                      begin
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                      end;
                  {endif}
              {endif}
         end;
     {endif}
     nb_res := pred (j);
     if (nb_res < nb_deb)
     then
         begin
              getmem (res, nb_res);
              if ((res <> nil) and (tampon <> nil))
              then
                  move (tampon^, res^, nb_res);
              {endif}
              if (tampon <> nil)
              then
                  freemem (tampon, nb_deb);
              {endif}
         end
     else
         begin
              if (tampon <> nil)
              then
                  move (deb^, tampon^, nb_deb);
              {endif}
              res := tampon;
              nb_res := nb_deb;
         end;
     {endif}
end; { Pack }

procedure unpack (deb : pointer; var res : pointer; var nb_res : word);
var
   tampon,
   page : prec;
   i,
   j,
   l_max : word;
   k : word;
begin
     page := deb;
     l_max := page^ [1]*256+page^ [2];
     getmem (tampon, l_max);
     if (tampon <> nil)
     then
         begin
              i := 1;
              j := 3;
              while (i <= l_max) do
                    begin
                         if (page^ [j] = 27) {ESC}
                         then
                             begin
                                  inc (j);
                                  k := page^ [succ (j)];
                                  k := succ (k);
                                  repeat
                                        tampon^ [i] := page^ [j];
                                        inc (i);
                                        dec (k);
                                  until (k < 1);
                                  inc (j);
                             end
                         else
                             begin
                                  tampon^ [i] := page^ [j];
                                  inc (i);
                             end;
                         {endif}
                         inc (j);
                    end;
              {endwhile}
         end;
     {endif}
     res := tampon;
     nb_res := l_max;
end; { Unpack }

end. { Fin de l'unit� 'U_Pack'. }
