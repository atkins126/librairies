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
  Signature : 58ba7637ee43bc04aa92893d692ed59e8b6e5a9f
  ***************************************************************************
*)

unit Olf.RTL.Params;

interface

uses
  System.Classes,
  System.JSON;

type
  TParamsFile = class;

  /// <summary>
  /// Method signature for load/save events in TParamsFile
  /// </summary>
  TParamsLoadSaveEvent = procedure(Const AParamsFile: TParamsFile) of object;
  /// <summary>
  /// Procedure signature for load/save events in TParamsFile
  /// </summary>
  TParamsLoadSaveProc = reference to procedure(Const AParamsFile: TParamsFile);

  /// <summary>
  /// Method signature for the crypt event in TParamsFile
  /// </summary>
  TParamsCryptEvent = function(Const AParams: string): TStream of object;
  /// <summary>
  /// Procedure signature for the crypt event in TParamsFile
  /// </summary>
  TParamsCryptProc = reference to function(Const AParams: string): TStream;

  /// <summary>
  /// Method signature for the decrypt event in TParamsFile
  /// </summary>
  TParamsDecryptEvent = function(Const AStream: TStream): string of object;
  /// <summary>
  /// Procedure signature for the decrypt event in TParamsFile
  /// </summary>
  TParamsDecryptProc = reference to function(Const AStream: TStream): string;

  /// <summary>
  /// TParamsFile work as an instance of a settings file.
  /// You can have more than one instance for more than 1 settings file.
  /// </summary>
  TParamsFile = class(TObject)
  private
    FBeginUpdateLevel: integer;
    FParamChanged: boolean;
    FParamList: TJSONObject;
    FFolderName: string;
    FFileName: string;
    FonAfterSaveEvent: TParamsLoadSaveEvent;
    FonBeforeLoadEvent: TParamsLoadSaveEvent;
    FonBeforeSaveEvent: TParamsLoadSaveEvent;
    FonAfterLoadEvent: TParamsLoadSaveEvent;
    FonAfterSaveProc: TParamsLoadSaveProc;
    FonBeforeLoadProc: TParamsLoadSaveProc;
    FonBeforeSaveProc: TParamsLoadSaveProc;
    FonAfterLoadProc: TParamsLoadSaveProc;
    FonDecryptEvent: TParamsDecryptEvent;
    FonDecryptProc: TParamsDecryptProc;
    FonCryptEvent: TParamsCryptEvent;
    FonCryptProc: TParamsCryptProc;
    FPortableMode: boolean;
    procedure SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
    procedure SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
    procedure SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
    procedure SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
    procedure SetonCryptEvent(const Value: TParamsCryptEvent);
    procedure SetonCryptProc(const Value: TParamsCryptProc);
    procedure SetonDecryptEvent(const Value: TParamsDecryptEvent);
    procedure SetonDecryptProc(const Value: TParamsDecryptProc);
    procedure SetPortableMode(const Value: boolean);
  protected
    function getParamsFileName(ACreateFolder: boolean = False): string;
    function getParamValue(key: string): TJSONValue;
    procedure setParamValue(key: string; Value: TJSONValue);
  public
    /// <summary>
    /// Class constructor wich just initialize private fields.
    /// </summary>
    constructor Create; overload;
    /// <summary>
    /// Class constructor wich loads the parameter file specified as parameter.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path to the parameter file (drive+folder+file name+extension)
    /// </param>
    constructor Create(AFilePath: string); overload;
    /// <summary>
    /// Instance destructor
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// Save current parameters to actual parameter file
    /// </summary>
    procedure Save;
    /// <summary>
    /// Load parameters from actual parameter file
    /// </summary>
    procedure Load;
    /// <summary>
    /// Cancel current changes and reload previous saved values
    /// </summary>
    procedure Cancel;
    /// <summary>
    /// Delete the file where settings are stored.
    /// </summary>
    /// <remarks>
    /// WARNING !!! No rollback. Deleting the file can't be canceled.
    /// </remarks>
    procedure Delete(const ClearMemoryToo: boolean = true);
    /// <summary>
    /// Clear current parameters list
    /// </summary>
    procedure Clear;
    /// <summary>
    /// Get the string value for key parameter with an empty string as default value
    /// </summary>
    function getValue(key: string; default: string = ''): string; overload;
    /// <summary>
    /// Get the boolean value for key parameter with False as default value
    /// </summary>
    function getValue(key: string; default: boolean = False): boolean; overload;
    /// <summary>
    /// Get the cardinal value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: cardinal = 0): cardinal; overload;
    /// <summary>
    /// Get the integer value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: integer = 0): integer; overload;
    /// <summary>
    /// Get the single value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: single = 0): single; overload;
    /// <summary>
    /// Get the TDateTime value for key parameter with December 30th 1899 at 12:00  as default value
    /// </summary>
    function getValue(key: string; default: TDateTime = 0): TDateTime; overload;
    /// <summary>
    /// Get the JSON value for key parameter with nil as default value
    /// </summary>
    function getValue(key: string; default: TJSONValue = nil)
      : TJSONValue; overload;
    /// <summary>
    /// Set the value for key parameter as string
    /// </summary>
    procedure setValue(key, Value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    procedure setValue(key: string; Value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as cardinal
    /// </summary>
    procedure setValue(key: string; Value: cardinal); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    procedure setValue(key: string; Value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    procedure setValue(key: string; Value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    procedure setValue(key: string; Value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    procedure setValue(key: string; Value: TJSONValue); overload;
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFolderName">
    /// Absolute folder path where you want to save the parameter file.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call the Load procedure after changing the folder.
    /// </param>
    /// <remarks>
    /// To change the file name, use setFilePath() instead of setFolderName().
    /// </remarks>
    procedure setFolderName(AFolderName: string; AReload: boolean = true);
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call Load procedure after changing the file path.
    /// </param>
    /// <remarks>
    /// If you only want to change the path to the parameter file, use setFolderName procedure instead of this one.
    /// </remarks>
    procedure setFilePath(AFilePath: string; AReload: boolean = true);
    /// <summary>
    /// Initialise the folder and the filename with a new default tree:
    /// => "Documents / Editor / Software" for DEBUG and iOS
    /// => "AppData (HomePath) / Editor / Software" in RELEASE (except iOS)
    /// </summary>
    procedure InitDefaultFileNameV2(Const AEditor, ASoftware: string;
      AReload: boolean = true);
    /// <summary>
    /// Move actual parameter file to the new file.
    /// </summary>
    /// <param name="ANewFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="ASave">
    /// If set to True, save actual values to the parameter file.
    /// If set to false, just move the parameter file to it's new folder/filename.
    /// </param>
    /// <param name="ACreateFolder">
    /// If set to True, create the folder of file parameter if it doesn't exists.
    /// </param>
    procedure MoveToFilePath(ANewFilePath: string; ASave: boolean = true;
      ACreateFolder: boolean = False);
    /// <summary>
    /// Return the absolute path to the parameter file (drive+folder+file name+extension)
    /// </summary>
    function getFilePath: string;
    /// <summary>
    /// Return the current parameters as a serialized JSON object.
    /// </summary>
    function ToJSON: string;
    /// <summary>
    /// Return the current parameters as a JSON object
    /// </summary>
    /// <param name="AClone">
    /// If set to True (by default), the result is a clone of actual object. Free it when you have finished to work with it or you'll have memory leaks in your projects.
    /// If set to False, the result is a reference to the internal JSON object. All changes are made to it. Don't destroy it or you'll have Access Violation exception.
    /// </param>
    function AsJSONObject(AClone: boolean = true): TJSONObject;
    /// <summary>
    /// Called before loading the settings file.
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    property onBeforeLoadEvent: TParamsLoadSaveEvent read FonBeforeLoadEvent
      write SetonBeforeLoadEvent;
    property onBeforeLoadProc: TParamsLoadSaveProc read FonBeforeLoadProc
      write SetonBeforeLoadProc;
    /// <summary>
    /// Called after loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    property onAfterLoadEvent: TParamsLoadSaveEvent read FonAfterLoadEvent
      write SetonAfterLoadEvent;
    property onAfterLoadProc: TParamsLoadSaveProc read FonAfterLoadProc
      write SetonAfterLoadProc;
    /// <summary>
    /// Called before saving the settings file
    /// </summary>
    property onBeforeSaveEvent: TParamsLoadSaveEvent read FonBeforeSaveEvent
      write SetonBeforeSaveEvent;
    property onBeforeSaveProc: TParamsLoadSaveProc read FonBeforeSaveProc
      write SetonBeforeSaveProc;
    /// <summary>
    /// Called after saving the settings file
    /// </summary>
    property onAfterSaveEvent: TParamsLoadSaveEvent read FonAfterSaveEvent
      write SetonAfterSaveEvent;
    property onAfterSaveProc: TParamsLoadSaveProc read FonAfterSaveProc
      write SetonAfterSaveProc;
    /// <summary>
    /// Called before saving the parameters in a file (and after onBeforeSave).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    property onCryptEvent: TParamsCryptEvent read FonCryptEvent
      write SetonCryptEvent;
    property onCryptProc: TParamsCryptProc read FonCryptProc
      write SetonCryptProc;
    /// <summary>
    /// Called after loading the parameters from a file (and before onAfterLoad).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    property onDecryptEvent: TParamsDecryptEvent read FonDecryptEvent
      write SetonDecryptEvent;
    property onDecryptProc: TParamsDecryptProc read FonDecryptProc
      write SetonDecryptProc;
    /// <summary>
    /// Portable mode : if true nothing is saved nor loaded.
    /// Default value is false.
    /// </summary>
    property PortableMode: boolean read FPortableMode write SetPortableMode;
    /// <summary>
    /// Retire une cl� des param�tres
    /// </summary>
    procedure Remove(key: string);
    /// <summary>
    /// Returns True if a setting has changed and not been saved.
    /// </summary>
    function HasChanged: boolean;
    /// <summary>
    /// Allow parameters changes but delay the Save operation to the EndUpdate call.
    /// </summary>
    /// <remarks>
    /// If you call BeginUpdate you MUST call its EndUpdate.
    /// Use a try... finally... end !
    /// </remarks>
    procedure BeginUpdate;
    /// <summary>
    /// Closes the block of code started with BeginUpdate. If you did some changes, it saves them by default.
    /// </summary>
    /// <remarks>
    /// If you call BeginUpdate you MUST call its EndUpdate.
    /// Use a try... finally... end !
    /// </remarks>
    procedure EndUpdate(const AutoSaveChanges: boolean = true);
  end;

  /// <summary>
  /// Use TParams with its class methods if you only want to manage one settings file. If you need more than 1 file to store your settings in the same project, crreate instances of TParamsFile instead of using TParams.
  /// </summary>
  /// <remarks>
  /// TParams is here for compatibility with old projects.
  /// </remarks>
  TParams = class(TObject)
  private
    class procedure SetonAfterLoadEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonAfterLoadProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonAfterSaveEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonAfterSaveProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonBeforeLoadEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonBeforeLoadProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonBeforeSaveEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonBeforeSaveProc(const Value
      : TParamsLoadSaveProc); static;
    class function GetonAfterLoadEvent: TParamsLoadSaveEvent; static;
    class function GetonAfterLoadProc: TParamsLoadSaveProc; static;
    class function GetonAfterSaveEvent: TParamsLoadSaveEvent; static;
    class function GetonAfterSaveProc: TParamsLoadSaveProc; static;
    class function GetonBeforeLoadEvent: TParamsLoadSaveEvent; static;
    class function GetonBeforeLoadProc: TParamsLoadSaveProc; static;
    class function GetonBeforeSaveEvent: TParamsLoadSaveEvent; static;
    class function GetonBeforeSaveProc: TParamsLoadSaveProc; static;
    class function GetonCryptEvent: TParamsCryptEvent; static;
    class function GetonCryptProc: TParamsCryptProc; static;
    class function GetonDecryptEvent: TParamsDecryptEvent; static;
    class function GetonDecryptProc: TParamsDecryptProc; static;
    class procedure SetonCryptEvent(const Value: TParamsCryptEvent); static;
    class procedure SetonCryptProc(const Value: TParamsCryptProc); static;
    class procedure SetonDecryptEvent(const Value: TParamsDecryptEvent); static;
    class procedure SetonDecryptProc(const Value: TParamsDecryptProc); static;
    class function GetPortableMode: boolean; static;
    class procedure SetPortableMode(const Value: boolean); static;
  public
    /// <summary>
    /// Save current parameters to actual parameter file
    /// </summary>
    class procedure Save;
    /// <summary>
    /// Load parameters from actual parameter file
    /// </summary>
    class procedure Load;
    /// <summary>
    /// Cancel current changes and reload previous saved values
    /// </summary>
    class procedure Cancel;
    /// <summary>
    /// Delete the file where settings are stored.
    /// </summary>
    /// <remarks>
    /// WARNING !!! No rollback. Deleting the file can't be canceled.
    /// </remarks>
    class procedure Delete(const ClearMemoryToo: boolean = true);
    /// <summary>
    /// Clear current parameters list
    /// </summary>
    class procedure Clear;
    /// <summary>
    /// Get the string value for key parameter with an empty string as default value
    /// </summary>
    class function getValue(key: string; default: string = ''): string;
      overload;
    /// <summary>
    /// Get the boolean value for key parameter with False as default value
    /// </summary>
    class function getValue(key: string; default: boolean = False)
      : boolean; overload;
    /// <summary>
    /// Get the integer value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: integer = 0)
      : integer; overload;
    /// <summary>
    /// Get the cardinal value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: cardinal = 0)
      : integer; overload;
    /// <summary>
    /// Get the single value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: single = 0): single; overload;
    /// <summary>
    /// Get the TDateTime value for key parameter with December 30th 1899 at 12:00  as default value
    /// </summary>
    class function getValue(key: string; default: TDateTime = 0)
      : TDateTime; overload;
    /// <summary>
    /// Get the JSON value for key parameter with nil as default value
    /// </summary>
    class function getValue(key: string; default: TJSONValue = nil)
      : TJSONValue; overload;
    /// <summary>
    /// Set the value for key parameter as string
    /// </summary>
    class procedure setValue(key, Value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    class procedure setValue(key: string; Value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    class procedure setValue(key: string; Value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as cardinal
    /// </summary>
    class procedure setValue(key: string; Value: cardinal); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    class procedure setValue(key: string; Value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    class procedure setValue(key: string; Value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    class procedure setValue(key: string; Value: TJSONValue); overload;
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFolderName">
    /// Absolute folder path where you want to save the parameter file.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call the Load procedure after changing the folder.
    /// </param>
    /// <remarks>
    /// To change the file name, use setFilePath() instead of setFolderName().
    /// </remarks>
    class procedure setFolderName(AFolderName: string; AReload: boolean = true);
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call Load procedure after changing the file path.
    /// </param>
    /// <remarks>
    /// If you only want to change the path to the parameter file, use setFolderName procedure instead of this one.
    /// </remarks>
    class procedure setFilePath(AFilePath: string; AReload: boolean = true);
    /// <summary>
    /// Initialise the folder and the filename with a new default tree:
    /// => "Documents / Editor / Software" for DEBUG and iOS
    /// => "AppData (HomePath) / Editor / Software" in RELEASE (except iOS)
    /// </summary>
    class procedure InitDefaultFileNameV2(Const AEditor, ASoftware: string;
      AReload: boolean = true);
    /// <summary>
    /// Move actual parameter file to the new file.
    /// </summary>
    /// <param name="ANewFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="ASave">
    /// If set to True, save actual values to the parameter file.
    /// If set to false, just move the parameter file to it's new folder/filename.
    /// </param>
    /// <param name="ACreateFolder">
    /// If set to True, create the folder of file parameter if it doesn't exists.
    /// </param>
    class procedure MoveToFilePath(ANewFilePath: string; ASave: boolean = true;
      ACreateFolder: boolean = False);
    /// <summary>
    /// Return the absolute path to the parameter file (drive+folder+file name+extension)
    /// </summary>
    class function getFilePath: string;
    /// <summary>
    /// Return the current parameters as a serialized JSON object.
    /// </summary>
    class function ToJSON: string;
    /// <summary>
    /// Return the current parameters as a JSON object
    /// </summary>
    class function AsJSONObject: TJSONObject;
    /// <summary>
    /// Called before loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    class property onBeforeLoadEvent: TParamsLoadSaveEvent
      read GetonBeforeLoadEvent write SetonBeforeLoadEvent;
    class property onBeforeLoadProc: TParamsLoadSaveProc
      read GetonBeforeLoadProc write SetonBeforeLoadProc;
    /// <summary>
    /// Called after loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    class property onAfterLoadEvent: TParamsLoadSaveEvent
      read GetonAfterLoadEvent write SetonAfterLoadEvent;
    class property onAfterLoadProc: TParamsLoadSaveProc read GetonAfterLoadProc
      write SetonAfterLoadProc;
    /// <summary>
    /// Called before saving the settings file
    /// </summary>
    /// <remarks>
    /// The finalization of this unit calls the TParams.Save. If you have a BeforeSaveEvent or BeforeEventProc, beware of potential access violation by using something perhaps already destroyed.
    /// </remarks>
    class property onBeforeSaveEvent: TParamsLoadSaveEvent
      read GetonBeforeSaveEvent write SetonBeforeSaveEvent;
    class property onBeforeSaveProc: TParamsLoadSaveProc
      read GetonBeforeSaveProc write SetonBeforeSaveProc;
    /// <summary>
    /// Called after saving the settings file
    /// </summary>
    /// <remarks>
    /// The finalization of this unit calls the TParams.Save. If you have a BeforeSaveEvent or BeforeEventProc, beware of potential access violation by using something perhaps already destroyed.
    /// </remarks>
    class property onAfterSaveEvent: TParamsLoadSaveEvent
      read GetonAfterSaveEvent write SetonAfterSaveEvent;
    class property onAfterSaveProc: TParamsLoadSaveProc read GetonAfterSaveProc
      write SetonAfterSaveProc;
    /// <summary>
    /// Called before saving the parameters in a file (and after onBeforeSave).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    class property onCryptEvent: TParamsCryptEvent read GetonCryptEvent
      write SetonCryptEvent;
    class property onCryptProc: TParamsCryptProc read GetonCryptProc
      write SetonCryptProc;
    /// <summary>
    /// Called after loading the parameters from a file (and before onAfterLoad).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    class property onDecryptEvent: TParamsDecryptEvent read GetonDecryptEvent
      write SetonDecryptEvent;
    class property onDecryptProc: TParamsDecryptProc read GetonDecryptProc
      write SetonDecryptProc;
    /// <summary>
    /// Portable mode : if true nothing is done on the storage.
    /// Default value is false.
    /// </summary>
    class property PortableMode: boolean read GetPortableMode
      write SetPortableMode;
    /// <summary>
    /// Retire une cl� des param�tres
    /// </summary>
    class procedure Remove(key: string);
    class function HasChanged: boolean;
    /// <summary>
    /// Allow parameters changes but delay the Save operation to the EndUpdate call.
    /// </summary>
    /// <remarks>
    /// If you call BeginUpdate you MUST call its EndUpdate.
    /// Use a try... finally... end !
    /// </remarks>
    class procedure BeginUpdate;
    /// <summary>
    /// Closes the block of code started with BeginUpdate. If you did some changes, it saves them by default.
    /// </summary>
    /// <remarks>
    /// If you call BeginUpdate you MUST call its EndUpdate.
    /// Use a try... finally... end !
    /// </remarks>
    class procedure EndUpdate(const AutoSaveChanges: boolean = true);
  end;

implementation

uses
  System.Generics.collections,
  System.IOUtils,
  System.SysUtils;

{ TParamsFile }

function TParamsFile.getParamsFileName(ACreateFolder: boolean): string;
var
  Folder: string;
  FileName: string;
  AppName: string;
  Extension: string;
begin
  AppName := TPath.GetFileNameWithoutExtension(paramstr(0));

  if Assigned(onCryptEvent) or Assigned(onCryptProc) or Assigned(onDecryptEvent)
    or Assigned(onDecryptProc) then
    Extension := '.parc'
  else
    Extension := '.par';

  // get filename
  if FFileName.IsEmpty then
  begin
{$IF Defined(DEBUG)}
    FileName := AppName + '-debug' + Extension;
{$ELSE IF Defined(RELEASE)}
    FileName := AppName + Extension;
{$ELSE}
{$MESSAGE FATAL 'not implemented'}
{$ENDIF} end
  else
    FileName := FFileName;

  // get folder name
  if FFolderName.IsEmpty then
    Folder := TPath.Combine(TPath.GetDocumentsPath, AppName)
  else
    Folder := FFolderName;
  if ACreateFolder and (not tdirectory.Exists(Folder)) and (not FPortableMode)
  then
    tdirectory.CreateDirectory(Folder);

  // get file path
  result := TPath.Combine(Folder, FileName);
end;

function TParamsFile.getParamValue(key: string): TJSONValue;
begin
  System.TMonitor.Enter(Self);
  try
    result := nil;
    if Assigned(FParamList) then
      if (FParamList.Count > 0) then
        result := FParamList.getValue(key);
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TParamsFile.setParamValue(key: string; Value: TJSONValue);
begin
  System.TMonitor.Enter(Self);
  try
    if not Assigned(FParamList) then
      FParamList := TJSONObject.Create
    else if (FParamList.Count > 0) and (nil <> FParamList.getValue(key)) then
      FParamList.RemovePair(key).Free;
    FParamList.AddPair(key, Value);
    FParamChanged := true;
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TParamsFile.SetPortableMode(const Value: boolean);
begin
  FPortableMode := Value;
end;

procedure TParamsFile.setValue(key: string; Value: cardinal);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

function TParamsFile.getValue(key: string; default: boolean): boolean;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToBoolean
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: string): string;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: integer): integer;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToInteger
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: single): single;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToSingle
  else
    result := default;
end;

function TParamsFile.AsJSONObject(AClone: boolean): TJSONObject;
begin
  System.TMonitor.Enter(Self);
  try
    if not Assigned(FParamList) then
      result := nil
    else if AClone then
      result := FParamList.Clone as TJSONObject
    else
      result := FParamList;
  finally
    System.TMonitor.Exit(Self);
  end;
end;

constructor TParamsFile.Create;
begin
  FFolderName := '';
  FFileName := '';
  FParamChanged := False;
  FParamList := TJSONObject.Create;

  FonAfterSaveEvent := nil;
  FonBeforeLoadEvent := nil;
  FonBeforeSaveEvent := nil;
  FonAfterLoadEvent := nil;
  FonAfterSaveProc := nil;
  FonBeforeLoadProc := nil;
  FonBeforeSaveProc := nil;
  FonAfterLoadProc := nil;

  FonCryptEvent := nil;
  FonCryptProc := nil;
  FonDecryptEvent := nil;
  FonDecryptProc := nil;

  FPortableMode := False;

  FBeginUpdateLevel := 0;
end;

procedure TParamsFile.BeginUpdate;
begin
  if (FBeginUpdateLevel = FBeginUpdateLevel.MaxValue) then
    raise exception.Create
      ('Missing some EndUpdate. BeginUpdate is called too often.');

  inc(FBeginUpdateLevel);
end;

procedure TParamsFile.Cancel;
begin
  if (FBeginUpdateLevel > 0) then
    raise exception.Create
      ('Can''t cancel the settings in a BeginUpdate/EndUpdate block !');

  Load;
end;

procedure TParamsFile.Clear;
begin
  if (FBeginUpdateLevel > 0) then
    raise exception.Create
      ('Can''t clear the settings in a BeginUpdate/EndUpdate block !');

  System.TMonitor.Enter(Self);
  try
    FParamList.Free;
    FParamList := TJSONObject.Create;
  finally
    System.TMonitor.Exit(Self);
  end;
end;

constructor TParamsFile.Create(AFilePath: string);
begin
  Create;
  setFilePath(AFilePath, true);
end;

procedure TParamsFile.Delete(const ClearMemoryToo: boolean);
begin
  if (FBeginUpdateLevel > 0) then
    raise exception.Create
      ('Can''t delete the settings in a BeginUpdate/EndUpdate block !');

  if ClearMemoryToo then
    Clear;

  if tfile.Exists(getFilePath) then
    tfile.Delete(getFilePath);
end;

destructor TParamsFile.Destroy;
begin
  Save;
  if Assigned(FParamList) then
    FreeAndNil(FParamList);
  inherited;
end;

procedure TParamsFile.EndUpdate(const AutoSaveChanges: boolean);
begin
  if (FBeginUpdateLevel < 1) then
    raise exception.Create
      ('Missing some BeginUpdate. EndUpdate is called too often.');

  dec(FBeginUpdateLevel);
  if (FBeginUpdateLevel = 0) and AutoSaveChanges then
    Save;
end;

function TParamsFile.getFilePath: string;
begin
  result := getParamsFileName;
end;

function TParamsFile.getValue(key: string; default: TDateTime): TDateTime;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := strToDateTime(jsonvalue.Value)
  else
    result := default;
end;

procedure TParamsFile.Load;
var
  FileName: string;
  fs: TFileStream;
  JSON: string;
begin
  if (FBeginUpdateLevel > 0) then
    raise exception.Create
      ('Can''t reload the settings in a BeginUpdate/EndUpdate block !');

  // Call the Before Load event if it exists
  if Assigned(onBeforeLoadProc) then
    onBeforeLoadProc(Self);
  if Assigned(onBeforeLoadEvent) then
    onBeforeLoadEvent(Self);

  // Load the file and its settings
  if not FPortableMode then
  begin
    FileName := getParamsFileName;
    if tfile.Exists(FileName) then
    begin
      if Assigned(FParamList) then
        FreeAndNil(FParamList);
      if Assigned(onDecryptEvent) or Assigned(onDecryptProc) then
      begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        try
          if Assigned(onDecryptEvent) then
            JSON := onDecryptEvent(fs)
          else if Assigned(onDecryptProc) then
            JSON := onDecryptProc(fs)
          else
            JSON := '';
        finally
          fs.Free;
        end;
      end
      else
        try
          JSON := tfile.ReadAllText(FileName, TEncoding.UTF8);
        except
          JSON := '';
        end;
      FParamList := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    end;
  end;

  // Call the After Load event if it exists
  if Assigned(onAfterLoadProc) then
    onAfterLoadProc(Self);
  if Assigned(onAfterLoadEvent) then
    onAfterLoadEvent(Self);
end;

procedure TParamsFile.MoveToFilePath(ANewFilePath: string; ASave: boolean;
  ACreateFolder: boolean);
var
  oldFilePath: string;
  NewPath: string;
begin
  oldFilePath := getFilePath;
  if (oldFilePath <> ANewFilePath) then
  begin
    NewPath := TPath.GetDirectoryName(ANewFilePath);
    if not tdirectory.Exists(NewPath) then
      if ACreateFolder and (not FPortableMode) then
        tdirectory.CreateDirectory(NewPath)
      else
        raise exception.Create('Folder "' + NewPath + '" doesn''t exist.');
    tfile.Move(oldFilePath, ANewFilePath);
    setFilePath(ANewFilePath, False);
    if ASave then
      Save;
  end;
end;

procedure TParamsFile.Remove(key: string);
begin
  System.TMonitor.Enter(Self);
  try
    if (FParamList.Count > 0) and (nil <> FParamList.getValue(key)) then
    begin
      FParamList.RemovePair(key).Free;
      FParamChanged := true;
    end;
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TParamsFile.Save;
var
  FileName: string;
  cs: TStream;
  fs: TFileStream;
begin
  if (FBeginUpdateLevel > 0) then
    Exit;

  // Call the Before Save event if it exists
  if Assigned(onBeforeSaveProc) then
    onBeforeSaveProc(Self);
  if Assigned(onBeforeSaveEvent) then
    onBeforeSaveEvent(Self);

  // Save the settings if anything has changed in this file since previous Save or Load operation
  if FParamChanged and (not FPortableMode) then
  begin
    System.TMonitor.Enter(Self);
    try
      FileName := getParamsFileName(true);
      if Assigned(FParamList) and (FParamList.Count > 0) then
      begin
        cs := nil;
        if Assigned(onCryptEvent) then
          cs := onCryptEvent(FParamList.ToJSON)
        else if Assigned(onCryptProc) then
          cs := onCryptProc(FParamList.ToJSON)
        else
          tfile.WriteAllText(FileName, FParamList.ToJSON, TEncoding.UTF8);
        if Assigned(cs) then
          try
            fs := TFileStream.Create(FileName, fmOpenWrite + fmCreate);
            try
              cs.position := 0;
              fs.CopyFrom(cs);
            finally
              fs.Free;
            end;
          finally
            cs.Free;
          end;
      end
      else if tfile.Exists(FileName) then
        tfile.Delete(FileName);
      FParamChanged := False;
    finally
      System.TMonitor.Exit(Self);
    end;
  end;

  // Call the After Save event if it exists
  if Assigned(onAfterSaveProc) then
    onAfterSaveProc(Self);
  if Assigned(onAfterSaveEvent) then
    onAfterSaveEvent(Self);
end;

procedure TParamsFile.setValue(key: string; Value: single);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: TDateTime);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(DateTimeToStr(Value));
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

function TParamsFile.ToJSON: string;
begin
  System.TMonitor.Enter(Self);
  try
    if Assigned(FParamList) then
      result := FParamList.ToJSON
    else
      result := '';
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TParamsFile.setValue(key, Value: string);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: boolean);
var
  jsonvalue: TJSONBool;
begin
  jsonvalue := TJSONBool.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: integer);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setFilePath(AFilePath: string; AReload: boolean);
begin
  if AFilePath.IsEmpty then
  begin
    FFolderName := '';
    FFileName := '';
  end
  else
  begin
    FFolderName := TPath.GetDirectoryName(AFilePath);
    if not tdirectory.Exists(FFolderName) then
      raise exception.Create('Folder "' + FFolderName + '" doesn''t exist.');
    FFileName := TPath.GetFileName(AFilePath);
  end;
  if AReload then
    Load;
end;

procedure TParamsFile.setFolderName(AFolderName: string; AReload: boolean);
begin
  FFolderName := AFolderName;
  if AReload then
    Load;
end;

procedure TParamsFile.SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  FonAfterLoadEvent := Value;
end;

procedure TParamsFile.SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
begin
  FonAfterLoadProc := Value;
end;

procedure TParamsFile.SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  FonAfterSaveEvent := Value;
end;

procedure TParamsFile.SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
begin
  FonAfterSaveProc := Value;
end;

procedure TParamsFile.SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  FonBeforeLoadEvent := Value;
end;

procedure TParamsFile.SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
begin
  FonBeforeLoadProc := Value;
end;

procedure TParamsFile.SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  FonBeforeSaveEvent := Value;
end;

procedure TParamsFile.SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
begin
  FonBeforeSaveProc := Value;
end;

procedure TParamsFile.SetonCryptEvent(const Value: TParamsCryptEvent);
begin
  FonCryptEvent := Value;
end;

procedure TParamsFile.SetonCryptProc(const Value: TParamsCryptProc);
begin
  FonCryptProc := Value;
end;

procedure TParamsFile.SetonDecryptEvent(const Value: TParamsDecryptEvent);
begin
  FonDecryptEvent := Value;
end;

procedure TParamsFile.SetonDecryptProc(const Value: TParamsDecryptProc);
begin
  FonDecryptProc := Value;
end;

function TParamsFile.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := getParamValue(key);
  if not Assigned(result) then
    result := default;
end;

function TParamsFile.HasChanged: boolean;
begin
  result := FParamChanged;
end;

procedure TParamsFile.InitDefaultFileNameV2(const AEditor, ASoftware: string;
  AReload: boolean);
var
  Folder: string;
begin
  if AEditor.IsEmpty and ASoftware.IsEmpty then
    raise exception.Create('Needs at least an Editor or Software name.');

{$IF Defined(DEBUG) or Defined(IOS)}
  Folder := TPath.GetDocumentsPath;
{$ELSE IF Defined(RELEASE)}
  Folder := TPath.GetHomePath;
{$ELSE}
{$MESSAGE FATAL 'not implemented'}
{$ENDIF}
  //
  if not AEditor.IsEmpty then
{$IFDEF DEBUG}
    Folder := TPath.Combine(Folder, AEditor + '-DEBUG');
{$ELSE}
    Folder := TPath.Combine(Folder, AEditor);
{$ENDIF}
  //
  if not ASoftware.IsEmpty then
{$IFDEF DEBUG}
    Folder := TPath.Combine(Folder, ASoftware + '-DEBUG');
{$ELSE}
    Folder := TPath.Combine(Folder, ASoftware);
{$ENDIF}
  //
  setFolderName(Folder, AReload);
end;

function TParamsFile.getValue(key: string; default: cardinal): cardinal;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToInt64
  else
    result := default;
end;

procedure TParamsFile.setValue(key: string; Value: TJSONValue);
begin
  setParamValue(key, Value);
end;

{ TParams }

var
  DefaultParamsFile: TParamsFile;

class function TParams.AsJSONObject: TJSONObject;
begin
  result := DefaultParamsFile.AsJSONObject(true);
end;

class procedure TParams.BeginUpdate;
begin
  DefaultParamsFile.BeginUpdate;
end;

class procedure TParams.Cancel;
begin
  DefaultParamsFile.Cancel;
end;

class procedure TParams.Clear;
begin
  DefaultParamsFile.Clear;
end;

class procedure TParams.Delete(const ClearMemoryToo: boolean);
begin
  DefaultParamsFile.Delete(ClearMemoryToo);
end;

class procedure TParams.EndUpdate(const AutoSaveChanges: boolean);
begin
  DefaultParamsFile.EndUpdate(AutoSaveChanges);
end;

class function TParams.getFilePath: string;
begin
  result := DefaultParamsFile.getFilePath;
end;

class function TParams.GetonAfterLoadEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onAfterLoadEvent;
end;

class function TParams.GetonAfterLoadProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onAfterLoadProc;
end;

class function TParams.GetonAfterSaveEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onAfterSaveEvent;
end;

class function TParams.GetonAfterSaveProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onAfterSaveProc;
end;

class function TParams.GetonBeforeLoadEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onBeforeLoadEvent;
end;

class function TParams.GetonBeforeLoadProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onBeforeLoadProc;
end;

class function TParams.GetonBeforeSaveEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onBeforeSaveEvent;
end;

class function TParams.GetonBeforeSaveProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onBeforeSaveProc;
end;

class function TParams.GetonCryptEvent: TParamsCryptEvent;
begin
  result := DefaultParamsFile.onCryptEvent;
end;

class function TParams.GetonCryptProc: TParamsCryptProc;
begin
  result := DefaultParamsFile.onCryptProc
end;

class function TParams.GetonDecryptEvent: TParamsDecryptEvent;
begin
  result := DefaultParamsFile.onDecryptEvent;
end;

class function TParams.GetonDecryptProc: TParamsDecryptProc;
begin
  result := DefaultParamsFile.FonDecryptProc
end;

class function TParams.GetPortableMode: boolean;
begin
  result := DefaultParamsFile.PortableMode;
end;

class function TParams.getValue(key: string; default: integer): integer;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: boolean): boolean;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key, default: string): string;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: single): single;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: TDateTime): TDateTime;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.Load;
begin
  DefaultParamsFile.Load;
end;

class procedure TParams.MoveToFilePath(ANewFilePath: string; ASave: boolean;
  ACreateFolder: boolean);
begin
  DefaultParamsFile.MoveToFilePath(ANewFilePath, ASave, ACreateFolder);
end;

class procedure TParams.Remove(key: string);
begin
  DefaultParamsFile.Remove(key);
end;

class procedure TParams.Save;
begin
  DefaultParamsFile.Save;
end;

class procedure TParams.setFilePath(AFilePath: string; AReload: boolean);
begin
  DefaultParamsFile.setFilePath(AFilePath, AReload);
end;

class procedure TParams.setFolderName(AFolderName: string; AReload: boolean);
begin
  DefaultParamsFile.setFolderName(AFolderName, AReload);
end;

class procedure TParams.SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onAfterLoadEvent := Value;
end;

class procedure TParams.SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onAfterLoadProc := Value;
end;

class procedure TParams.SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onAfterSaveEvent := Value;
end;

class procedure TParams.SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onAfterSaveProc := Value;
end;

class procedure TParams.SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onBeforeLoadEvent := Value;
end;

class procedure TParams.SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onBeforeLoadProc := Value;
end;

class procedure TParams.SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onBeforeSaveEvent := Value;
end;

class procedure TParams.SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onBeforeSaveProc := Value;
end;

class procedure TParams.SetonCryptEvent(const Value: TParamsCryptEvent);
begin
  DefaultParamsFile.onCryptEvent := Value;
end;

class procedure TParams.SetonCryptProc(const Value: TParamsCryptProc);
begin
  DefaultParamsFile.onCryptProc := Value;
end;

class procedure TParams.SetonDecryptEvent(const Value: TParamsDecryptEvent);
begin
  DefaultParamsFile.onDecryptEvent := Value;
end;

class procedure TParams.SetonDecryptProc(const Value: TParamsDecryptProc);
begin
  DefaultParamsFile.onDecryptProc := Value;
end;

class procedure TParams.SetPortableMode(const Value: boolean);
begin
  DefaultParamsFile.PortableMode := Value;
end;

class procedure TParams.setValue(key: string; Value: boolean);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key, Value: string);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: TDateTime);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: single);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: integer);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class function TParams.ToJSON: string;
begin
  result := DefaultParamsFile.ToJSON;
end;

class function TParams.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.HasChanged: boolean;
begin
  result := DefaultParamsFile.HasChanged;
end;

class procedure TParams.InitDefaultFileNameV2(const AEditor, ASoftware: string;
  AReload: boolean);
begin
  DefaultParamsFile.InitDefaultFileNameV2(AEditor, ASoftware, AReload);
end;

class function TParams.getValue(key: string; default: cardinal): integer;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.setValue(key: string; Value: TJSONValue);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: cardinal);
begin
  DefaultParamsFile.setValue(key, Value);
end;

initialization

DefaultParamsFile := TParamsFile.Create;
TParams.Load;

finalization

TParams.Save;
if Assigned(DefaultParamsFile) then
  FreeAndNil(DefaultParamsFile);

end.
