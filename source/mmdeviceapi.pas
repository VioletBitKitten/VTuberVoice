{
  Taken from: https://forum.lazarus.freepascal.org/index.php?topic=37843.0
  Reference: https://learn.microsoft.com/en-us/windows/win32/api/mmdeviceapi/
}

{ Modern Pascal Directives }
{$mode objfpc}

unit mmdeviceapi;

interface

uses Windows, ActiveX, ComObj;

// Start API extracted from MSDN

const
  CLASS_IMMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';

const
  DEVICE_STATE_ACTIVE     = $00000001;
  DEVICE_STATE_DISABLED   = $00000002;
//  DEVICE_STATE_NOTPRESENT = $00000004;
  DEVICE_STATE_UNPLUGGED  = $00000008;
//  DEVICE_STATEMASK_ALL    = $0000000F;

{$MINENUMSIZE 4}
type
  EDataFlow = (
    eRender,
    eCapture,
    eAll,
    EDataFlow_enum_count);

  ERole = (
    eConsole,
    eMultimedia,
    eCommunications,
    ERole_enum_count);

  TPropertyKey = record
    fmtid: TGUID;
    pid: DWORD;
  end;

  IMMNotificationClient = interface(IUnknown)
    ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']
    function OnDeviceStateChanged(pwstrDeviceId: LPWSTR;
      dwNewState: DWORD): HRESULT; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole;
      pwstrDefaultDeviceId: LPWSTR): HRESULT; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPWSTR;
      const key: TPropertyKey): HRESULT; stdcall;
  end;

  IPropertyStore = interface(IUnknown)
    function GetCount(out cProps: DWORD): HRESULT; stdcall;
    function GetAt(iProp: DWORD; out key: TPropertyKey): HRESULT; stdcall;
    function GetValue(const key: TPropertyKey;
      out value: TPropVariant): HRESULT; stdcall;
  end;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD;
      pActivationParams: PPropVariant;
      out EndpointVolume: IUnknown): HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD;
      out Properties: IPropertyStore): HRESULT; stdcall;
    function GetId(out strId: LPWSTR): HRESULT; stdcall;
    function GetState(out State: DWORD): HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out cDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out Device: IMMDevice): HRESULT; stdcall;
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow;
      dwStateMask: DWORD; out Devices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(EDF: EDataFlow; role: ERole;
      out EndPoint: IMMDevice): HRESULT; stdcall;
    function GetDevice(pwstrId: LPWSTR; out EndPoint: IMMDevice): HRESULT; stdcall;
    function RegisterEndpointNotificationCallback(
      const Client: IMMNotificationClient): HRESULT; stdcall;
    function UnregisterEndpointNotificationCallback(
      const Client: IMMNotificationClient): HRESULT; stdcall;
  end;

  // End API

  // Own wrapper

  TDeviceState = (dsUnknown, dsActive, dsDisabled, dsUnplagged);

  TDevice = class(TObject)
  private
    FId: UnicodeString;
    FName: UnicodeString;
    FState: TDeviceState;
    FDefault: Boolean;
  public
    constructor Create(const MMDevice: IMMDevice);
    property Id: UnicodeString read FId;
    property Name: UnicodeString read FName;
    property State: TDeviceState read FState;
    property Default: Boolean read FDefault;
  end;

  TDeviceList = class(TObject)
  private
    FList: array of TDevice;
    function GetCount: Integer;
    function GetItem(Index: Integer): TDevice;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TDevice read GetItem; default;
  end;

{ Returns the name of the default Audio Output Device. }
function DefaultOutputDevice : UnicodeString;

implementation

constructor TDeviceList.Create;
const
  CValidState = DEVICE_STATE_ACTIVE or
    DEVICE_STATE_DISABLED or
    DEVICE_STATE_UNPLUGGED;
var
  DE: IMMDeviceEnumerator;
  DC: IMMDeviceCollection;
  i, Cnt: UINT;
  MMDevice: IMMDevice;
  Device: TDevice;
begin
  inherited Create;
  DE := CreateComObject(CLASS_IMMDeviceEnumerator) as IMMDeviceEnumerator;
  OleCheck(DE.EnumAudioEndpoints(eRender, CValidState, DC));
  OleCheck(DC.GetCount(Cnt));
  SetLength(FList, Cnt);
  for i := 0 to Cnt - 1 do
  begin
    OleCheck(DC.Item(i, MMDevice));
    FList[i] := TDevice.Create(MMDevice);
  end;
  if DE.GetDefaultAudioEndpoint(eRender, eConsole, MMDevice) = S_OK then
  begin
    Device := TDevice.Create(MMDevice);
    try
      for i := Low(FList) to High(FList) do
        if FList[i].Id = Device.Id then
        begin
          FList[i].FDefault := True;
          Break;
        end;
    finally
      Device.Free;
    end;
  end;
end;

destructor TDeviceList.Destroy;
var
  i: Integer;
begin
  for i := Low(FList) to High(FList) do
    FList[i].Free;
  inherited;
end;

function TDeviceList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TDeviceList.GetItem(Index: Integer): TDevice;
begin
  Result := FList[Index];
end;

function PropVariantClear(var PropVar: TPropVariant): HRESULT; stdcall;
  external 'ole32.dll';

constructor TDevice.Create(const MMDevice: IMMDevice);

  procedure PropVariantInit(out PropVar: TPropVariant); inline;
  begin
    ZeroMemory(@PropVar, SizeOf(PropVar));
  end;

const
  PKEY_Device_FriendlyName: TPropertyKey =
    (fmtid:'{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid:14); // DEVPROP_TYPE_STRING
var
  Props: IPropertyStore;
  VarName: TPropVariant;
  DevState: DWORD;
  AId: PWideChar;
begin
  inherited Create;
  OleCheck(MMDevice.GetState(DevState));
  OleCheck(MMDevice.OpenPropertyStore(STGM_READ, Props));
  OleCheck(MMDevice.GetId(AId));
  FId := AId;
  CoTaskMemFree(AId);
  PropVariantInit(VarName);
  OleCheck(Props.GetValue(PKEY_Device_FriendlyName, VarName));
  FName := VarName.pwszVal;
  case DevState of
    DEVICE_STATE_ACTIVE:
      FState := dsActive;
    DEVICE_STATE_DISABLED:
      FState := dsDisabled;
    DEVICE_STATE_UNPLUGGED:
      FState := dsUnplagged;
  else
    FState := dsUnknown;
  end;
  PropVariantClear(VarName);
  Props := nil;
end;

{ Returns the name of the default Audio Output Device. }
function DefaultOutputDevice : UnicodeString;
var
  DE: IMMDeviceEnumerator;
  MMDevice: IMMDevice;
  Device: TDevice;
begin
  Result := '';
  DE := CreateComObject(CLASS_IMMDeviceEnumerator) as IMMDeviceEnumerator;
  if DE.GetDefaultAudioEndpoint(eRender, eConsole, MMDevice) = S_OK then
  begin
    Device := TDevice.Create(MMDevice);
    Result := Device.Name;
    Device.Free;
  end;

end;

end.