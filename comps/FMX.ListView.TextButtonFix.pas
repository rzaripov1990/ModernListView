unit FMX.ListView.TextButtonFix;

// by ZuBy

interface

uses
  System.Types, System.Classes, FMX.Utils, FMX.Types,
  FMX.ListView.Types, FMX.ListView.Appearances;

type
  TListViewTextButtonFix = record
    /// <summary> fix by ZuBy (lost text on the TextButton when Resizing) </summary>
    class procedure Rendering(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean); static;
  end;

implementation

{ TListViewTextButtonFix }

class procedure TListViewTextButtonFix.Rendering(const Sender: TObject; const AItem: TListViewItem;
  var AHandled: Boolean);
var
  ATextButton: TListItemTextButton;
begin
  if AItem.Objects.TextButton = nil then
    exit;

  ATextButton := AItem.Objects.FindObjectT<TListItemTextButton>('B'); // default name for TextButton (do not localize)
  if ATextButton = nil then
    ATextButton := TListItemTextButton.Create(AItem);
  ATextButton.Name := 'B';

  ATextButton.Visible := AItem.Objects.TextButton.Visible;

  if ATextButton.Visible then
  begin
    ATextButton.Enabled := AItem.Objects.TextButton.Enabled;
    ATextButton.Text := AItem.Objects.TextButton.Text;

    ATextButton.Align := AItem.Objects.TextButton.Align;
    ATextButton.VertAlign := AItem.Objects.TextButton.VertAlign;
    ATextButton.Width := AItem.Objects.TextButton.Width;
    ATextButton.Height := AItem.Objects.TextButton.Height;
    ATextButton.PlaceOffset.Assign(AItem.Objects.TextButton.PlaceOffset);

    ATextButton.ButtonType := AItem.Objects.TextButton.ButtonType;
    ATextButton.Opacity := AItem.Objects.TextButton.Opacity;

    ATextButton.Font.Assign(AItem.Objects.TextButton.Font);
    ATextButton.TextAlign := AItem.Objects.TextButton.TextAlign;
    ATextButton.TextVertAlign := AItem.Objects.TextButton.TextVertAlign;
    ATextButton.Trimming := AItem.Objects.TextButton.Trimming;
    ATextButton.WordWrap := AItem.Objects.TextButton.WordWrap;

    ATextButton.TintColor := AItem.Objects.TextButton.TintColor;
    ATextButton.TextColor := AItem.Objects.TextButton.TextColor;
    ATextButton.PressedTextColor := AItem.Objects.TextButton.PressedTextColor;
    ATextButton.TextShadowColor := AItem.Objects.TextButton.TextShadowColor;
    ATextButton.TextShadowOffset.Assign(AItem.Objects.TextButton.TextShadowOffset);
  end;

  AHandled := true;
end;

end.
