{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ListView.Types;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Classes, System.Generics.Collections, System.Generics.Defaults, System.SysUtils,
  FMX.Types, FMX.Controls,FMX.TextLayout, System.Math.Vectors, System.Rtti, FMX.Objects, FMX.Graphics, FMX.ActnList,
  FMX.Styles.Objects, FMX.ImgList;

{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
{$DEFINE LISTVIEW_TOUCH}
{$ENDIF}

{.$DEFINE PIXEL_ALIGNMENT}
{.$DEFINE DRAW_ITEM_MARGINS}

type
  TListItemAlign = (Leading, Center, Trailing);
  TListItemPurpose = (None, Header, Footer);
  TListItemPurposes = set of TListItemPurpose;

  TListItemPurposeHelper = record helper for TListItemPurpose
    function ToString: string;
  end;

  TListItem = class;
  IListViewAdapter = interface;
  IListViewController = interface;

  TListItemStyleResources = class;

  TListItemDrawState = (Selected, Deleting, EditMode);
  TListItemDrawStates = set of TListItemDrawState;

  TListItemDrawable = class;
  TListItemView = class;
  TListItemCallbackOp = (CreateDrawables, InvalidateOwner, Click);
  TListItemCallback = TProc<TListItemView, TListItemDrawable, TListItemCallbackOp>;

  /// <summary>TListItem view is comprised of TListViewDrawables. These are the actual
  /// view elements that are being painted in the item cells.</summary>
  TListItemDrawable = class(TInterfacedPersistent)
  public type
    TParams = record
      AbsoluteOpacity: Single;
      ItemSelectedAlpha: Single;
      DeletingUnwantedOpacity: Single;
      EditModeTransitionAlpha: Single;
      ParentAbsoluteRect: TRectF;
      Images: TCustomImageList;
    end;
  strict private
    FPlaceOffsetX: TPosition;
  protected type
    TStyleResource = (FontSize, FontFamily, FontStyle, TextColor, SelectedTextColor,
      TextShadowColor, PressedTextColor);
    TStyleResources = set of TStyleResource;
  protected const
    TextResources: set of TStyleResource = [TStyleResource.FontFamily,
      TStyleResource.FontSize, TStyleResource.FontStyle, TStyleResource.TextColor,
      TStyleResource.TextShadowColor, TStyleResource.SelectedTextColor, TStyleResource.PressedTextColor];
  private
    FAlign: TListItemAlign;
    FVertAlign: TListItemAlign;
    FVisible: Boolean;
    FWidth: Single;
    FHeight: Single;
    FOpacity: Single;
    FUpdating: Integer;
    NeedRepaint: Boolean;
    FOnSelect: TNotifyEvent;
    FName: string;
    [Weak] FTagObject: TObject;
    FTagFloat: Single;
    FTagString: string;
    FLocalRect: TRectF;
    FStyleValuesNeedUpdate: TStyleResources;
    FController: IListViewController;
    FCallback: TListItemCallback;

    procedure SetOneDimension(const Index: Integer; const Value: Single);
    procedure SetSize(const Value: TPointF);
    function GetSize: TPointF;
    procedure SetOpacity(const Value: Single);
    procedure SetAlign(const Value: TListItemAlign);
    procedure SetVertAlign(const Value: TListItemAlign);
    procedure SetVisible(const Value: Boolean);
    function GetData: TValue; virtual;
    procedure SetData(const Value: TValue); virtual;
    function GetPlaceOffset: TPosition; inline;
    procedure SetInvalidateCallback(const Callback: TListItemCallback); virtual;
    procedure PlaceOffsetChanged(Sender: TObject);

  protected
    /// <summary>Called when the <see cref='Size'>Size</see> of this drawable changes</summary>
    procedure DoResize; virtual;
    /// <summary>Called when the <see cref='Opacity'>Opacity</see> of this drawable changes</summary>
    procedure DoOpacityChange; virtual;
    /// <summary>Called when <see cref='FMX.ListView.Types.TListItem'>TListItem</see> comprising
    /// this drawable is selected.</summary>
    procedure DoSelect; virtual;
    /// <summary>Called when <see cref='PlaceOffset'>PlaceOffset</see> changes</summary>
    procedure DoPlaceOffsetChanged; virtual;
    /// <summary>Called when <see cref='Align'>Align</see> or <see cref='VertAlign'>VertAlign</see> changes</summary>
    procedure DoAlignChanged; virtual;
    /// <summary>Finds an embedded <see cref='FMX.Controls.TControl'>TControl</see> at given <c>Point</c></summary>
    function ObjectAtPoint(const Point: TPointF): TControl; virtual;

    /// <summary>Handle MouseDown event. Called by
    // <see cref='FMX.ListView.Types.TListItem.MouseDown'>TListItem.MouseDown</see> </summary>
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean; virtual;
    /// <summary>Handle MouseMove event. Called by host
    /// <see cref='FMX.ListView.Types.TListItem.MouseMove'>TListItem.MouseMove</see></summary>
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF); virtual;
    /// <summary>Handle MouseUp event. Called by
    // <see cref='FMX.ListView.Types.TListItem.MouseUp'>TListItem.MouseUp</see> </summary>
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF); virtual;
    /// <summary>Called by <see cref='UpdateValuesFromResources'>UpdateValuesFromResources</see>.
    /// Updates default parameters defined by style in descendants such as font, font syle and colors.</summary>
    procedure DoUpdateValuesFromResources(const Resources: TListItemStyleResources; const Purpose: TListItemPurpose); virtual;
  public
    constructor Create(const AOwner: TListItem); virtual;
    destructor Destroy; override;
    function ToString: string; override;

    /// <summary>Return amount of rendering passes. See TListViewBase.DrawListItems</summary>
    function GetRenderPassCount: Integer; virtual;
    /// <summary>Align and calculate this drawable's local rectangle for given item's DestRect and DrawStates</summary>
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); virtual;
    /// <summary>Return True if Point is inside local rect</summary>
    function InLocalRect(const Point: TPointF): Boolean;
    /// <summary>Request repaint. When between <c>BeginUpdate</c>/<c>EndUpdate</c> the request will be held back until the
    /// update cycle is finished</summary>
    procedure Invalidate;
    /// <summary>Render this drawable
    ///  <param><c>Canvas</c> destination canvas</param>
    ///  <param><c>DrawItemIndex</c> index within parent <c>TListView</c> of item being rendered</param>
    ///  <param><c>DrawStates</c> which of the item states to render: <c>Selected</c>, <c>Deleting</c>, <c>EditMode</c>;
    ///   see <see cref='TListItemDrawStates'>TListItemDrawStates</see></param>
    ///  <param><c>Resources</c> default style resources to use for rendering;
    ///   <see cref='TListItemStyleResources'>TListItemStyleResources</see></param>
    ///  <param><c>Params</c> extra rendering parameters; see
    ///   <see cref='TListItemDrawable.TParams'>TListItemDrawable.TParams</see></param>
    /// </summary>
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TParams; const SubPassNo: Integer = 0); virtual; abstract;
    /// <summary>Begin update. During update calling <see cref='InvalidateCallback'>InvalidateCallback</see>
    /// will be held back until <see cref='EndUpdate'>EndUpdate</see></summary>
    procedure BeginUpdate;
    /// <summary>End update. If invalidation was required during the update cycle,
    /// <see cref='InvalidateCallback'>InvalidateCallback</see> will be invoked</summary>
    procedure EndUpdate;
    /// <summary>Initializes or updates default parameters defined by style in descendants such as font, font syle
    // and colors.</summary>
    procedure UpdateValuesFromResources(const Resources: TListItemStyleResources; const Purpose: TListItemPurpose);
    /// <summary>Set flag that enables UpdateValuesFromResources</summary>
    procedure UpdateValuesFromStyle;
    /// <summary>Local width of list item inside its designated area</summary>
    property Width: Single index 0 read FWidth write SetOneDimension;
    /// <summary>Local height of list item inside its designated area</summary>
    property Height: Single index 1 read FHeight write SetOneDimension;
    /// <summary>Size of this drawable</summary>
    property Size: TPointF read GetSize write SetSize;
    /// <summary>Horizontal alignment of drawable inside its designated area</summary>
    property Align: TListItemAlign read FAlign write SetAlign;
    /// <summary>Vertical alignment of drawable inside its designated area</summary>
    property VertAlign: TListItemAlign read FVertAlign write SetVertAlign;
    /// <summary>Determines whether the current drawable is visible or not</summary>
    property Visible: Boolean read FVisible write SetVisible;
    /// <summary>The offset in logical units regarding aligned location for finer placement control</summary>
    property PlaceOffset: TPosition read GetPlaceOffset;
    /// <summary>Name of this drawable</summary>
    property Name: string read FName write FName;
    /// <summary>Drawing opacity</summary>
    property Opacity: Single read FOpacity write SetOpacity;
    /// <summary>LocalRect of this drawable</summary>
    property LocalRect: TRectF read FLocalRect;
    /// <summary>Invoked when owner TListItem is selected</summary>
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    // <summary>Polymorphic property access</summary>
    property Data: TValue read GetData write SetData;
    /// <summary>User-defined object reference for this drawable</summary>
    property TagObject: TObject read FTagObject write FTagObject;
    /// <summary>User-defined floating-point number for this drawable</summary>
    property TagFloat: Single read FTagFloat write FTagFloat;
    /// <summary>User-defined string for this drawable</summary>
    property TagString: string read FTagString write FTagString;
    /// <summary>Callback invoked when item is being invalidated
    /// <see cref='FMX.ListView.Types.TListItemCallback'>TListItemCallback</see>
    /// </summary>
    property InvalidateCallback: TListItemCallback write SetInvalidateCallback;
  end;

  /// <summary>Declared for compatibility</summary>
  TListItemObject = class(TListItemDrawable)
  end;

  /// <summary>Represents a text drawable in ListView items</summary>
  TListItemText = class(TListItemDrawable)
  private const
    DefaultFontFamily = 'Helvetica'; // do not localize
    DefaultFontSize = 14;
    ShadowOffset: TPointF = (X: 0; Y: 1);
  private type
    TFontSettings = record
      Family: string;
      Size: Single;
      Style: TFontStyles;
    end;
  strict private
    FFontX: TFont;
    FFontSettings: TFontSettings;
  private
    FTextLayout: TTextLayout;
    FText: string;
    FTextAlign: TTextAlign;
    FTextVertAlign: TTextAlign;
    FWordWrap: Boolean;
    LayoutChanged: Boolean;
    FTextColor: TAlphaColor;
    FSelectedTextColor: TAlphaColor;
    FTrimming: TTextTrimming;
    FTextShadowOffsetX: TPosition;
    FTextShadowColor: TAlphaColor;
    FIsDetailText: Boolean;

    procedure FontChanged(Sender: TObject);
    procedure TextShadowOffsetChanged(Sender: TObject);
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetTextVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetSelectedTextColor(const Value: TAlphaColor);
    procedure SetTextShadowColor(const Value: TAlphaColor);
    procedure SetIsDetailText(const Value: Boolean);
    procedure SetData(const AValue: TValue); override;
    function GetData: TValue; override;
    function GetShadowOffset: TPosition; inline;
    function GetFont: TFont;
    function FontSettingsSnapshot: TFontSettings;
  protected
    procedure DoResize; override;
    procedure DoUpdateValuesFromResources(const Resources: TListItemStyleResources; const Purpose: TListItemPurpose); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0); override;
    /// <summary>Font used to drawing text in this drawable</summary>
    property Font: TFont read GetFont;
    /// <summary>Text displayed in this drawable</summary>
    property Text: string read FText write SetText;
    /// <summary>Horizontal text alignment inside local item rectangle</summary>
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    /// <summary>Vertical text alignment inside local item rectangle</summary>
    property TextVertAlign: TTextAlign read FTextVertAlign write SetTextVertAlign;
    /// <summary>Wrap the text it does not fit in the available width</summary>
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    /// <summary>Text color in neutral state</summary>
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    /// <summary>Text color in selected state</summary>
    property SelectedTextColor: TAlphaColor read FSelectedTextColor write SetSelectedTextColor;
    /// <summary>Text shadow color. The text shadow will appear behind normal text only when its color is
    /// set to non-zero value (default)</summary>
    property TextShadowColor: TAlphaColor read FTextShadowColor write SetTextShadowColor;
    /// <summary>Text shadow offset. The text shadow will appear behind normal text only when its color is
    /// set to non-zero value (default)</summary>
    property TextShadowOffset: TPosition read GetShadowOffset;
    /// <summary>Text trimming
    /// <see cref='FMX.Types.TTextTrimming'>TTextTrimming</see>
    /// </summary>
    property Trimming: TTextTrimming read FTrimming write SetTrimming;
    /// <summary>Hints regarding the contents of this text object, which affecs visual style</summary>
    property IsDetailText: Boolean read FIsDetailText write SetIsDetailText;
  end;

  /// <summary>An empty drawable</summary>
  TListItemDummy = class(TListItemDrawable)
  public
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;
  end;

  /// <summary>Image scaling modes for TListItemImage
  /// <see cref='FMX.ListView.Types.TListItemImage.ScalingMode'>TListItemImage.ScalingMode</see>
  /// <param><c>StretchWithAspect</c></param> stretch while preserving aspect ratio
  /// <param><c>Original</c></param> keep original size and proportion
  /// <param><c>Stretch</c></param> stertch to fill available area disregarding proportions
  /// </summary>
  TImageScalingMode = (StretchWithAspect, Original, Stretch);

  /// <summary>Compatibility helper for TImageScalingMode</summary>
  TImageScalingModeHelper = record helper for TImageScalingMode
  const
    smStretchWithAspect = TImageScalingMode.StretchWithAspect deprecated 'Use TImageScalingMode.StretchWithAspect';
    smOriginal = TImageScalingMode.Original deprecated 'Use TImageScalingMode.Original';
    smStretch = TImageScalingMode.Stretch deprecated 'Use TImageScalingMode.Stretch';
  end;

  /// <summary>A TListItemDrawable representing an image</summary>
  TListItemImage = class(TListItemDrawable)
  public type
    /// <summary>Image source:
    /// <para><b>None</b> no image source</para>
    /// <para><b>Bitmap</b> bitmap or bitmap reference</para>
    /// <para><b>ImageList</b> bitmap is specified by an index in an ImageList</para>
    /// </summary>
    TImageSource = (None, Bitmap, ImageList);
  private
    FStaticBitmap: TBitmap;
    [Weak]FReferBitmap: TBitmap;
    FSrcRect: TRectF;
    FOwnsBitmap: Boolean;
    FImageScalingMode: TImageScalingMode;
    FImageIndex: TImageIndex;
    FImageSource: TImageSource;
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetOwnsBitmap(const Value: Boolean);
    procedure SetSrcRect(const Value: TRectF);
    procedure SetImageScalingMode(const Value: TImageScalingMode);
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageSource: TImageSource; inline;
    procedure SetData(const Value: TValue); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;
    /// <summary>Bitmap, used if <b>ImageSource</b> equals <b>TImageSource.Bitmap</b></summary>
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    /// <summary>Determines whether this list owns and maintains the bitmap, or whether it is a reference only.
    /// It is must faster and memory efficient to have multiple references to a single bitmap rather than multiple
    /// copies of the same bitmap copied across the items</summary>
    property OwnsBitmap: Boolean read FOwnsBitmap write SetOwnsBitmap;
    /// <summary>Fit InputRect part of Bitmap into DestinationRect according to current ScalingMode
    /// <para><b>Bitmap</b> TBitmap being fit</para>
    /// <para><b>InputRect</b> source rectangle</para>
    /// <para><b>DestinationRect</b> destination rectangle where the bitmap is to be placed</para>
    /// </summary>
    procedure FitInto(const Bitmap: TBitmap; var InputRect, DestinationRect: TRectF);
    /// <summary>Source rectangle in ImageSource</summary>
    property SrcRect: TRectF read FSrcRect write SetSrcRect;
    /// <summary>Scaling mode used for fitting the bitmap into destination rectangle</summary>
    property ScalingMode: TImageScalingMode read FImageScalingMode write SetImageScalingMode
      default TImageScalingMode.StretchWithAspect;
    /// <summary>Indicates whether the images are obtained from TImageList or directly by using Bitmap property.</summary>
    property ImageSource: TImageSource read GetImageSource;
    ///<summary> Zero based index of an image. The default is <c>-1</c>.
    ///<para> See also <b>FMX.ActnList.IGlyph</b></para></summary>
    ///<remarks> If non-existing index is specified, an image is not drawn and no exception is raised</remarks>
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  TListItemEmbeddedControl = class;
  /// <summary>IScene implementation for TListItemEmbeddedControl</summary>
  TListItemControlScene = class(TFmxObject, IStyleBookOwner, IScene)
  strict private
    FCanvas: TCanvas;
    [Weak] FContainer: TControl;
    [Weak] FOwnerItem: TListItem;
    FDrawing: Boolean;
    FDisableUpdating: Integer;
  private
    FLayoutSize: TPoint;
    function GetRealScene: IScene;
  protected
    // TFmxObject
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    // IStyleBookOwner
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    // IScene
    function GetCanvas: TCanvas;
    function GetSceneScale: Single;
    function GetObject: TFmxObject;
    procedure AddUpdateRect(R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function LocalToScreen(P: TPointF): TPointF;
    function ScreenToLocal(P: TPointF): TPointF;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
    procedure DisableUpdating;
    procedure EnableUpdating;
    /// <summary>Set item that contains this scene</summary>
    procedure SetOwnerItem(const Item: TListItem);
    /// <summary>Set container control</summary>
    procedure SetContainer(const Container: TControl);
    /// <summary>Get IScene of the parent control, i.e. TListView</summary>
    property RealScene: IScene read GetRealScene;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>Repaint scene on canvas Canvas</summary>
    procedure RepaintScene(const Canvas: TCanvas);
    /// <summary>Container control</summary>
    property Container: TControl read FContainer;
  end;

  /// <summary>A dummy TControl that contains a TListItemEmbeddedControl</summary>
  TListItemControlContainer = class(TControl)
  private
    [weak]FItemOwner: TListItemEmbeddedControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  /// <summary>Base class for TListItem embedded controls</summary>
  TListItemEmbeddedControl = class(TListItemDrawable)
  private
    FScene: TListItemControlScene;
    FContainer: TListItemControlContainer;
  protected
    /// <summary>Return TControl located at given Point</summary>
    function ObjectAtPoint(const Point: TPointF): TControl; override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;
    /// <summary>TListItemControlContainer of this embedded control</summary>
    property Container: TListItemControlContainer read FContainer;
  end;

  /// <summary>Simple embedded control base class</summary>
  TListItemSimpleControl = class(TListItemDrawable)
  private const
    DisabledOpacity = 0.6;
  private
    FEnabled: Boolean;
    FPressed: Boolean;
    FMouseOver: Boolean;
    FTouchExpand: Single;
    procedure SetEnabled(const Value: Boolean);
  protected
    /// <summary>Shall intercept clicks</summary>
    function IsClickOpaque: Boolean; virtual;
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF); override;
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF); override;
    /// <summary>Handle click</summary>
    procedure DoClick; virtual;
    /// <summary>Handle change of Enabled</summary>
    procedure DoEnabledChange; virtual;
  public
    constructor Create(const AOwner: TListItem); override;
    /// <summary>Test if point Pos belongs to local rectangle of this control</summary>
    function PointInLocalRect(const Pos: TPointF): Boolean;
    /// <summary>This method is called when click event is sent to the control to handle any actions associated
    /// with this item</summary>
    procedure Click;
    /// <summary>Control state: Enabled</summary>
    property Enabled: Boolean read FEnabled write SetEnabled;
    /// <summary>Control state: Pressed</summary>
    property Pressed: Boolean read FPressed;
    /// <summary>Control state: MouseOver</summary>
    property MouseOver: Boolean read FMouseOver;
    /// <summary>Additional area (in logical units) around the control that is sensitive to touch</summary>
    property TouchExpand: Single read FTouchExpand write FTouchExpand;
  end;

  /// <summary>Accessory type for <b>TListItemAccessory</b>
  ///  <para><c>More</c> more</para>
  ///  <para><c>Checkmark</c> checkmark</para>
  ///  <para><c>Detail</c> detail disclosure</para>
  /// </summary>
  TAccessoryType = (More, Checkmark, Detail);

  /// <summary>List item accessory, a glyph typically displayed at the right edge of the list item</summary>
  TListItemAccessory = class(TListItemDrawable)
  private
    FAccessoryType: TAccessoryType;
  protected
    /// <summary>Set accessory type: <c>More</c>, <c>Checkmark</c> or <c>Detail</c></summary>
    procedure SetAccessoryType(Value: TAccessoryType);
  public
    constructor Create(const AOwner: TListItem); override;

    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;

    /// <summary>Accessory type, <see cref='FMX.ListView.Types.TAccessoryType'>TAccessoryType</see></summary>
    property AccessoryType: TAccessoryType read FAccessoryType write SetAccessoryType;
  end;

  /// <summary>Glyph type for <b>TListItemGlyphButton</b>
  ///  <para><b>Add</b> plus button</para>
  ///  <para><b>Delete</b> delete button</para>
  ///  <para><b>Checkbox</b> selection checkbox</para>
  /// </summary>
  TGlyphButtonType = (Add, Delete, Checkbox);

  /// <summary>Glyph button is an additional control usually used in Edit mode. It can be an Add/Plus sign,
  /// a Delete button or a Checkbox</summary>
  TListItemGlyphButton = class(TListItemSimpleControl)
  private const
    CheckedAnimationFrameRate = 60;
    CheckedAnimationDuration = 0.15; // in seconds
  private
    FButtonType: TGlyphButtonType;
    FClickOnSelect: Boolean;
    FChecked: Boolean;
    FTransitionEnabled: Boolean;
    FTransitionAlpha: Single;
    FTransitionTimer: TTimer;
    FTransitionStartTicks: Double;
    FTimerService: IFMXTimerService;
    procedure SetButtonType(const Value: TGlyphButtonType);
    procedure SetChecked(const Value: Boolean);
    procedure InitCheckedTransition;
    procedure ResetCheckedTransition;
    procedure TransitionTimerNotify(Sender: TObject);
  protected
    procedure DoSelect; override;
    procedure DoClick; override;
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure SetData(const AValue: TValue); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;
    /// <summary>Button type, <see cref='FMX.ListView.Types.TGlyphButtonType'>TGlyphButtonType</see></summary>
    property ButtonType: TGlyphButtonType read FButtonType write SetButtonType;
    /// <summary>If set to True, this button will receive click events from entire list by using of selection</summary>
    property ClickOnSelect: Boolean read FClickOnSelect write FClickOnSelect;
    /// <summary>Determines whether checkbox is checked, has no effect for other button types</summary>
    property Checked: Boolean read FChecked write SetChecked;
  end;

  /// <summary>Type of TListItemTextButton
  ///  <para><b>Normal</b> a regular button</para>
  ///  <para><b>Delete</b> a delete button</para>
  /// </summary>
  TTextButtonType = (Normal, Delete);

  /// <summary>A button with text that can be clicked inside of a TListItem</summary>
  TListItemTextButton = class(TListItemSimpleControl)
  private
    FTextDrawable: TListItemText;
    FButtonType: TTextButtonType;
    FTintColor: TAlphaColor;
    FPressedTextColor: TAlphaColor;
    FTextColor: TAlphaColor;

    // getters routing to FTextDrawable
    function GetFont: TFont;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    function GetTextAlign: TTextAlign;
    procedure SetTextVertAlign(const Value: TTextAlign);
    function GetTextVertAlign: TTextAlign;
    procedure SetWordWrap(const Value: Boolean);
    function GetWordWrap: Boolean;
    procedure SetTextColor(const Value: TAlphaColor);
    function GetTextColor: TAlphaColor;
    procedure SetTrimming(const Value: TTextTrimming);
    function GetTrimming: TTextTrimming;
    procedure SetTextShadowColor(const Value: TAlphaColor);
    function GetTextShadowColor: TAlphaColor;

    // button-specific text properties
    procedure SetPressedTextColor(const Value: TAlphaColor);
    function GetTextShadowOffset: TPosition; inline;
    procedure SetButtonType(const Value: TTextButtonType);
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetInvalidateCallback(const Callback: TListItemCallback); override;
  protected
    function IsClickOpaque: Boolean; override;
    procedure DoResize; override;
    procedure DoOpacityChange; override;
    procedure DoPlaceOffsetChanged; override;
    procedure DoAlignChanged; override;
    procedure SetData(const AValue: TValue); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;

    procedure DoUpdateValuesFromResources(const Resources: TListItemStyleResources;
      const Purpose: TListItemPurpose); override;
    function GetRenderPassCount: Integer; override;
    procedure CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
      const DrawStates: TListItemDrawStates; const Item: TListItem); override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0); override;

    /// <summary>Button type, <see cref='FMX.ListView.Types.TTextButtonType'>TTextButtonType</see></summary>
    property ButtonType: TTextButtonType read FButtonType write SetButtonType;
    /// <summary>Button tint color</summary>
    property TintColor: TAlphaColor read FTintColor write SetTintColor;
    /// <summary>Font used to draw button text</summary>
    property Font: TFont read GetFont;
    /// <summary>Button text</summary>
    property Text: string read GetText write SetText;
    /// <summary>Horizontal alignment of text within the button area</summary>
    property TextAlign: TTextAlign read GetTextAlign write SetTextAlign;
    /// <summary>Vertical alignment of text within the button area</summary>
    property TextVertAlign: TTextAlign read GetTextVertAlign write SetTextVertAlign;
    /// <summary>If true, the text that does not fit in button width will be wrapped</summary>
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    /// <summary>Text color in neutral state</summary>
    property TextColor: TAlphaColor read GetTextColor write SetTextColor;
    /// <summary>Text color in pressed state</summary>
    property PressedTextColor: TAlphaColor read FPressedTextColor write SetPressedTextColor;
    /// <summary>Text shadow color</summary>
    property TextShadowColor: TAlphaColor read GetTextShadowColor write SetTextShadowColor;
    /// <summary>Text shadow offset</summary>
    property TextShadowOffset: TPosition read GetTextShadowOffset;
    /// <summary>Text trimming
    /// <see cref='FMX.Types.TTextTrimming'>TTextTrimming</see>
    /// </summary>
    property Trimming: TTextTrimming read GetTrimming write SetTrimming;
  end;

  /// <summary>TListItemView is a collection of drawables that comprise the view of a TListItem</summary>
  TListItemView = class
  private type
    TViewList = TObjectList<TListItemDrawable>;
  private
    FCallback: TListItemCallback;
    FViewList: TListItemView.TViewList;
    FInitialized: Boolean;

    function GetCount: Integer;
    function GetObject(const Index: Integer): TListItemDrawable;
    function GetViewList: TViewList; inline;
  protected
    procedure Include(const AItem: TListItemDrawable);
    procedure Exclude(const AItem: TListItemDrawable);
    function GetInitialized: Boolean;
    procedure SetInitialized(const Value: Boolean);
  public
    constructor Create(const AOwner: TListItem);
    destructor Destroy; override;
    /// <summary>Add a drawable to view</summary>
    function Add(const AItem: TListItemDrawable): Integer;
    /// <summary>Insert a drawable at a position indicated by Index</summary>
    procedure Insert(const Index: Integer; const Value: TListItemDrawable);
    /// <summary>Clear the view</summary>
    procedure Clear; virtual;
    /// <summary>Delete drawable at index Index</summary>
    procedure Delete(Index: Integer);
    /// <summary>Find <see cref='FMX.ListView.Types.TListItemDrawable'>TListItemDrawable</see>
    /// specified by name AName</summary>
    function FindDrawable(const AName: string): TListItemDrawable;
    /// <summary>Find TListItemDrawable specified by name AName</summary>
    function FindObject(const AName: string): TListItemDrawable; deprecated 'Use FindDrawable';

                                                                                                                          
    /// <summary>Find TListItemDrawable specified by name AName and throw an error if not found</summary>
    function DrawableByName(const AName: string): TListItemDrawable;
    /// <summary>Find TListItemDrawable specified by name AName and throw an error if not found</summary>
    function ObjectByName(const AName: string): TListItemDrawable; deprecated 'Use DrawableByName';
    /// <summary>Number of drawables in this view</summary>
    property Count: Integer read GetCount;
    /// <summary>TListItemDrawable by index</summary>
    property Drawables[const Index: Integer]: TListItemDrawable read GetObject; default;
    /// <summary>Get TViewList that contains the drawables</summary>
    property ViewList: TViewList read GetViewList;
    /// <summary>Used internally to notify owner about changes</summary>
    property Callback: TListItemCallback write FCallback;
    /// <summary>Used internally</summary>
    property Initialized: Boolean read GetInitialized write SetInitialized;
  end;

  /// <summary>Compatibility class for TListItemView</summary>
  TListItemObjects = class(TListItemView)
  end;

  /// <summary>TListItem is an element that comprises TListView. Each individual item contains a View,
  /// which in turn is comprised of instances of TListItemDrawable</summary>
  TListItem = class
  public type
    TListItemViewType = class of TListItemView;
    TListItemNotifyEvent = procedure(const Item: TListItem) of object;
  strict private
    FHeaderRef: Integer;
    FAdapter: IListViewAdapter;
    FController: IListViewController;
  private
    FIndex: Integer;
    FHeight: Integer;
    FPurpose: TListItemPurpose;
    FUpdating: Integer;
    FNeedRepaint: Boolean;
    FView: TListItemView;
    [Weak] FTagObject: TObject;
    FTag: NativeInt;
    FTagString: string;
    function GetCount: Integer;
    procedure SetHeight(const Value: Integer);
    function GetController: IListViewController;
  protected
    /// <summary>Invoked when item height changes and heights of all items need to be recounted</summary>
    procedure InvalidateHeights; virtual;
    /// <summary>Setter for Purpose property</summary>
    procedure SetPurpose(const AValue: TListItemPurpose); virtual;
    /// <summary>Require repaint, notify the controller that the item is invalid</summary>
    procedure Repaint; virtual;
    /// <summary>Return class of TListItemView</summary>
    function ListItemObjectsClass: TListItemViewType; virtual;
    /// <summary>Getter for Index property</summary>
    function GetIndex: Integer; virtual;
    /// <summary>Setter for Index property</summary>
    procedure SetIndex(const Value: Integer); virtual;
  public
    constructor Create(const AAdapter: IListViewAdapter; const AController: IListViewController = nil);
    destructor Destroy; override;
    function ToString: string; override;
    /// <summary>Invalidate item, request repainting</summary>
    procedure Invalidate;
    /// <summary>Begin update. Invalidation request will not cause immediate action during update.</summary>
    procedure BeginUpdate;
    /// <summary>End update</summary>
    procedure EndUpdate;
    /// <summary>Called when the view drawables need to be created</summary>
    procedure CreateObjects; virtual;
    /// <summary>Called when the item is about to be painted</summary>
    procedure WillBePainted; virtual;
    /// <summary>Locate a control at a point, used with embedded controls</summary>
    function ObjectAtPoint(const Point: TPointF): TControl;
    /// <summary>Handle MouseDown event</summary>
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
    /// <summary>Handle MouseMove event</summary>
    procedure MouseMove(const Shift: TShiftState; const MousePos: TPointF);
    /// <summary>Handle MouseUp event</summary>
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
    /// <summary>Handle selection</summary>
    procedure MouseSelect;
    /// <summary>True if there are items inside that react as clicked when the item is selected</summary>
    function HasClickOnSelectItems: Boolean;
    /// <summary>Adapter that observes this item</summary>
    property Adapter: IListViewAdapter read FAdapter;
    /// <summary>Number of drawables in the View, shorthand for View.Count</summary>
    property Count: Integer read GetCount;
    /// <summary>The view, drawables that comprise the visual of this item</summary>
    property View: TListItemView read FView;
    /// <summary>Height of this item</summary>
    property Height: Integer read FHeight write SetHeight;
    /// <summary>Reference to IListViewController</summary>
    property Controller: IListViewController read GetController;
    /// <summary>Which purpose item serves for: normal item (none), header or footer.
    /// <see cref='FMX.ListView.Types.TListItemPurpose'>TListItemPurpose</see></summary>
    property Purpose: TListItemPurpose read FPurpose write SetPurpose;
    /// <summary>Reference to header of the group this item belongs to, for grouped views</summary>
    property HeaderRef: Integer read FHeaderRef write FHeaderRef;
    /// <summary>Index of this item in TListView.Adapter</summary>
    property Index: Integer read GetIndex write SetIndex;
    /// <summary>User-defined integer tag for this item</summary>
    property Tag: NativeInt read FTag write FTag;
    /// <summary>User-defined object reference for this item</summary>
    property TagObject: TObject read FTagObject write FTagObject;
    /// <summary>User-defined string tag for this item</summary>
    property TagString: string read FTagString write FTagString;
  end;

  /// <summary>A container used for passing various style-defined properties used in <c>TListView</c></summary>
  TListItemStyleResources = class
  private
    FOwnsObjects: Boolean;
  public type
    /// <summary>References to <c>TStyleObject</c>s used for rendering of <c>TListItemAccessory</c></summary>
    TAccessoryStyleObject = record
      /// <summary>Accessory <c>TStyleObject</c> in neutral state</summary>
      [Weak] Normal: TStyleObject;
      /// <summary>Accessory <c>TStyleObject</c> in selected state</summary>
      [Weak] Selected: TStyleObject;
    end;

    /// <summary>Style objects used for button rendering</summary>
    TButtonStyleObject = record
      /// <summary>Button <c>TStyleObject</c> in neutral state</summary>
      [Weak] Normal: TStyleObject;
      /// <summary>Button <c>TStyleObject</c> in pressed state</summary>
      [Weak] Pressed: TStyleObject;
    end;
  public
    /// <summary>Style references used for Accessory rendering</summary>
    AccessoryImages: array [TAccessoryType] of TAccessoryStyleObject;
    /// <summary>Font used in header items</summary>
    HeaderTextFont: TFont;
    /// <summary>Color of header text</summary>
    HeaderTextColor: TAlphaColor;
    /// <summary>Color of header text shadow</summary>
    HeaderTextShadowColor: TAlphaColor;
    /// <summary>Default font for text drawables</summary>
    DefaultTextFont: TFont;
    /// <summary>Default color for text drawables</summary>
    DefaultTextColor: TAlphaColor;
    /// <summary>Default font for detail text</summary>
    DetailTextFont: TFont;
    /// <summary>Default color for detail text</summary>
    DetailTextColor: TAlphaColor;
    /// <summary>Default color for text in selected state</summary>
    DefaultTextSelectedColor: TAlphaColor;
    /// <summary>Style objects for Add Item <c>TListItemGlyphButton</c></summary>
    ButtonAddItemStyleImage: TButtonStyleObject;
    /// <summary>Style objects for Delete Item <c>TListItemGlyphButton</c></summary>
    ButtonDeleteItemStyleImage: TButtonStyleObject;
    /// <summary>Background image for regular <c>TListItemTextButton</c></summary>
    ButtonNormalStyleImage: TButtonStyleObject;
    /// <summary>Background image for delete-themed <c>TListItemTextButton</c></summary>
    ButtonDeleteStyleImage: TButtonStyleObject;
    /// <summary>Style objects for Checkbox <c>TListItemGlyphButton</c></summary>
    ButtonCheckboxStyleImage: TButtonStyleObject;
    /// <summary>Font for <c>TListItemTextButton</c></summary>
    ButtonTextFont: TFont;
    /// <summary>Text color for neutral state <c>TListItemTextButton</c></summary>
    ButtonTextColor: TAlphaColor;
    /// <summary>Text color for pressed state <c>TListItemTextButton</c></summary>
    ButtonTextPressedColor: TAlphaColor;
    /// <summary>Font for delete-themed <c>TListItemTextButton</c></summary>
    DeleteButtonTextFont: TFont;
    /// <summary>Text color for neutral state delete-themed <c>TListItemTextButton</c></summary>
    DeleteButtonTextColor: TAlphaColor;
    /// <summary>Text color for pressed state delete-themed <c>TListItemTextButton</c></summary>
    DeleteButtonTextPressedColor: TAlphaColor;
    /// <summary>Glow color that appears at the top or the bottom of screen when scrolling
    /// is being stretched out</summary>
    ScrollingStretchGlowColor: TAlphaColor;
    /// <summary>Indicator color for drawing Pull Refresh indicator</summary>
    PullRefreshIndicatorColor: TAlphaColor;
    /// <summary>Stroke color for drawing Pull Refresh indicator</summary>
    PullRefreshStrokeColor: TAlphaColor;
    constructor Create; overload;
    constructor Create(const Source: TListItemStyleResources); overload;
    destructor Destroy; override;
  end;

  /// <summary>Interface providing access to style resources. Implemented by <c>TListViewBase</c></summary>
  IListItemStyleResources = interface
    ['{0328C6F1-432C-4F8B-994B-7AB2543CD172}']
    /// <summary>Getter for StyleResources property</summary>
    function GetStyleResources: TListItemStyleResources;
    /// <summary><c>TListView</c> style resources</summary>
    property StyleResources: TListItemStyleResources read GetStyleResources;
    /// <summary><c>True</c> if style resources should be reloaded</summary>
    function StyleResourcesNeedUpdate: Boolean;
  end;

  /// <summary>Interface providing loose coupling between <c>TListView</c> and <c>TListItem</c>. Implemented by
  /// <c>TListViewBase</c></summary>
  IListViewController = interface
    ['{3855EF72-3B32-41BE-9068-7B109B2DD8E5}']
    function GetEditModeTransitionAlpha: Single;
    function GetDeleteModeTransitionAlpha: Single;
    function GetImages: TCustomImageList;
    /// <summary><c>True</c> if delete mode is enabled</summary>
    function IsDeleteModeAllowed: Boolean;
    /// <summary>Left offset of item rectangle in edit mode</summary>
    function GetItemEditOffset(const Item: TListItem): Single;
    /// <summary>Right cutoff of item rectangle in delete mode</summary>
    function GetItemDeleteCutoff(const Item: TListItem): Single;
    /// <summary>Item selection alpha</summary>
    function GetItemSelectionAlpha(const Item: TListItem): Single;
    /// <summary>Client area margins</summary>
    function GetClientMargins: TRectF;
    /// <summary>IScene of the <c>TListView</c> control</summary>
    function GetScene: IScene;
    /// <summary>Request <c>TListView</c> to initialize item indices;
    /// see <see cref='TListViewItem.GetIndex'>TListViewItem.GetIndex</see>
    /// </summary>
    procedure RequestReindexing(const Item: TListItem);
    /// <summary>Notify <c>TListView</c> that item size has changed;
    /// see <see cref='TListViewItem.InvalidateHeights'>TListViewItem.InvalidateHeights</see>
    /// </summary>
    procedure ItemResized(const Item: TListItem);
    /// <summary>Notify <c>TListView</c> that item requires repainting</summary>
    procedure ItemInvalidated(const Item: TListItem);
    /// <summary>Notify <c>TListView</c> that a control inside of an item has been clicked</summary>
    procedure ControlClicked(const Item: TListItem; const Control: TListItemDrawable);
    /// <summary>Notify <c>TListView</c> that a control inside of an item has been clicked</summary>
    procedure CheckStateChanged(const Item: TListItem; const Control: TListItemDrawable);
    /// <summary>Item alpha during edit mode transition</summary>
    property EditModeTransitionAlpha: Single read GetEditModeTransitionAlpha;
    /// <summary>Item alpha during delete transition</summary>
    property DeleteModeTransitionAlpha: Single read GetDeleteModeTransitionAlpha;
    ///<summary>TImageList used when ImageSource = ImageList; can be <c>nil</c></summary>
    property Images: TCustomImageList read GetImages;
  end;

  /// <summary>Presentation of <c>TPresentedListView</c>. The control passes messages to the
  /// presentation layer by the means of this interface.</summary>
  IListViewPresentation = interface
    ['{85C07617-2BB7-44DC-BBCB-2E3FE422B006}']
    /// <summary>Called when ancestor's visible property is changed</summary>
    procedure AncestorVisibleChanged(const Visible: Boolean);
    /// <summary>Called when Parent is changed</summary>
    procedure ParentChanged;
    /// <summary>Called when position in parent list is changed</summary>
    procedure OrderChanged;
    /// <summary>Called when control size has changed</summary>
    procedure SizeChanged;
    /// <summary>Called when edit mode has changed</summary>
    procedure EditModeChanged;
    /// <summary>Called when visibility status has changed</summary>
    procedure StatusChanged;
    /// <summary>Called when item presentation requires update</summary>
    procedure ItemsUpdated;
    /// <summary>Item has been invalidated, presentation should reinitialize its view</summary>
    procedure ItemInvalidated(const Item: TListItem);
    /// <summary>Item has been selected by the control, presentation should update its selection</summary>
    procedure SetItemSelected(const ItemIndex: Integer; const Value: Boolean);
    /// <summary>Selected item index has been changed by the control, presentation should update selection</summary>
    procedure SetItemIndex(const ItemIndex: Integer);
    /// <summary>Force stop pull refresh</summary>
    procedure StopPullRefresh;
  end;

  /// <summary>Base interface from presented control to presentation</summary>
  IListViewCustomPresentationParent = interface
    ['{EBBE5FAA-F2B3-4606-AE32-8027DB97EC92}']
    /// <summary>Get root object, normally Root.GetObject</summary>
    function GetRootObject: TObject;
    /// <summary>Get content frame of the presented control</summary>
    function GetContentFrame: TRect;
    /// <summary>Get opacity of the presented control</summary>
    function GetControlOpacity: Single;
  end;

  /// <summary>Enumeration of internally used boolean flags</summary>
  TListViewModeFlag = (Edit, Enabled, Visible, Deletion, PullRefresh, Buttons, Search, SearchOnTop, PullRefreshWait,
    SwipeDelete);
  /// <summary>Set of boolean flags used internally</summary>
  TListViewModeFlags = set of TListViewModeFlag;

  /// <summary>View options specific to native iOS presentation</summary>
  TListViewNativeOption = (Grouped, Indexed, Styled);
  /// <summary>View options specific to native iOS presentation.
  /// <see cref='IListViewPresentationParent'>IListViewPresentationParent</see></summary>
  TListViewNativeOptions = set of TListViewNativeOption;

  /// <summary>Interface from presented control to presentation</summary>
  IListViewPresentationParent = interface(IListViewCustomPresentationParent)
    ['{F5657E45-0955-4A9F-9FE6-6C5E019846E4}']
    /// <summary>Obtains <c>IListViewAdapter</c></summary>
    function GetAdapter: IListViewAdapter;
    /// <summary>Obtains client rectangle for item with index <c>ItemIndex</c></summary>
    function GetItemClientRect(const ItemIndex: Integer): TRectF;
    /// <summary>Obtains item count</summary>
    function GetItemCount: Integer;
    /// <summary>Obtains height of item <c>ItemIndex</c></summary>
    function GetItemHeight(const ItemIndex: Integer): Integer;
    /// <summary>Guess item height before exact values can be calculated</summary>
    function GetEstimatedItemHeight: Single;
    /// <summary>Guess header height before exact values can be calculated</summary>
    function GetEstimatedHeaderHeight: Single;
    /// <summary>Guess footer height before exact values can be calculated</summary>
    function GetEstimatedFooterHeight: Single;
    /// <summary>Get main text of item <c>ItemIndex</c></summary>
    function GetItemText(const ItemIndex: Integer): string;
    /// <summary>Get index title for item <c>ItemIndex</c></summary>
    function GetItemIndexTitle(const ItemIndex: Integer): string;
    /// <summary>Query if item <c>ItemIndex</c> is selectable</summary>
    function CanSelectItem(const ItemIndex: Integer): Boolean;
    /// <summary>Notify control when item <c>ItemIndex</c> was selected</summary>
    procedure DidSelectItem(const ItemIndex: Integer);
    /// <summary>Query if item <c>ItemIndex</c> can be unselected</summary>
    function CanUnselectItem(const ItemIndex: Integer): Boolean;
    /// <summary>Notify control that item <c>ItemIndex</c> was unselected</summary>
    procedure DidUnselectItem(const ItemIndex: Integer);
    /// <summary>Notify control that an item button was clicked</summary>
    procedure ItemButtonClicked(const ItemIndex: Integer);
    /// <summary>Obtain presentation-specific boolean flags</summary>
    function GetFlags: TListViewModeFlags;
    /// <summary>Obtain native presentation-specific boolean flags</summary>
    function GetOptions: TListViewNativeOptions;
    /// <summary>Commit deletion of item <c>ItemIndex</c></summary>
    function DeleteItem(const ItemIndex: Integer): Boolean;
    /// <summary>Invoke pull refresh</summary>
    procedure InvokePullRefresh;
    /// <summary>Set search filter string</summary>
    procedure SetSearchFilter(const Filter: string);
    /// <summary>Pass scroll view position to the control</summary>
    procedure SetScrollViewPos(const Value: Single);
    /// <summary>Require rebuild of the list</summary>
    procedure RebuildList;
    /// <summary>Set boolean flag that indicates that native view is being created. It is used
    /// as a guard condition to prevent TListView logic from interfering with the presentation
    /// while a view is being initialized</summary>
    procedure SetCreatingNativeView(const Value: Boolean);
    /// <summary><c>True</c> if TListView is transparent</summary>
    function GetIsTransparent: Boolean;
    /// <summary>Obtain control opacity</summary>
    function GetOpacity: Single;
    /// <summary>Obtain background color defined by TListView style</summary>
    function GetBackgroundStyleColor: TAlphaColor;
    /// <summary>Request complete recreation of presentation</summary>
    procedure RecreateNativePresentation;
  end;

  /// <summary>Used to check if there is a design presentation attached to ListView</summary>
  IListViewDesignPresentationParent = interface
    ['{C62F3FE5-FE96-47A7-99CB-2EEBC85664FA}']
    ///<summary>True if design presentation is attached</summary>
    function HasDesignPresentationAttached: Boolean;
  end;

  /// <summary>A drawable shim extension to apply bounds changes from 
  /// the designer.</summary>
  IListViewDrawableShim = interface
    ['{9FB67E2A-37B9-473B-A95A-13EDD19ED91B}']
    ///<summary>Calculate appearance PlaceOffset, Width, Height for given rect from designer</summary>
    function CalcAppearanceBounds(const AValue: TRect; const CurrentBounds: TRectF): TRectF;
  end;

  /// <summary>Predicate used for item filtering.
  /// <see cref='IListViewFilterable'></see></summary>
  TFilterPredicate = TPredicate<string>;

  TListItemsList = TList<TListItem>;

  /// <summary>IListViewAdapter provides interface between the data and their representation.
  /// The essential part of this interface is implemented in FMX.ListView.Adapters.Base.TAbstractListViewAdapter
  /// </summary>
  IListViewAdapter = interface
    ['{6E850F76-BABD-4756-BF05-A30C66A692AD}']
    /// <summary>Return number of items that this Adapter represents. See Item[Index]. </summary>
    function GetCount: Integer;
    /// <summary>Get TListItem by index Index</summary>
    function GetItem(const Index: Integer): TListItem;
    /// <summary>Get index of given TListItem</summary>
    function IndexOf(const AItem: TListItem): Integer;
    /// <summary>Return TEnumerator&lt;TListItem&gt;</summary>
    function GetEnumerator: TEnumerator<TListItem>;
    /// <summary>Height of items that do not have it explicitly defined</summary>
    function GetDefaultViewHeight: Integer;
    /// <summary>Subscribe to OnChanged event</summary>
    procedure SetOnChanged(const Value: TNotifyEvent);
    /// <summary>Subscribe to OnItemsMayChange event</summary>
    procedure SetOnItemsMayChange(const Value: TNotifyEvent);
    /// <summary>Subscribe to OnItemsCouldHaveChanged event</summary>
    procedure SetOnItemsCouldHaveChanged(const Value: TNotifyEvent);
    /// <summary>Subscribe to OnItemsInvalidate event</summary>
    procedure SetOnItemsInvalidate(const Value: TNotifyEvent);
    /// <summary>Subscribe to OnItemsResize event</summary>
    procedure SetOnItemsResize(const Value: TNotifyEvent);
    /// <summary>Subscribe to OnResetView event</summary>
    procedure SetOnResetView(const Value: TNotifyEvent);

    /// <summary>Sort data</summary>
    procedure Sort(AComparer: IComparer<TListItem>);
    /// <summary>Called by TListView every time it needs to paint. In dynamic adapters this is where
    /// new TListItems should be created for data.</summary>
    procedure CreateNewViews;
    /// <summary>Recreate views for items that have specified purposes. May be called when item
    /// views may need update, for example when host TListView is being resized</summary>
    procedure ResetViews(const Purposes: TListItemPurposes);
    // <summary>Called by ResetViews to invoke OnResetView event for each ListItem</summary>
    procedure ResetView(const Item: TListItem);
    /// <summary>Get item by Index. Items would normally be already created by CreateNewViews by the time
    ///  this property is accessed. Index should be in range of [0, Count)</summary>
    property Item[const Index: Integer]: TListItem read GetItem; default;
    /// <summary>Count of items</summary>
    property Count: Integer read GetCount;
    /// <summary>Invoked when items have changed</summary>
    property OnChanged: TNotifyEvent write SetOnChanged;
    /// <summary>Invoked before an edit operation, items or their contents may change</summary>
    property OnItemsMayChange: TNotifyEvent write SetOnItemsMayChange;
    /// <summary>Invoked after an edit operation, items could have been changed</summary>
    property OnItemsCouldHaveChanged: TNotifyEvent write SetOnItemsCouldHaveChanged;
    /// <summary>Invoked when items are invalidated and a repaint is necessary</summary>
    property OnItemsInvalidate: TNotifyEvent write SetOnItemsInvalidate;
    /// <summary>Item sizes were changed, notify the view that the sizes of items need recalculation</summary>
    property OnItemsResize: TNotifyEvent write SetOnItemsResize;
    /// <summary>View has been reset</summary>
    property OnResetView: TNotifyEvent write SetOnResetView;
  end;

  IListViewEditor = interface;
  TBeforeItemAddedNotify = procedure(Sender: IListViewEditor) of object;
  TAfterItemAddedNotify = procedure(Sender: IListViewEditor; const Item: TListItem) of object;
  TBeforeItemDeletedNotify = procedure(Sender: IListViewEditor; const Index: Integer) of object;
  TAfterItemDeletedNotify = procedure(Sender: IListViewEditor) of object;

  /// <summary>Extension of IListViewAdapter for editable content. Implemented by TAppearanceListViewItems</summary>
  IListViewEditor = interface
    ['{19A0606B-8C8E-49B2-A6B3-A708B7B8AD46}']
    /// <summary>Create a new Item and append at end</summary>
    function Add: TListItem;
    /// <summary>Delete Item at Index</summary>
    procedure Delete(const Index: Integer);
    /// <summary>Create a new Item and insert at Index</summary>
    function Insert(const Index: Integer): TListItem;
    /// <summary>Delete all items</summary>
    procedure Clear;
    /// <summary>Set OnBeforeItemAdded handler</summary>
    procedure SetBeforeItemAdded(const AHandler: TBeforeItemAddedNotify);
    /// <summary>Set OnAfterItemAdded handler</summary>
    procedure SetAfterItemAdded(const AHandler: TAfterItemAddedNotify);
    /// <summary>Set OnBeforeItemDeleted handler</summary>
    procedure SetBeforeItemDeleted(const AHandler: TBeforeItemDeletedNotify);
    /// <summary>Set OnAfterItemDeleted handler</summary>
    procedure SetAfterItemDeleted(const AHandler: TAfterItemDeletedNotify);
    /// <summary>Notification before item is added</summary>
    property OnBeforeItemAdded: TBeforeItemAddedNotify write SetBeforeItemAdded;
    /// <summary>Notification after item is added</summary>
    property OnAfterItemAdded: TAfterItemAddedNotify write SetAfterItemAdded;
    /// <summary>Notification before item is deleted</summary>
    property OnBeforeItemDeleted: TBeforeItemDeletedNotify write SetBeforeItemDeleted;
    /// <summary>Notification after item is deleted</summary>
    property OnAfterItemDeleted: TAfterItemDeletedNotify write SetAfterItemDeleted;
  end;

  /// <summary>Extension of IListViewAdapter for items with checkboxes.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewCheckProvider = interface
    ['{032EB974-1C25-4B5E-BB07-01FA82554748}']
    /// <summary>Index of first checked item</summary>
    function FirstChecked(const Checked: Boolean = True): Integer;
    /// <summary>A quick query to see if one or all of the items are in checked state.</summary>
    /// <returns><para>If AChecked = True, True if at least one item is checked</para>
    /// <para> If AChecked = False, True if every item is checked</para>
    /// </returns>
    function AnyChecked(const AChecked: Boolean = True): Boolean;
    /// <summary>Change check state of all items</summary>
    procedure CheckAll(const NewState: Boolean = True);
    /// <summary>Get checked state for item at Index</summary>
    function GetChecked(const Index: Integer): Boolean;
    /// <summary>Set checked state for item at Index</summary>
    procedure SetChecked(const Index: Integer; const Value: Boolean);
    /// <summary>Get/set checked state of item at Index</summary>
    property Checked[const Index: Integer]: Boolean read GetChecked write SetChecked; default;
  end;

  /// <summary>Extension of IListViewAdapter for items with text and indexes.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewTextProvider = interface
    ['{C6D52C15-423D-4B2F-AC87-7D7D47A9C7CC}']
    /// <summary>Get primary text of item at Index</summary>
    function GetText(const Index: Integer): string;
    /// <summary>Get index title at Index</summary>
    function GetIndexTitle(const Index: Integer): string;
    /// <summary>Primary text of item at Index</summary>
    property Text[const Index: Integer]: string read GetText;
    /// <summary>Index title of item at Index</summary>
    property IndexTitle[const Index: Integer]: string read GetIndexTitle;
  end;

  /// <summary>Extension of IListViewAdapter for items with embedded text buttons.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewTextButtonProvider = interface
    ['{42CC3926-0A23-465B-9ECE-229C60B3BA8E}']
    /// <summary>Get drawable of text button for item at Index</summary>
    function GetTextButtonDrawable(const Index: Integer): TListItemTextButton;
    /// <summary>Drawable of text button for item at Index</summary>
    property TextButtonDrawable[const Index: Integer]: TListItemTextButton read GetTextButtonDrawable;
  end;

  /// <summary>Extension of IListViewAdapter for items with glyph buttons.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewGlyphButtonProvider = interface
    ['{64FF4B01-E378-4F40-A9A5-E4C1A7C942D6}']
    /// <summary>Get drawable of ןהםחנ button for item at Index</summary>
    function GetGlyphButtonDrawable(const Index: Integer): TListItemGlyphButton;
    /// <summary>Drawable of ןהםחנ button for item at Index</summary>
    property GlyphButtonDrawable[const Index: Integer]: TListItemGlyphButton read GetGlyphButtonDrawable;
  end;

  /// <summary>Extension of IListViewAdapter for items with data.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewExtrasProvider = interface
    ['{0BCFB611-3763-4C49-974F-1104F6116D6E}']
    /// <summary>Get indexed data DataIndex for item at Index</summary>
    function GetItemData(const Index: Integer; const DataIndex: string): TValue;
    /// <summary>Set indexed data DataIndex for item at Index</summary>
    procedure SetItemData(const Index: Integer; const DataIndex: string; const AValue: TValue);
    /// <summary>True if item at Index has indexed data DataIndex</summary>
    function GetHasData(const Index: Integer; const DataIndex: string): Boolean;
    /// <summary>Get tag of item at Index</summary>
    function GetItemTag(const Index: Integer): NativeInt;
    /// <summary>Set tag of item at Index</summary>
    procedure SetItemTag(const Index: Integer; const AValue: NativeInt);

    /// <summary>Indexed data DataIndex of item at Index</summary>
    property Data[const Index: Integer; const DataIndex: string]: TValue read GetItemData write SetItemData;
    /// <summary>Tag of item at Index</summary>
    property Tag[const Index: Integer]: NativeInt read GetItemTag write SetItemTag;
    /// <summary>True if item at Index has Indexed data DataIndex</summary>
    property HasData[const Index: Integer; const DataIndex: string]: Boolean read GetHasData;
  end;

  /// <summary>Extension of IListViewAdapter items of which can be filtered by a search box.
  /// Implemented by TAppearanceListViewItems</summary>
  IListViewFilterable = interface
    ['{02F85899-8787-4378-9622-105820EB4577}']
    /// <summary>Get filter predicate</summary>
    function GetFilterPredicate: TFilterPredicate;
    /// <summary>Set filter predicate</summary>
    procedure SetFilterPredicate(const Value: TFilterPredicate);
    /// <summary>Return a complete list of all items regardless of filter state</summary>
    function GetUnfilteredItems: TListItemsList;
    /// <summary>True if filter is applied</summary>
    function GetFiltered: Boolean;

    /// <summary>Notifies the adapter that all items have been deleted</summary>
    procedure ItemsCleared;
    /// <summary>Notifies the adapter that item at Index have been deleted</summary>
    procedure ItemDeleted(const Index: Integer);
    /// <summary>Notifies the adapter that item at Index have been added</summary>
    procedure ItemAdded(const Index: Integer; const Item: TListItem);

    /// <summary>Complete list of all items regardless of filter state</summary>
    property UnfilteredItems: TListItemsList read GetUnfilteredItems;
    /// <summary>True if filter is applied</summary>
    property Filtered: Boolean read GetFiltered;
    /// <summary>Filter predicate</summary>
    property Filter: TFilterPredicate read GetFilterPredicate write SetFilterPredicate;
  end;

implementation

uses
  System.RTLConsts, System.UIConsts, System.Math, System.Messaging, System.TypInfo, FMX.Consts, FMX.Styles,
  FMX.Platform;

{$REGION 'Global Functions'}

const
  GlyphOffsetX = -3;
  MinusOffsetX = 7;
  OpacityEpsilon = 0.003921569; // 1 / 255
  AnimationDeltaEpsilon = 0.01;

type
  TOpenControl = class(TControl);
{$IFDEF DRAW_ITEM_MARGINS}
var
  TempBrush: TBrush = nil;

function MarginBrush: TBrush;
begin
  if TempBrush = nil then
    TempBrush := TBrush.Create(TBrushKind.Solid, $50808080);

  Result := TempBrush;
end;

{$ENDIF}

function GetDeletingUnwantedOpacity(const Parent: TControl): Single;
var
  Controller: IListViewController;
begin
  if Supports(Parent, IListViewController, Controller) then
    if Controller.DeleteModeTransitionAlpha > AnimationDeltaEpsilon then
      Result := Max(0, 1 - (Controller.DeleteModeTransitionAlpha * 2))
    else
      Result := 1
  else
    Result := 1;
end;

function Blend(const Color1, Color2: TAlphaColor; const Alpha: Single): TAlphaColor;

  function BlendComponent(const Value1, Value2: Integer; const Alpha: Single): Integer; inline;
  begin
    Result := Value1 + Round((Value2 - Value1) * Alpha);
  end;

begin
  TAlphaColorRec(Result).R := BlendComponent(TAlphaColorRec(Color1).R, TAlphaColorRec(Color2).R, Alpha);
  TAlphaColorRec(Result).G := BlendComponent(TAlphaColorRec(Color1).G, TAlphaColorRec(Color2).G, Alpha);
  TAlphaColorRec(Result).B := BlendComponent(TAlphaColorRec(Color1).B, TAlphaColorRec(Color2).B, Alpha);
  TAlphaColorRec(Result).A := BlendComponent(TAlphaColorRec(Color1).A, TAlphaColorRec(Color2).A, Alpha);
end;


{$ENDREGION}
{$REGION 'List Item Object'}

constructor TListItemDrawable.Create(const AOwner: TListItem);
begin
  inherited Create;

  if AOwner <> nil then
  begin
    AOwner.View.Include(Self);
    FController := AOwner.Controller;
  end;

  FWidth := 0;
  FHeight := 0;
  FVisible := True;

  FAlign := TListItemAlign.Leading;
  FVertAlign := TListItemAlign.Leading;

  FUpdating := 0;
  NeedRepaint := False;
  FOpacity := 1;
end;

destructor TListItemDrawable.Destroy;
begin
  FController := nil;
  FCallback := nil;
  FPlaceOffsetX.Free;
  inherited;
end;

procedure TListItemDrawable.DoResize;
begin
  Invalidate;
end;

procedure TListItemDrawable.DoAlignChanged;
begin
end;

procedure TListItemDrawable.DoOpacityChange;
begin
  Invalidate;
end;

procedure TListItemDrawable.DoPlaceOffsetChanged;
begin
end;

procedure TListItemDrawable.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TListItemDrawable.DoUpdateValuesFromResources(const Resources: TListItemStyleResources;
  const Purpose: TListItemPurpose);
begin
end;

procedure TListItemDrawable.SetSize(const Value: TPointF);
var
  Adjusted: TPointF;
begin
  Adjusted := Value;
  if Adjusted.X < 0 then
    Adjusted.X := 0;
  if Adjusted.Y < 0 then
    Adjusted.Y := 0;

  if not SameValue(Adjusted.X, FWidth, TEpsilon.Position) or
    not SameValue(Adjusted.Y, FHeight, TEpsilon.Position) then
  begin
    FWidth := Adjusted.X;
    FHeight := Adjusted.Y;
    DoResize;
  end;
end;

function TListItemDrawable.GetSize: TPointF;
begin
  Result := TPointF.Create(FWidth, FHeight);
end;

procedure TListItemDrawable.SetOneDimension(const Index: Integer; const Value: Single);
var
  NewValue: Single;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;

  case Index of
    0:
      if FWidth <> NewValue then
      begin
        FWidth := NewValue;
        DoResize;
      end;
    1:
      if FHeight <> NewValue then
      begin
        FHeight := NewValue;
        DoResize;
      end;
  end;
end;

procedure TListItemDrawable.SetAlign(const Value: TListItemAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    DoAlignChanged;
    Invalidate;
  end;
end;

procedure TListItemDrawable.SetData(const Value: TValue);
begin
end;

procedure TListItemDrawable.SetInvalidateCallback(const Callback: TListItemCallback);
begin
  FCallback := Callback;
end;

procedure TListItemDrawable.SetVertAlign(const Value: TListItemAlign);
begin
  if FVertAlign <> Value then
  begin
    FVertAlign := Value;
    DoAlignChanged;
    Invalidate;
  end;
end;

procedure TListItemDrawable.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Invalidate;
  end;
end;

function TListItemDrawable.ToString: string;
  function GetClassName: string;
  begin
    if Self <> nil then
      Result := ClassName
    else
      Result := 'TListItemDrawable(nil)';
  end;
begin
  Result := Format('%s@%x', [GetClassName, NativeUInt(Self)]);
end;

procedure TListItemDrawable.UpdateValuesFromResources(const Resources: TListItemStyleResources;
  const Purpose: TListItemPurpose);
begin
  if FStyleValuesNeedUpdate <> [] then
  begin
    DoUpdateValuesFromResources(Resources, Purpose);
    FStyleValuesNeedUpdate := [];
  end;
end;

procedure TListItemDrawable.UpdateValuesFromStyle;
begin
  FStyleValuesNeedUpdate := [Low(TStyleResource)..High(TStyleResource)];
end;

procedure TListItemDrawable.SetOpacity(const Value: Single);
var
  NewValue: Single;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;
  if NewValue > 1 then
    NewValue := 1;

  if FOpacity <> NewValue then
  begin
    FOpacity := NewValue;
    DoOpacityChange;
  end;
end;

procedure TListItemDrawable.PlaceOffsetChanged(Sender: TObject);
begin
  Invalidate;
  DoPlaceOffsetChanged;
end;

procedure TListItemDrawable.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);

{$IFDEF PIXEL_ALIGNMENT}
  function PixelAlign(const Value: Single): Single;
  begin
    Result := Int(Value * SceneScale) / SceneScale
  end;
{$ELSE}
  function PixelAlign(const Value: Single): Single; inline;
  begin
    Result := Value;
  end;
{$ENDIF}

  procedure AlignValue(Align: TListItemAlign; out Left, Right: Single;
    Width, DesLeft, DesRight, CustomOffset: Single); inline;
  begin
    case Align of
      TListItemAlign.Center:
        begin
          Left := PixelAlign(CustomOffset + (DesLeft + DesRight - Width) * 0.5);
          Right := Left + Width;
        end;

      TListItemAlign.Trailing:
        begin
          Right := PixelAlign(CustomOffset + DesRight);
          Left := Right - Width;
        end;

    else
      begin
        Left := PixelAlign(CustomOffset + DesLeft);
        Right := Left + Width;
      end;
    end;
  end;

var
  LOffset: TPosition;

begin
  LOffset := PlaceOffset;
  if (FWidth > 0) and (FWidth < DestRect.Width) then
  begin
    AlignValue(FAlign, FLocalRect.Left, FLocalRect.Right, FWidth, DestRect.Left, DestRect.Right, LOffset.X);
  end
  else
  begin
    FLocalRect.Left := PixelAlign(DestRect.Left + LOffset.X);
    FLocalRect.Right := FLocalRect.Left + DestRect.Width;
  end;

  if (FHeight > 0) and (FHeight < DestRect.Height) then
  begin
    AlignValue(FVertAlign, FLocalRect.Top, FLocalRect.Bottom, FHeight, DestRect.Top, DestRect.Bottom, LOffset.Y);
  end
  else
  begin
    FLocalRect.Top := PixelAlign(DestRect.Top + LOffset.Y);
    FLocalRect.Bottom := FLocalRect.Top + DestRect.Height;
  end;
end;

procedure TListItemDrawable.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TListItemDrawable.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if (FUpdating <= 0) and NeedRepaint then
    begin
      NeedRepaint := False;
      Invalidate;
    end;
  end;
end;

function TListItemDrawable.InLocalRect(const Point: TPointF): Boolean;
begin
  Result := FLocalRect.Contains(Point);
end;

procedure TListItemDrawable.Invalidate;
begin
  if FUpdating < 1 then
  begin
    if Assigned(FCallback) then
      FCallback(nil, Self, TListItemCallbackOp.InvalidateOwner);
  end
  else
    NeedRepaint := True;
end;


function TListItemDrawable.ObjectAtPoint(const Point: TPointF): TControl;
begin
  Result := nil;
end;

function TListItemDrawable.GetData: TValue;
begin
  Result := TValue.Empty;
end;

function TListItemDrawable.GetPlaceOffset: TPosition;
begin
  if FPlaceOffsetX = nil then
  begin
    FPlaceOffsetX := TPosition.Create(TPointF.Zero);
    FPlaceOffsetX.OnChange := PlaceOffsetChanged;
  end;
  Result := FPlaceOffsetX;
end;

function TListItemDrawable.GetRenderPassCount: Integer;
begin
  Result := 0;
end;

function TListItemDrawable.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  Result := False;
end;

procedure TListItemDrawable.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
begin
end;

procedure TListItemDrawable.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
begin
end;

{$ENDREGION}
{$REGION 'List Item Text'}

constructor TListItemText.Create(const AOwner: TListItem);
begin
  inherited;

  FTextColor := claBlack;
  FSelectedTextColor := claWhite;
  FTextShadowColor := 0;
  FTextAlign := TTextAlign.Leading;
  FTextVertAlign := TTextAlign.Leading;

  UpdateValuesFromStyle;

  FWordWrap := False;
  FTrimming := TTextTrimming.None;
end;

destructor TListItemText.Destroy;
begin
  FreeAndNil(FTextLayout);
  FFontX.Free;
  FTextShadowOffsetX.Free;
  inherited;
end;

procedure TListItemText.DoUpdateValuesFromResources(const Resources: TListItemStyleResources;
  const Purpose: TListItemPurpose);
var
  ResourceFont: TFont;
begin
  ResourceFont := nil;
  if Resources <> nil then
  begin
    if Purpose <> TListItemPurpose.None then
    begin
      ResourceFont := Resources.HeaderTextFont;
      if (TStyleResource.TextColor in FStyleValuesNeedUpdate) and (Resources.HeaderTextColor > 0) then
        FTextColor := Resources.HeaderTextColor;
      if (TStyleResource.TextShadowColor in FStyleValuesNeedUpdate) and (Resources.HeaderTextShadowColor > 0) then
        FTextShadowColor := Resources.HeaderTextShadowColor;
      FTextVertAlign := TTextAlign.Center;
    end
    else if FIsDetailText then
    begin
      ResourceFont := Resources.DetailTextFont;
      if (TStyleResource.TextColor in FStyleValuesNeedUpdate) and (Resources.DetailTextColor > 0) then
        FTextColor := Resources.DetailTextColor;
      if (TStyleResource.SelectedTextColor in FStyleValuesNeedUpdate) and
        (Resources.DefaultTextSelectedColor > 0) then
        FSelectedTextColor := Resources.DefaultTextSelectedColor;
    end
    else
    begin
      ResourceFont := Resources.DefaultTextFont;
      if (TStyleResource.TextColor in FStyleValuesNeedUpdate) and (Resources.DefaultTextColor > 0) then
        FTextColor := Resources.DefaultTextColor;

      if (TStyleResource.SelectedTextColor in FStyleValuesNeedUpdate) and (Resources.DefaultTextSelectedColor > 0) then
        FSelectedTextColor := Resources.DefaultTextSelectedColor;
    end;
  end;

  if ResourceFont <> nil then
  begin
    if FFontX <> nil then
    begin
      FFontX.OnChanged := nil;
      try
        if TStyleResource.FontFamily in FStyleValuesNeedUpdate then
          FFontX.Family := ResourceFont.Family;
        if TStyleResource.FontSize in FStyleValuesNeedUpdate then
          FFontX.Size := ResourceFont.Size;
        if TStyleResource.FontStyle in FStyleValuesNeedUpdate then
          FFontX.StyleExt := ResourceFont.StyleExt;
      finally
        FFontX.OnChanged := FontChanged;
      end;
    end
    else
    begin
      Font.Assign(ResourceFont);
      Font.OnChanged := FontChanged;
    end;

    FStyleValuesNeedUpdate := FStyleValuesNeedUpdate - TextResources;
  end
  else
    Font.SetSettings(DefaultFontFamily, DefaultFontSize, Font.StyleExt);
end;

procedure TListItemText.DoResize;
begin
  inherited;
  LayoutChanged := True;
end;

procedure TListItemText.FontChanged(Sender: TObject);
var
  NewSettings: TFontSettings;
begin
  NewSettings := FontSettingsSnapshot;
  if not SameStr(NewSettings.Family, FFontSettings.Family) then
    Exclude(FStyleValuesNeedUpdate, TStyleResource.FontFamily);
  if not SameValue(NewSettings.Size, FFontSettings.Size, TEpsilon.Position) then
    Exclude(FStyleValuesNeedUpdate, TStyleResource.FontSize);
  if NewSettings.Style <> FFontSettings.Style then
    Exclude(FStyleValuesNeedUpdate, TStyleResource.FontStyle);
  FFontSettings := NewSettings;

  LayoutChanged := True;
  Invalidate;
end;

function TListItemText.FontSettingsSnapshot: TFontSettings;
begin
  Result.Size := FFontX.Size;
  Result.Style := FFontX.Style;
  Result.Family := FFontX.Family;
end;

function TListItemText.GetData: TValue;
begin
  Result := Text;
end;

function TListItemText.GetFont: TFont;
begin
  if FFontX = nil then
  begin
    FFontX := TFont.Create;
    FFontX.OnChanged := FontChanged;
  end;
  FFontSettings := FontSettingsSnapshot;
  Result := FFontX;
end;

function TListItemText.GetShadowOffset: TPosition;
begin
  if FTextShadowOffsetX = nil then
  begin
    FTextShadowOffsetX := TPosition.Create(ShadowOffset);
    FTextShadowOffsetX.OnChange := TextShadowOffsetChanged;
  end;
  Result := FTextShadowOffsetX;
end;

procedure TListItemText.TextShadowOffsetChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TListItemText.SetTextShadowColor(const Value: TAlphaColor);
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    Exclude(FStyleValuesNeedUpdate, TStyleResource.TextShadowColor);
    Invalidate;
  end;
end;

procedure TListItemText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Exclude(FStyleValuesNeedUpdate, TStyleResource.TextColor);
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetData(const AValue: TValue);
begin
  if AValue.IsEmpty then
    Text := ''
  else
    Text := AValue.ToString;
end;

procedure TListItemText.SetIsDetailText(const Value: Boolean);
begin
  if FIsDetailText <> Value then
    FIsDetailText := Value;
end;

procedure TListItemText.SetSelectedTextColor(const Value: TAlphaColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    Exclude(FStyleValuesNeedUpdate, TStyleResource.SelectedTextColor);
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTextVertAlign(const Value: TTextAlign);
begin
  if FTextVertAlign <> Value then
  begin
    FTextVertAlign := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    LayoutChanged := True;
    Invalidate;
  end;
end;

procedure TListItemText.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);
var
  NewRect: TRectF;
  DeadSpace: Single;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Item.Controller <> nil then
    begin
      DeadSpace := Item.Controller.GetItemEditOffset(Item) * Item.Controller.EditModeTransitionAlpha;
      NewRect.Left := NewRect.Left + DeadSpace;

      if FIsDetailText then
        NewRect.Right := Max(NewRect.Right + DeadSpace, NewRect.Left);
    end;

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates, Item);

    // Text should no go over right client area when moved in edit mode.
    if Item.Controller <> nil then
      FLocalRect.Right := Min(FLocalRect.Right, Item.Controller.GetClientMargins.Right);
  end
  else
    inherited;

  { Delete button cuts through the entire right side, so it needs to be marginalized globally.
    This is because LocalRect may actually leave the boundaries of DestRect depending on PlacementOffset. }
  if (TListItemDrawState.Deleting in DrawStates) and (Item.Controller <> nil) then
    FLocalRect.Right := Min(FLocalRect.Right, Item.Controller.GetItemDeleteCutoff(Item));
end;

procedure TListItemText.Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources; const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
var
  CurColor: TAlphaColor;
  SelectedAlpha, LOpacity: Single;
begin
  if SubPassNo <> 0 then
    Exit;

  if FTextLayout = nil then
  begin
    FTextLayout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create(Canvas);
    LayoutChanged := True
  end;

  if not LayoutChanged then
    LayoutChanged := (FTextLayout.MaxSize.X <> FLocalRect.Width) or (FTextLayout.MaxSize.Y <> FLocalRect.Height);

  CurColor := FTextColor;
  LOpacity := Params.AbsoluteOpacity;
  if TListItemDrawState.Selected in DrawStates then
  begin
    SelectedAlpha := Params.ItemSelectedAlpha;

    if (SelectedAlpha > 0) and (SelectedAlpha < 1) then
      CurColor := Blend(CurColor, FSelectedTextColor, SelectedAlpha)
    else if SelectedAlpha >= 1 then
      CurColor := FSelectedTextColor;
  end;

  if (not LayoutChanged) and (FTextLayout.Color <> CurColor) then
    LayoutChanged := True;

  if LayoutChanged then
  begin
    FTextLayout.BeginUpdate;
    try
      FTextLayout.HorizontalAlign := FTextAlign;
      FTextLayout.VerticalAlign := FTextVertAlign;
      FTextLayout.Font := Font;
      FTextLayout.Color := CurColor;
      FTextLayout.RightToLeft := False;
      FTextLayout.MaxSize := TPointF.Create(FLocalRect.Width, FLocalRect.Height);
      FTextLayout.Text := FText;
      FTextLayout.Trimming := FTrimming;
      FTextLayout.WordWrap := FWordWrap;
    finally
      FTextLayout.EndUpdate;
      LayoutChanged := False;
    end;
  end;

  if TAlphaColorRec(FTextShadowColor).A > 0 then
  begin
    FTextLayout.BeginUpdate;
    FTextLayout.Opacity := LOpacity;
    FTextLayout.Color := FTextShadowColor;
    FTextLayout.TopLeft := FLocalRect.TopLeft + TextShadowOffset.Point;
    FTextLayout.EndUpdate;
    FTextLayout.RenderLayout(Canvas);

    FTextLayout.BeginUpdate;
    FTextLayout.Color := CurColor;
    FTextLayout.EndUpdate;
  end;

{$IFDEF DRAW_ITEM_MARGINS}
  if FIsDetailText then
    MarginBrush.Color := $50FF0000
  else
    MarginBrush.Color := $5000FF00;

  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  FTextLayout.BeginUpdate;
  FTextLayout.Opacity := LOpacity;
  FTextLayout.TopLeft := FLocalRect.TopLeft;
  FTextLayout.EndUpdate;
  FTextLayout.RenderLayout(Canvas);
end;

{$ENDREGION}
{$REGION 'List Item Dummy'}

procedure TListItemDummy.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
begin
end;

{$ENDREGION}
{$REGION 'List Item Image'}

constructor TListItemImage.Create(const AOwner: TListItem);
begin
  inherited;
  FImageIndex := -1;
  FImageScalingMode := TImageScalingMode.StretchWithAspect;
end;

destructor TListItemImage.Destroy;
begin
  FStaticBitmap.Free;
  inherited;
end;

function TListItemImage.GetBitmap: TBitmap;
begin
  if FOwnsBitmap then
    Result := FStaticBitmap
  else
    Result := FReferBitmap;
end;

procedure TListItemImage.SetBitmap(const Value: TBitmap);
begin
  if FOwnsBitmap then
    FStaticBitmap := Value
  else
    FReferBitmap := Value;
  if FImageSource <> TImageSource.ImageList then
    Invalidate;
end;

procedure TListItemImage.SetData(const Value: TValue);
var
  LBitmap: TBitmap;
  LIndex: Integer;
  LEIndex: Extended;
begin
  if Value.TryAsType<TBitmap>(LBitmap) then
    Bitmap := LBitmap
  else if Value.TryAsType<Integer>(LIndex) then
    SetImageIndex(LIndex)
  else if Value.TryAsType<Extended>(LEIndex) then
    SetImageIndex(Trunc(LEIndex));
end;

procedure TListItemImage.SetOwnsBitmap(const Value: Boolean);
begin
  if FOwnsBitmap = Value then
    Exit;

  if FOwnsBitmap and (not Value) then
  begin
    FReferBitmap := FStaticBitmap;
    FStaticBitmap := nil;
  end
  else if (not FOwnsBitmap) and Value then
  begin
    FStaticBitmap := FReferBitmap;
    FReferBitmap := nil;
  end;

  FOwnsBitmap := Value;
end;

procedure TListItemImage.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TListItemImage.SetImageScalingMode(const Value: TImageScalingMode);
begin
  if FImageScalingMode <> Value then
  begin
    FImageScalingMode := Value;
    Invalidate;
  end;
end;

procedure TListItemImage.SetSrcRect(const Value: TRectF);
begin
  if FSrcRect <> Value then
  begin
    FSrcRect := Value;
    Invalidate;
  end;
end;

procedure TListItemImage.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);
var
  NewRect: TRectF;
  DeadSpace: Single;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Item.Controller <> nil then
    begin
      DeadSpace := Item.Controller.GetItemEditOffset(Item) * Item.Controller.EditModeTransitionAlpha;
      NewRect.Left := NewRect.Left + DeadSpace;
    end;

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates, Item);
  end
  else
    inherited;
end;

function TListItemImage.GetImageSource: TImageSource;
begin
  if FImageIndex <> -1 then
    FImageSource := TImageSource.ImageList
  else if GetBitmap <> nil then
    FImageSource := TImageSource.Bitmap
  else
    FImageSource := TImageSource.None;
  Result := FImageSource;
end;

procedure TListItemImage.FitInto(const Bitmap: TBitmap; var InputRect, DestinationRect: TRectF);
  procedure ClipRects(var InpRect, DestRect: TRectF; const LocalRect: TRectF);
  var
    Delta: Single;
  begin
    if DestRect.Right > LocalRect.Right then
    begin
      Delta := 1 - ((DestRect.Right - LocalRect.Right) / DestRect.Width);

      InpRect.Right := InpRect.Left + InpRect.Width * Delta;
      DestRect.Right := LocalRect.Right;
    end;

    if DestRect.Bottom > LocalRect.Bottom then
    begin
      Delta := 1 - ((DestRect.Bottom - LocalRect.Bottom) / DestRect.Height);

      InpRect.Bottom := InpRect.Top + InpRect.Height * Delta;
      DestRect.Bottom := LocalRect.Bottom;
    end;
  end;

var
  LocRect, InpRect, DestRect: TRectF;
  Aspect: Single;
  XAspect, YAspect: Single;
begin
  LocRect := FLocalRect;

  if FSrcRect.Width < 1 then
  begin
    InpRect.Left := 0;
    InpRect.Right := Bitmap.Width;
  end
  else
  begin
    InpRect.Left := FSrcRect.Left;
    InpRect.Right := FSrcRect.Right;
  end;

  if FSrcRect.Height < 1 then
  begin
    InpRect.Top := 0;
    InpRect.Bottom := Bitmap.Height;
  end
  else
  begin
    InpRect.Top := FSrcRect.Top;
    InpRect.Bottom := FSrcRect.Bottom;
  end;

  case FImageScalingMode of
    TImageScalingMode.Original:
      begin
        DestRect.Left := LocRect.Left;
        DestRect.Top := LocRect.Top;
        DestRect.Right := DestRect.Left + InpRect.Width;
        DestRect.Bottom := DestRect.Top + InpRect.Height;

        ClipRects(InpRect, DestRect, LocRect);

        // Center image
        DestRect.Offset((FLocalRect.Right - DestRect.Right) / 2, (FLocalRect.Bottom - DestRect.Bottom) / 2);
      end;

    TImageScalingMode.Stretch:
      DestRect := LocRect;

    TImageScalingMode.StretchWithAspect:
    begin
      // Calc ratios
      if InpRect.Width > 0 then
        XAspect := FLocalRect.Width / InpRect.Width
      else
        XAspect := 1;

      if InpRect.Height > 0 then
        YAspect := FLocalRect.Height / InpRect.Height
      else
        YAspect := 1;

      // Use smallest ratio
      if YAspect < XAspect then
        Aspect := YAspect
      else
        Aspect := XAspect;

      DestRect := FLocalRect;
      DestRect.Right := DestRect.Left + InpRect.Width * Aspect;
      DestRect.Bottom := DestRect.Top + InpRect.Height * Aspect;
      // Center image
      DestRect.Offset((FLocalRect.Right - DestRect.Right) / 2, (FLocalRect.Bottom - DestRect.Bottom) / 2);
    end;
  end;

  InputRect := InpRect;
  DestinationRect := DestRect;
end;

procedure TListItemImage.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
var
  Bitmap: TBitmap;
  InpRect, DestRect: TRectF;
begin
  if SubPassNo <> 0 then
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $50808000;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  if (ImageSource = TImageSource.ImageList) and (Params.Images <> nil) then
    Bitmap := Params.Images.Bitmap(TPointF(FLocalRect.Size) * Canvas.Scale, FImageIndex)
  else
    Bitmap := GetBitmap;

  if Bitmap = nil then
    Exit;

  FitInto(Bitmap, InpRect, DestRect);

  Canvas.DrawBitmap(Bitmap, InpRect, DestRect, Params.AbsoluteOpacity);
end;

{$ENDREGION}
{$REGION 'List Item Simple Control'}

constructor TListItemSimpleControl.Create(const AOwner: TListItem);
begin
  inherited;

  FEnabled := True;

{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  FTouchExpand := 8;
{$ENDIF}
end;

procedure TListItemSimpleControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    DoEnabledChange;
  end;
end;

function TListItemSimpleControl.PointInLocalRect(const Pos: TPointF): Boolean;
var
  LocRect: TRectF;
begin
  LocRect := FLocalRect;
  LocRect.Inflate(FTouchExpand, FTouchExpand);

  Result := LocRect.Contains(Pos);
end;

function TListItemSimpleControl.IsClickOpaque: Boolean;
begin
  Result := True;
end;

function TListItemSimpleControl.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  Result := False;

  if (Button = TMouseButton.mbLeft) and FEnabled and PointInLocalRect(MousePos) and IsClickOpaque then
  begin
    FPressed := True;
    FMouseOver := True;

    Invalidate;

    Result := True;
  end;
end;

procedure TListItemSimpleControl.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
begin
// On iOS, once "mouse" is outside of control area, the control never regains pressed/over state, requiring a new tap.
//  FMouseOver := FMouseOver and PointInLocalRect(MousePos);
  FMouseOver := PointInLocalRect(MousePos);
  Invalidate;
end;

procedure TListItemSimpleControl.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
var
  ShouldClick: Boolean;
begin
  if FPressed then
  begin
    ShouldClick := FMouseOver;

    FPressed := False;
    FMouseOver := False;

    if ShouldClick then
      DoClick;

    Invalidate;
  end;
end;

procedure TListItemSimpleControl.DoClick;
begin
  FCallback(nil, Self, TListItemCallbackOp.Click);
end;

procedure TListItemSimpleControl.DoEnabledChange;
begin
  Invalidate;
end;

procedure TListItemSimpleControl.Click;
begin
  DoClick;
end;

{$ENDREGION}
{$REGION 'List Item Accessory'}

constructor TListItemAccessory.Create(const AOwner: TListItem);
begin
  inherited;

end;

procedure TListItemAccessory.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);
var
  NewRect: TRectF;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;
    if FController <> nil then
      NewRect.Offset(FController.GetItemEditOffset(Item) * FController.EditModeTransitionAlpha, 0);

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates, Item);
  end
  else
    inherited;
end;

procedure TListItemAccessory.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
var
  Image: TStyleObject;
  TempOpacity, SelectedAlpha: Single;
begin
  if (SubPassNo <> 0) or (TListItemDrawState.Deleting in DrawStates) then
    Exit;

  TempOpacity := Params.AbsoluteOpacity;

                                                                          
                                                                                                                         
                                                                                              

  if TempOpacity < OpacityEpsilon then
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $500040FF;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  if Resources <> nil then
  begin
    if TListItemDrawState.Selected in DrawStates then
    begin // Apply crossfading between selected and non-selected states.
      SelectedAlpha := Params.ItemSelectedAlpha;

      if SelectedAlpha < 1 then
      begin
        Image := Resources.AccessoryImages[FAccessoryType].Normal;

        if Image <> nil then
          Image.DrawToCanvas(Canvas, FLocalRect, TempOpacity * (1 - SelectedAlpha));
      end;

      if SelectedAlpha > 0 then
      begin
        Image := Resources.AccessoryImages[FAccessoryType].Selected;

        if Image <> nil then
          Image.DrawToCanvas(Canvas, FLocalRect, TempOpacity * SelectedAlpha);
      end;
    end
    else
    begin // Draw normal non-selected image.
      Image := Resources.AccessoryImages[FAccessoryType].Normal;

      if Image <> nil then
        Image.DrawToCanvas(Canvas, FLocalRect, TempOpacity);
    end;
  end;
end;

procedure TListItemAccessory.SetAccessoryType(Value: TAccessoryType);
begin
  if FAccessoryType <> Value then
  begin
    FAccessoryType := Value;
    Invalidate;
  end;
end;

{$ENDREGION}
{$REGION 'List Item Glyph Button'}

constructor TListItemGlyphButton.Create(const AOwner: TListItem);
begin
  inherited;
  FTransitionEnabled := False;
  FTransitionAlpha := 0;
end;

destructor TListItemGlyphButton.Destroy;
begin
  FreeAndNil(FTransitionTimer);
  inherited;
end;

procedure TListItemGlyphButton.SetButtonType(const Value: TGlyphButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Invalidate;
  end;
end;

procedure TListItemGlyphButton.SetData(const AValue: TValue);
var
  LChecked: Boolean;
begin
  if AValue.TryAsType<Boolean>(LChecked) then
    Checked := LChecked;
end;

procedure TListItemGlyphButton.InitCheckedTransition;
begin
  FTransitionEnabled := True;

  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);

  if FTransitionTimer = nil then
  begin
    FTransitionTimer := TTimer.Create(nil);
    FTransitionTimer.Enabled := False;
    FTransitionTimer.Interval := Round(1000 / CheckedAnimationFrameRate);
    FTransitionTimer.OnTimer := TransitionTimerNotify;
  end;

  FTransitionTimer.Enabled := True;

  if FTimerService <> nil then
    FTransitionStartTicks := FTimerService.GetTick
  else
    FTransitionStartTicks := 0;
end;

procedure TListItemGlyphButton.ResetCheckedTransition;
begin
  FTransitionEnabled := False;

  if FTransitionTimer <> nil then
  begin
    FTransitionTimer.Enabled := False;
    FTransitionTimer.Parent := nil;
    FreeAndNil(FTransitionTimer);
  end;

  if FTimerService <> nil then
    FTimerService := nil;

  if FChecked then
    FTransitionAlpha := 1
  else
    FTransitionAlpha := 0;
end;

procedure TListItemGlyphButton.TransitionTimerNotify(Sender: TObject);
const
  CheckedAnimationIncrement = 1 / (60 * CheckedAnimationDuration);
begin
  if FChecked then
  begin
    if FTimerService <> nil then
      FTransitionAlpha := Min(Abs(FTimerService.GetTick - FTransitionStartTicks) / CheckedAnimationDuration, 1)
    else
      FTransitionAlpha := FTransitionAlpha + CheckedAnimationIncrement;

    if FTransitionAlpha >= 1 then
      ResetCheckedTransition;
  end
  else
  begin
    if FTimerService <> nil then
      FTransitionAlpha := Max(1 - (Abs(FTimerService.GetTick - FTransitionStartTicks) / CheckedAnimationDuration), 0)
    else
      FTransitionAlpha := FTransitionAlpha - CheckedAnimationIncrement;

    if FTransitionAlpha <= 0 then
      ResetCheckedTransition;
  end;

  Invalidate;
end;

procedure TListItemGlyphButton.DoClick;
begin
  inherited;
  if not FClickOnSelect then
    FCallback(nil, Self, TListItemCallbackOp.Click);
end;

procedure TListItemGlyphButton.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    InitCheckedTransition;
    FChecked := Value;
    if not FTransitionEnabled then
      ResetCheckedTransition;
    Invalidate;
  end;
end;

procedure TListItemGlyphButton.DoSelect;
begin
  inherited;
  if FClickOnSelect then
    FCallback(nil, Self, TListItemCallbackOp.Click);
end;

function TListItemGlyphButton.MouseDown(const Button: TMouseButton; const Shift: TShiftState;
  const MousePos: TPointF): Boolean;
begin
  if not FClickOnSelect then
    Result := inherited MouseDown(Button, Shift, MousePos)
  else
    Result := False;
end;

procedure TListItemGlyphButton.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);
var
  NewRect: TRectF;
begin
  if TListItemDrawState.EditMode in DrawStates then
  begin
    NewRect := DestRect;

    if Item.Controller <> nil then
      NewRect.Offset(Item.Controller.GetItemEditOffset(Item) * (Item.Controller.EditModeTransitionAlpha - 1), 0);

    inherited CalculateLocalRect(NewRect, SceneScale, DrawStates, Item);
  end
  else
    inherited;

{$IF DEFINED(MSWINDOWS) OR DEFINED(OSX)}
  FLocalRect := FLocalRect.SnapToPixel(SceneScale, False);
{$ENDIF}
end;

procedure TListItemGlyphButton.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
var
  ReallyPressed: Boolean;
  TempOpacity: Single;
  PrevMatrix, M: TMatrix;
  LocRect: TRectF;
  CenterShift: TPointF;
  RenderGlyphButtonType: TGlyphButtonType;
begin
  if SubPassNo <> 0 then
    Exit;

  ReallyPressed := FPressed and FMouseOver;

  LocRect := FLocalRect;
  TempOpacity := Params.AbsoluteOpacity;

  if not FEnabled then
    TempOpacity := TempOpacity * DisabledOpacity;

  if TListItemDrawState.EditMode in DrawStates then
    TempOpacity := TempOpacity * Max(0.1, FController.EditModeTransitionAlpha * 2 - 1)
  else
    Exit;

{$IFDEF DRAW_ITEM_MARGINS}
  MarginBrush.Color := $50804020;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

  if Resources <> nil then
  begin
    RenderGlyphButtonType := FButtonType;

    if (RenderGlyphButtonType = TGlyphButtonType.Delete) and (not FController.IsDeleteModeAllowed) then
      RenderGlyphButtonType := TGlyphButtonType.Checkbox;
    case RenderGlyphButtonType of
      TGlyphButtonType.Add:
        begin
          if (not ReallyPressed) and (Resources.ButtonAddItemStyleImage.Normal <> nil) then
            Resources.ButtonAddItemStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if ReallyPressed and (Resources.ButtonAddItemStyleImage.Pressed <> nil) then
            Resources.ButtonAddItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);
        end;

      TGlyphButtonType.Delete:
        begin
          LocRect.Left := LocRect.Left + MinusOffsetX;
          if Resources.ButtonDeleteItemStyleImage.Normal <> nil then
            Resources.ButtonDeleteItemStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if Resources.ButtonDeleteItemStyleImage.Pressed <> nil then
          begin
            if FTransitionAlpha <= 0 then
              Resources.ButtonDeleteItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity)
            else
            begin
              PrevMatrix := Canvas.Matrix;

              CenterShift.X := LocRect.Left + LocRect.Width * 0.5;
              CenterShift.Y := LocRect.Top + LocRect.Height * 0.5;

              M := TMatrix.CreateTranslation(-CenterShift.X, -CenterShift.Y)
                * TMatrix.CreateRotation(-FTransitionAlpha * Pi * 0.5)
                * TMatrix.CreateTranslation(CenterShift.X, CenterShift.Y)
                * PrevMatrix;

              Canvas.SetMatrix(M);

              Resources.ButtonDeleteItemStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);

              Canvas.SetMatrix(PrevMatrix);
            end;
          end;
        end;

      TGlyphButtonType.Checkbox:
        begin
          if (not FChecked) and (Resources.ButtonCheckboxStyleImage.Normal <> nil) then
            Resources.ButtonCheckboxStyleImage.Normal.DrawToCanvas(Canvas, LocRect, TempOpacity);

          if FChecked and (Resources.ButtonCheckboxStyleImage.Pressed <> nil) then
            Resources.ButtonCheckboxStyleImage.Pressed.DrawToCanvas(Canvas, LocRect, TempOpacity);
        end;
    end;
  end;
end;

{$ENDREGION}
{$REGION 'List Item Text Button'}


{ TListItemTextButton }

constructor TListItemTextButton.Create(const AOwner: TListItem);
begin
  FTextDrawable := TListItemText.Create(nil);
  inherited;

  FTintColor := TAlphaColorRec.Null;
  FPressedTextColor := claWhite;

  UpdateValuesFromStyle;
end;

destructor TListItemTextButton.Destroy;
begin
  FTextDrawable.Free;
  inherited;
end;

procedure TListItemTextButton.CalculateLocalRect(const DestRect: TRectF; const SceneScale: Single;
  const DrawStates: TListItemDrawStates; const Item: TListItem);
begin
  inherited;

  FTextDrawable.CalculateLocalRect(DestRect, SceneScale, DrawStates, Item);
  FLocalRect := FTextDrawable.FLocalRect;
{$IF DEFINED(MSWINDOWS) OR DEFINED(OSX)}
  FLocalRect := FLocalRect.SnapToPixel(SceneScale, False);
{$ENDIF}
end;

procedure TListItemTextButton.DoAlignChanged;
begin
  inherited;
  FTextDrawable.Align := Align;
  FTextDrawable.VertAlign := VertAlign;
end;

procedure TListItemTextButton.DoOpacityChange;
begin
  inherited;
  FTextDrawable.DoOpacityChange;
end;

procedure TListItemTextButton.DoPlaceOffsetChanged;
begin
  FTextDrawable.PlaceOffset.Point := PlaceOffset.Point;
end;

procedure TListItemTextButton.DoResize;
begin
  inherited;
  FTextDrawable.SetSize(Size);
end;


procedure TListItemTextButton.DoUpdateValuesFromResources(const Resources: TListItemStyleResources;
  const Purpose: TListItemPurpose);
var
 Surrogate: TListItemStyleResources;
begin
  Surrogate := TListItemStyleResources.Create(Resources);
  try
    case FButtonType of
      TTextButtonType.Normal:
        begin
          Surrogate.DefaultTextFont := Resources.ButtonTextFont;
          Surrogate.HeaderTextFont := Resources.ButtonTextFont;

          if TStyleResource.TextColor in FStyleValuesNeedUpdate then
          begin
            FTextColor := Resources.ButtonTextColor;
            FPressedTextColor := Resources.ButtonTextPressedColor;
          end;
        end;
      TTextButtonType.Delete:
        begin
          Surrogate.DefaultTextFont := Resources.DeleteButtonTextFont;
          Surrogate.HeaderTextFont := Resources.DeleteButtonTextFont;

          if TStyleResource.TextColor in FStyleValuesNeedUpdate then
          begin
            FTextColor := Resources.DeleteButtonTextColor;
            FPressedTextColor := Resources.DeleteButtonTextPressedColor;
          end;
        end;
    end;

    FTextDrawable.DoUpdateValuesFromResources(Surrogate, Purpose);

    FStyleValuesNeedUpdate := FStyleValuesNeedUpdate - TextResources;

  finally
    Surrogate.Free;
  end;
end;

procedure TListItemTextButton.SetData(const AValue: TValue);
begin
  if AValue.IsEmpty then
    Text := ''
  else
    Text := AValue.ToString;
end;

procedure TListItemTextButton.SetInvalidateCallback(const Callback: TListItemCallback);
begin
  inherited;
  FTextDrawable.InvalidateCallback := FCallback;
end;

function TListItemTextButton.GetRenderPassCount: Integer;
begin
  if FTextDrawable.Text.Length > 0 then
    Result := 2
  else
    Result := inherited;
end;

function TListItemTextButton.IsClickOpaque: Boolean;
begin
  Result := inherited;

  if Result and (FController <> nil) and (FController.EditModeTransitionAlpha > AnimationDeltaEpsilon) then
    Result := False;
end;

procedure TListItemTextButton.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer);
var
  ReallyPressed: Boolean;
  TempOpacity: Single;
begin
  TempOpacity := Params.AbsoluteOpacity;

  if not FEnabled then
    TempOpacity := TempOpacity * DisabledOpacity;

  if TListItemDrawState.Deleting in DrawStates then
    TempOpacity := TempOpacity * Params.DeletingUnwantedOpacity;

  if (TListItemDrawState.EditMode in DrawStates) and (FController <> nil) then
      TempOpacity := TempOpacity * (1 - FController.EditModeTransitionAlpha);

  if TempOpacity < OpacityEpsilon then
    Exit;

  ReallyPressed := FPressed and FMouseOver;

  // background
  if SubPassNo = 0 then
  begin
{$IFDEF DRAW_ITEM_MARGINS}
    MarginBrush.Color := $50804020;
    Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, MarginBrush);
{$ENDIF}

    if Resources <> nil then
    begin
      case FButtonType of
        TTextButtonType.Normal:
          begin
            if (not ReallyPressed) and (Resources.ButtonNormalStyleImage.Normal <> nil) then
              Resources.ButtonNormalStyleImage.Normal.DrawToCanvas(Canvas, FLocalRect, FTintColor, TempOpacity);

            if ReallyPressed and (Resources.ButtonNormalStyleImage.Pressed <> nil) then
              Resources.ButtonNormalStyleImage.Pressed.DrawToCanvas(Canvas, FLocalRect, FTintColor, TempOpacity);
          end;

        TTextButtonType.Delete:
          begin
            if (not ReallyPressed) and (Resources.ButtonDeleteStyleImage.Normal <> nil) then
              Resources.ButtonDeleteStyleImage.Normal.DrawToCanvas(Canvas, FLocalRect, FTintColor, TempOpacity);

            if ReallyPressed and (Resources.ButtonDeleteStyleImage.Pressed <> nil) then
              Resources.ButtonDeleteStyleImage.Pressed.DrawToCanvas(Canvas, FLocalRect, FTintColor, TempOpacity);
          end;
      end;
    end;
  end
  else if SubPassNo = 1 then
  begin
    if ReallyPressed then
    begin
      FTextDrawable.FTextColor := FPressedTextColor;
      FTextDrawable.FSelectedTextColor := FPressedTextColor;
    end
    else
    begin
      FTextDrawable.FTextColor := FTextColor;
      FTextDrawable.FSelectedTextColor := FTextColor;
    end;
    FTextDrawable.Render(Canvas, DrawItemIndex, DrawStates, Resources, Params, 0);
  end;
end;

procedure TListItemTextButton.SetButtonType(const Value: TTextButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    UpdateValuesFromStyle;
    Invalidate;
  end;
end;


function TListItemTextButton.GetTextColor: TAlphaColor;
begin
  Result := FTextDrawable.TextColor;
end;

function TListItemTextButton.GetTextShadowColor: TAlphaColor;
begin
  Result := FTextDrawable.TextShadowColor;
end;

function TListItemTextButton.GetTextShadowOffset: TPosition;
begin
  Result := FTextDrawable.TextShadowOffset;
end;

procedure TListItemTextButton.SetText(const Value: string);
begin
  FTextDrawable.FUpdating := FUpdating;
  try
    FTextDrawable.Text := Value;
    if FTextDrawable.NeedRepaint then
      Invalidate;
  finally
    FTextDrawable.FUpdating := 0;
  end;
end;

function TListItemTextButton.GetText: string;
begin
  Result := FTextDrawable.Text;
end;


procedure TListItemTextButton.SetTextAlign(const Value: TTextAlign);
begin
  FTextDrawable.TextAlign := Value;
  if FTextDrawable.NeedRepaint then
    Invalidate;
end;

function TListItemTextButton.GetTextAlign: TTextAlign;
begin
  Result := FTextDrawable.TextAlign;
end;

function TListItemTextButton.GetFont: TFont;
begin
  Result := FTextDrawable.Font;
end;

procedure TListItemTextButton.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    FTextDrawable.TextColor := Value;
    Exclude(FStyleValuesNeedUpdate, TStyleResource.TextColor);
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetPressedTextColor(const Value: TAlphaColor);
begin
  if FPressedTextColor <> Value then
  begin
    FPressedTextColor := Value;
    Exclude(FStyleValuesNeedUpdate, TStyleResource.PressedTextColor);
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTextShadowColor(const Value: TAlphaColor);
begin
  FTextDrawable.TextShadowColor := Value;
  Exclude(FStyleValuesNeedUpdate, TStyleResource.TextShadowColor);
  if FTextDrawable.NeedRepaint then
    Invalidate;
end;

procedure TListItemTextButton.SetTextVertAlign(const Value: TTextAlign);
begin
  FTextDrawable.TextVertAlign := Value;
  if FTextDrawable.NeedRepaint then
    Invalidate;
end;

function TListItemTextButton.GetTextVertAlign: TTextAlign;
begin
  Result := FTextDrawable.TextVertAlign;
end;

procedure TListItemTextButton.SetTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    Invalidate;
  end;
end;

procedure TListItemTextButton.SetTrimming(const Value: TTextTrimming);
begin
  FTextDrawable.Trimming := Value;
  if FTextDrawable.NeedRepaint then
    Invalidate;
end;

function TListItemTextButton.GetTrimming: TTextTrimming;
begin
  Result := FTextDrawable.Trimming;
end;

procedure TListItemTextButton.SetWordWrap(const Value: Boolean);
begin
  FTextDrawable.WordWrap := Value;
  if FTextDrawable.NeedRepaint then
    Invalidate;
end;

function TListItemTextButton.GetWordWrap: Boolean;
begin
  Result := FTextDrawable.WordWrap;
end;

{$ENDREGION}
{$REGION 'List Item Control Scene'}

constructor TListItemControlScene.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TListItemControlScene.Destroy;
begin
  inherited;
end;

procedure TListItemControlScene.DisableUpdating;
begin
  Inc(FDisableUpdating);
end;

procedure TListItemControlScene.SetContainer(const Container: TControl);
begin
  FContainer := Container;
end;

procedure TListItemControlScene.SetOwnerItem(const Item: TListItem);
begin
  FOwnerItem := Item;
end;

procedure TListItemControlScene.SetStyleBook(const Value: TStyleBook);
begin
end;

procedure TListItemControlScene.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl then
  begin
    TControl(AObject).SetNewScene(Self);
    TControl(AObject).DisableDisappear := True;
  end;
  AObject.SetRoot(RealScene.GetObject.Root);
end;

procedure TListItemControlScene.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  AObject.SetRoot(nil);
  if AObject is TControl then
    TControl(AObject).SetNewScene(nil);
end;

procedure TListItemControlScene.EnableUpdating;
begin
  Dec(FDisableUpdating);
  if FDisableUpdating < 0 then
    raise EInvalidSceneUpdatingPairCall.Create(SInvalidSceneUpdatingPairCall);
end;

function TListItemControlScene.GetStyleBook: TStyleBook;
begin
  Result := nil;
end;

function TListItemControlScene.GetCanvas: TCanvas;
begin
  Result := FCanvas
end;

function TListItemControlScene.GetRealScene: IScene;
begin
  Result := FOwnerItem.Controller.GetScene;
end;

function TListItemControlScene.GetSceneScale: Single;
begin
  Result := RealScene.GetSceneScale;
end;

function TListItemControlScene.GetObject: TFmxObject;
begin
  Result := Self;
end;

procedure TListItemControlScene.ChangeScrollingState(const AControl: TControl; const Active: Boolean);
begin
end;

function TListItemControlScene.LocalToScreen(P: TPointF): TPointF;
begin
  Result := RealScene.LocalToScreen(P);
end;

function TListItemControlScene.ScreenToLocal(P: TPointF): TPointF;
begin
  Result := RealScene.ScreenToLocal(P);
end;

function TListItemControlScene.GetUpdateRectsCount: Integer;
begin
  Result := 1;
end;

function TListItemControlScene.GetUpdateRect(const Index: Integer): TRectF;
begin
  Result := TRectF.Create(0, 0, FLayoutSize.X, FLayoutSize.Y);
end;

procedure TListItemControlScene.AddUpdateRect(R: TRectF);
begin
  if FDisableUpdating = 0 then
    FOwnerItem.Invalidate;
end;

procedure TListItemControlScene.RepaintScene(const Canvas: TCanvas);
var
  Index: Integer;
  {$IFDEF CPUARM}[unsafe]{$ENDIF}Control: TOpenControl;
begin
  if (not FDrawing) then
  begin
    FDrawing := True;
    FCanvas := Canvas;
    try
      for Index := 0 to ChildrenCount - 1 do
        if (Children[Index] is TControl) and
          (TControl(Children[Index]).Visible or ((not TControl(Children[Index]).Visible) and
          (csDesigning in ComponentState) and (not TControl(Children[Index]).Locked))) then
        begin
          Control := TOpenControl(Children[Index]);
          TOpenControl(Control).PaintInternal;
        end;
    finally
      FCanvas := nil;
      FDrawing := False;
    end;
  end;
end;

{$ENDREGION}
{$REGION 'List Item Control Container'}

constructor TListItemControlContainer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TListItemControlContainer.Destroy;
begin
  inherited;
end;

{$ENDREGION}
{$REGION 'List Item Embedded Control'}

constructor TListItemEmbeddedControl.Create(const AOwner: TListItem);
begin
  inherited;

  FScene := TListItemControlScene.Create(nil);
  FScene.SetOwnerItem(AOwner);

  FContainer := TListItemControlContainer.Create(nil);
  FContainer.Parent := FScene;
  FContainer.FItemOwner := Self;

  FScene.SetContainer(FContainer);
end;

destructor TListItemEmbeddedControl.Destroy;
begin
  FContainer.Free;
  FScene.Free;

  inherited;
end;

procedure TListItemEmbeddedControl.Render(const Canvas: TCanvas; const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates; const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams; const SubPassNo: Integer = 0);
var
  BoundsPos: TPointF;
begin
  if SubPassNo <> 0 then
    Exit;

  FScene.FLayoutSize := TPoint.Create(Trunc(Canvas.Width), Trunc(Canvas.Height));

  BoundsPos := FLocalRect.TopLeft;
  BoundsPos := BoundsPos + Params.ParentAbsoluteRect.TopLeft;

  FContainer.SetBounds(BoundsPos.X, BoundsPos.Y, FLocalRect.Width, FLocalRect.Height);
  FScene.RepaintScene(Canvas);
end;

function TListItemEmbeddedControl.ObjectAtPoint(const Point: TPointF): TControl;
var
  Control: IControl;
begin
  Control := FContainer.ObjectAtPoint(Point);
  if (Control <> nil) and (Control.GetObject is TControl) and (Control.GetObject <> FContainer) then
    Result := TControl(Control.GetObject)
  else
    Result := nil;
end;

{$ENDREGION}
{$REGION 'List Item'}

constructor TListItem.Create(const AAdapter: IListViewAdapter;
  const AController: IListViewController);
begin
  inherited Create;
  FController := AController;
  FAdapter := AAdapter;
  FView := ListItemObjectsClass.Create(Self);
  FView.Callback := procedure(View: TListItemView; Drawable: TListItemDrawable; Op: TListItemCallbackOp) begin
    case Op of
      TListItemCallbackOp.CreateDrawables:
        CreateObjects;
      TListItemCallbackOp.InvalidateOwner:
        Invalidate;
      TListItemCallbackOp.Click:
        FController.ControlClicked(Self, Drawable);
    end;
  end;
  FHeaderRef := -1;
  FIndex := -1;
  FUpdating := 0;
  FNeedRepaint := False;
end;

procedure TListItem.CreateObjects;
begin
//
end;

destructor TListItem.Destroy;
begin
  FView.Free;
  inherited;
end;

function TListItem.GetController: IListViewController;
begin
  Result := FController;
end;

function TListItem.GetCount: Integer;
begin
  Result := FView.Count;
end;

procedure TListItem.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TListItem.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if (FUpdating <= 0) and FNeedRepaint then
    begin
      FNeedRepaint := False;
      Invalidate;
    end;
  end;
end;

procedure TListItem.Invalidate;
begin
  if FUpdating < 1 then
    Repaint
  else
    FNeedRepaint := True;
end;

procedure TListItem.SetHeight(const Value: Integer);
var
  NewValue: Integer;
begin
  NewValue := Value;
  if NewValue < 0 then
    NewValue := 0;

  if NewValue <> FHeight then
  begin
    FHeight := NewValue;
    InvalidateHeights;
  end;
end;

procedure TListItem.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

function TListItem.GetIndex: Integer;
begin
  Result := FIndex;
end;

procedure TListItem.SetPurpose(const AValue: TListItemPurpose);
begin
  if AValue <> FPurpose then
  begin
    FPurpose := AValue;
    InvalidateHeights;
  end;
end;

function TListItem.ToString: string;
  function GetClassName: string;
  begin
    if Self <> nil then
      Result := ClassName
    else
      Result := 'TListItem(nil)';
  end;

  function GetPurpose: string;
  begin
    if Self <> nil then
      Result := Purpose.ToString
    else
      Result := '(unknown)';
  end;

  function GetViewCount: Integer;
  begin
    if (Self <> nil) and (Self.View <> nil) then
      Result := Self.View.Count
    else
      Result := -1;
  end;
begin
  Result := Format('%s@%x Purpose=%s |View|=%d', [GetClassName, NativeUInt(Self), GetPurpose, GetViewCount]);
end;

procedure TListItem.WillBePainted;
begin
end;

procedure TListItem.InvalidateHeights;
begin
  //
end;

function TListItem.ListItemObjectsClass: TListItemViewType;
begin
  Result := TListItemView;
end;

function TListItem.ObjectAtPoint(const Point: TPointF): TControl;
var
  I: Integer;
  Control: TControl;
begin
  Result := nil;
  for I := 0 to FView.Count - 1 do
    if FView[I] <> nil then
    begin
      Control := FView[I].ObjectAtPoint(Point);
      if Control <> nil then
        Exit(Control);
    end;
end;

procedure TListItem.Repaint;
begin
  FController.ItemInvalidated(Self);
end;

function TListItem.MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FView.Count - 1 do
    if (FView[I] <> nil) and FView[I].Visible then
    begin
      Result := FView[I].MouseDown(Button, Shift, MousePos);
      if Result then
        Break;
    end;
end;

procedure TListItem.MouseMove(const Shift: TShiftState; const MousePos: TPointF);
var
  I: Integer;
begin
  for I := 0 to FView.Count - 1 do
    if (FView[I] <> nil) and FView[I].Visible then
      FView[I].MouseMove(Shift, MousePos);
end;

procedure TListItem.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF);
var
  I: Integer;
begin
  for I := 0 to FView.Count - 1 do
    if (FView[I] <> nil) and FView[I].Visible then
      FView[I].MouseUp(Button, Shift, MousePos);
end;

procedure TListItem.MouseSelect;
var
  I: Integer;
begin
  for I := 0 to FView.Count - 1 do
    if (FView[I] <> nil) and FView[I].Visible then
      FView[I].DoSelect;
end;

function TListItem.HasClickOnSelectItems: Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FView.Count - 1 do
    if (FView[I] <> nil) and FView[I].Visible and (FView[I] is TListItemGlyphButton) then
    begin
      Result := TListItemGlyphButton(FView[I]).ClickOnSelect;
      if Result then
        Break;
    end;
end;

{$ENDREGION}
{$REGION 'List Item Objects'}

constructor TListItemView.Create(const AOwner: TListItem);
begin
  inherited Create;
  FViewList := TListItemView.TViewList.Create;
end;

function TListItemView.GetCount: Integer;
begin
  FCallback(Self, nil, TListItemCallbackOp.CreateDrawables);
  Result := FViewList.Count;
end;

function TListItemView.GetViewList: TViewList;
begin
  Result := FViewList;
end;

function TListItemView.GetObject(const Index: Integer): TListItemDrawable;
begin
  Result := ViewList[Index];
end;

function TListItemView.Add(const AItem: TListItemDrawable): Integer;
begin
  Result := ViewList.Add(AItem);
  FCallback(Self, nil, TListItemCallbackOp.InvalidateOwner);
end;

procedure TListItemView.Insert(const Index: Integer; const Value: TListItemDrawable);
begin
  ViewList.Insert(Index, Value);
end;

procedure TListItemView.Clear;
begin
  ViewList.Clear;
end;

procedure TListItemView.Delete(Index: Integer);
begin
  ViewList.Delete(Index);
  FCallback(Self, nil, TListItemCallbackOp.InvalidateOwner);
end;

destructor TListItemView.Destroy;
begin
  FCallback := nil;
  FViewList.Free;
  inherited;
end;

procedure TListItemView.Include(const AItem: TListItemDrawable);
begin
  if ViewList.IndexOf(AItem) = -1 then
  begin
    ViewList.Add(AItem);
    AItem.InvalidateCallback := FCallback;
    FCallback(Self, nil, TListItemCallbackOp.InvalidateOwner);
  end;
end;

procedure TListItemView.Exclude(const AItem: TListItemDrawable);
var
  Index: Integer;
begin
  Index := ViewList.IndexOf(AItem);

  if (Index <> -1) then
    ViewList.Delete(Index);
end;

function TListItemView.DrawableByName(const AName: string): TListItemDrawable;
begin
  Result := FindDrawable(AName);
  if Result = nil then
    raise EListError.Create(SGenericItemNotFound);
end;

function TListItemView.ObjectByName(const AName: string): TListItemDrawable;
begin
  Exit(DrawableByName(AName));
end;

function TListItemView.GetInitialized: Boolean;
begin
  Result := FInitialized;
end;

procedure TListItemView.SetInitialized(const Value: Boolean);
begin
  FInitialized := Value;
end;

function TListItemView.FindDrawable(const AName: string): TListItemDrawable;
var
  LObject: TListItemDrawable;
  I: Integer;
begin
  for I := 0 to ViewList.Count - 1 do
  begin
    LObject := ViewList[I];
    if LObject.Name = AName then
      Exit(LObject);
  end;
  Result := nil;
end;

function TListItemView.FindObject(const AName: string): TListItemDrawable;
begin
  Exit(FindDrawable(AName));
end;

{$ENDREGION}
{$REGION 'List Item Style Objects'}

{ TListItemStyleObjects }

constructor TListItemStyleResources.Create;
begin
  FOwnsObjects := True;
  DefaultTextFont := TFont.Create;
  DetailTextFont := TFont.Create;
  HeaderTextFont := TFont.Create;
  ButtonTextFont := TFont.Create;
  DeleteButtonTextFont := TFont.Create;
end;

constructor TListItemStyleResources.Create(const Source: TListItemStyleResources);
var
  I: TAccessoryType;
begin
  FOwnsObjects := False;
  for I := Low(TAccessoryType) to High(TAccessoryType) do
    AccessoryImages[I] := Source.AccessoryImages[I];
  HeaderTextFont := Source.HeaderTextFont;
  HeaderTextColor := Source.HeaderTextColor;
  HeaderTextShadowColor := Source.HeaderTextShadowColor;
  DefaultTextFont := Source.DefaultTextFont;
  DefaultTextColor := Source.DefaultTextColor;
  DetailTextFont := Source.DetailTextFont;
  DetailTextColor := Source.DetailTextColor;
  DefaultTextSelectedColor := Source.DefaultTextSelectedColor;
  ButtonAddItemStyleImage := Source.ButtonAddItemStyleImage;
  ButtonDeleteItemStyleImage := Source.ButtonDeleteItemStyleImage;
  ButtonNormalStyleImage := Source.ButtonNormalStyleImage;
  ButtonDeleteStyleImage := Source.ButtonDeleteStyleImage;
  ButtonCheckboxStyleImage := Source.ButtonCheckboxStyleImage;
  ButtonTextFont := Source.ButtonTextFont;
  ButtonTextColor := Source.ButtonTextColor;
  ButtonTextPressedColor := Source.ButtonTextPressedColor;
  DeleteButtonTextFont := Source.DeleteButtonTextFont;
  DeleteButtonTextColor := Source.DeleteButtonTextColor;
  DeleteButtonTextPressedColor := Source.DeleteButtonTextPressedColor;
  ScrollingStretchGlowColor := Source.ScrollingStretchGlowColor;
  PullRefreshIndicatorColor := Source.PullRefreshIndicatorColor;
  PullRefreshStrokeColor := Source.PullRefreshStrokeColor;
end;

destructor TListItemStyleResources.Destroy;
begin
  if FOwnsObjects then
  begin
    DefaultTextFont.Free;
    DetailTextFont.Free;
    HeaderTextFont.Free;
    ButtonTextFont.Free;
    DeleteButtonTextFont.Free;
  end;
  inherited;
end;
{$ENDREGION}

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TImageScalingMode), ['smStretchWithAspect', 'smOriginal', 'smStretch']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TImageScalingMode));
end;

{ TListItemPurposeHelper }

function TListItemPurposeHelper.ToString: string;
const
  PurposeString: array [TListItemPurpose] of string = ('None', 'Header', 'Footer');
begin
  Result := PurposeString[Self];
end;

initialization
  RegisterAliases;

finalization
  UnregisterAliases;
end.
