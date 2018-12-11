{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ListView;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.UIConsts, System.ImageList, FMX.Types, FMX.Controls, FMX.InertialMovement, FMX.TextLayout, FMX.ListView.Types,
  FMX.ListView.Adapters.Base, FMX.ListView.Appearances,  FMX.Styles, FMX.Objects, FMX.StdCtrls, System.Rtti,
  FMX.Graphics, FMX.Layouts, FMX.Styles.Objects, FMX.Edit, FMX.Platform, FMX.SearchBox, FMX.ActnList, FMX.ImgList,
  FMX.Presentation.Messages, FMX.Controls.Presentation, FMX.ListView.DynamicAppearance;

type
  TSwipeDirection = (ToLeft, ToRight); // ZuBy

  /// <summary>List view class that provides members that subclasses may use to
  /// communicate with a list view adapter that contains the actual items of the
  /// list view.</summary>
  TAdapterListView = class(TStyledControl)
  strict private
    FAdapter: IListViewAdapter;
    FHeightSumsNeedUpdate: Boolean;

    procedure ItemsMayChange(Sender: TObject);
    procedure ItemsCouldHaveChanged(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure ItemsResize(Sender: TObject);
    procedure ItemsInvalidate(Sender: TObject);
    procedure ResetView(Sender: TObject);
  protected
    /// <summary>Called after the adapter had been set, <see cref='SetAdapter'>SetAdapter</see></summary>
    procedure DoAdapterSet; virtual;
    /// <summary>Set <see cref='FMX.ListView.Types.IListViewAdapter'>IListViewAdapter</see></summary>
    procedure SetAdapter(Adapter: IListViewAdapter);
    /// <summary>Request update of item heights</summary>
    procedure InvalidateHeights;
    /// <summary>Handler of <see cref='IListViewAapter.OnChanged>IListViewAapter.OnChanged</see></summary>
    procedure DoItemsChange; virtual;
    /// <summary>Handler of
    /// <see cref='IListViewAapter.OnItemsMayChange'>IListViewAapter.OnItemsMayChange</see></summary>
    procedure DoItemsMayChange; virtual;
    /// <summary>Handler of
    /// <see cref='IListViewAdapter.OnItemsCouldHaveChanged'>IListViewAdapter.OnItemsCouldHaveChanged</see></summary>
    procedure DoItemsCouldHaveChanged; virtual;
    /// <summary>Handler of <see cref='IListViewAdapter.OnItemsResize'>IListViewAdapter.OnItemsResize</see></summary>
    procedure DoItemsResize; virtual;
    /// <summary>Handler of
    /// <see cref='IListViewAdapter.OnItemsInvalidate'>IListViewAdapter.OnItemsInvalidate</see></summary>
    procedure DoItemsInvalidate; virtual;
    /// <summary>Handler of
    /// <see cref='IListViewAdapter.OnResetView'>IListViewAdapter.OnResetView</see></summary>
    procedure DoResetView(const Sender: TListItem); virtual;
    /// <summary><c>True</c> if update of item height cache is required. Set by <c>InvalidateHeights</c></summary>
    property HeightSumsNeedUpdate: Boolean read FHeightSumsNeedUpdate write FHeightSumsNeedUpdate;
  public
    /// <summary><c>IListViewAdapter</c> providing contents of this <c>TAdapterListView</c></summary>
    property Adapter: IListViewAdapter read FAdapter write SetAdapter;
  end;

  /// <summary>The minimal ListView that's actually a real UI control. It implements all scrolling and drawing
  /// functionality. It needs a valid IListViewAdapter to provide item views;
  /// see <see cref='TAdapterListView.Adapter'>TAdapterListView.Adapter</see></summary>
  ///
  /// <remarks>
  /// Implements:
  ///  <para><c>ISearchResponder</c> enables setting a filter predicate</para>
  ///  <para><c>IListItemStyleResources</c> access to style resources</para>
  ///  <para><c>IListViewController</c> mediator protocol between <c>TListView</c> and <c>TListItem</c></para>
  ///  <para><c>IGlyph</c> interface to <c>TImageList</c></para>
  ///  <para><c>IMessageSendingCompatible</c> TMessageSender object for FMX.Presentation compatibility</para>
  /// </remarks>
  TListViewBase = class(TAdapterListView, ISearchResponder, IListItemStyleResources, IListViewController, IGlyph,
    IMessageSendingCompatible)
  private const
    ChangeRepaintedIncidentDelay = 0.1; // seconds
    PhysicsProcessingInterval = 8; // 8 ms for ~120 frames per second
    RecurrentTimerInterval = 16; // 16 ms for ~60 frames per second
    AutoTapScrollingSpeed = 8; // pixels per frame
    AutoTapMaxScrollingTime = 1; // seconds
    TapSelectWaitTime = 0.25; // seconds
    SelectionFadeInTime = 0.125; // seconds
    SelectionFadeOutTime = 0.25; // seconds
    MinScrollThreshold = 10;
    MinSwypeThreshold = 40;

    DefaultDeleteButtonWidth = 72;

    ItemSeparatorTop = 1;
    ItemSeparatorBottom = 2;

    EditModeSelectionAlpha = 0.25; // how bright the checked items are in editmode

    EditModeAnimationDuration = 0.1; // in seconds
    DeleteModeAnimationDuration = 0.15; // in seconds
    DefaultDeleteButtonText = 'Delete';

    PullRefreshIndicatorStrengthStart = 16;
    PullRefreshIndicatorMaxSteps = 12;

    DefaultLeftMargin = 10;
    DefaultRightMargin = 11;

  public type
    /// <summary>Edit mode change event</summary>
    /// <param name="Sender">event source (TListViewBase)</param>
    /// <param name="AHandled">set to <c>True</c> if event was handled</param>
    THandleChangeEvent = procedure(const Sender: TObject; var AHandled: Boolean) of object;
    /// <summary>Generic TListItem event; used for <c>OnListItemClick</c>, <c>OnItemChange</c></summary>
    /// <param name="Sender">event source (TListViewBase)</param>
    /// <param name="AItem">item being acted upon</param>
    TListItemEvent = procedure(const Sender: TObject; const AItem: TListItem) of object;
    /// <summary>OnItemClickEx event</summary>
    /// <param name="Sender">event source</param>
    /// <param name="ItemIndex">index of item</param>
    /// <param name="LocalClickPos">click position in item local coordinates</param>
    /// <param name="ItemObject"><c>TListItemDrawable</c> that received the click</param>
    TListItemClickEventEx = procedure(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF;
      const ItemObject: TListItemDrawable) of object;
    /// <summary><c>OnUpdateItemView</c> event</summary>
    TUpdateItemViewEvent = TListItemEvent;
    /// <summary><c>OnUpdatingItemView</c> event</summary>
    TUpdatingItemViewEvent = procedure(const Sender: TObject; const AItem: TListItem; var AHandled: Boolean) of object;
    /// <summary><c>OnDeletingItem</c> event</summary>
    TDeletingItemEvent = procedure(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean) of object;
    /// <summary><c>OnDeleteItem</c> event</summary>
    TDeleteItemEvent = procedure(Sender: TObject; AIndex: Integer) of object;
    /// <summary><c>OnDeleteChangeVisible</c> event</summary>
    TDeleteChangeVisibilityEvent = procedure(Sender: TObject; AValue: Boolean) of object;

    TColumnClick = procedure(const Sender: TObject; const Column: Integer; 
      const X, Y: Single; const AItem: TListViewItem; const DrawebleName: string) of object; // ZuBy
    TScrollEnd = procedure(Sender: TObject) of object; // ZuBy
    TSwipeDirectionEvent = procedure(Sender: TObject; const Direction: TSwipeDirection) of object; // ZuBy

  private type
    TItemHeightSums = TList<Integer>;

    TDelayedIncident = (ChangeRepainted, Invalidate, SetItemIndex, ClickEvent);

    TDelayedIncidentEntry = record
      Incident: TDelayedIncident;
      Triggered: Boolean;
      StartTime: Double;
      TimeToWait: Double;
      CustomData: NativeInt;
    end;

    TDelayedIncidents = TList<TDelayedIncidentEntry>;

    TTransitionType = (None, EditMode, DeleteMode);

    TInternalDragMode = (None, Drag, Swype);

    TItemSelectionAlpha = record
      StartTime: Double;
      Alpha: Single;
      StartAlpha: Single;

      class function Create(const StartTime: Double; const Alpha, StartAlpha: Single): TItemSelectionAlpha; static; inline;
    end;

    TItemSelectionAlphas = TDictionary<Integer, TItemSelectionAlpha>;
    TPullRefreshAnimation = (NotPlaying, Playing, Finished);
    TStateFlag = (NeedsRebuild, NeedsScrollingLimitsUpdate, Invalid, Painting, ResettingObjects, ScrollingActive,
      ScrollingMouseTouch, NeedsScrollBarDisplay);
    TStateFlags = set of TStateFlag;

    TEstimatedHeights = record
      Item: Single;
      Header: Single;
      Footer: Single;
    end;

  private
    FTimerService: IFMXTimerService;
    FSystemInformationService: IFMXSystemInformationService;
    FListingService: IFMXListingService;
    FStateFlags: TStateFlags;
    FRecurrentTimerHandle: TFmxHandle;
    FDelayedIncidents: TDelayedIncidents;
    FSelectionAlphas: TItemSelectionAlphas;
    FItemIndex: Integer;
    FAniCalc: TAniCalculations;
    FScrollViewPos: Single;
    FBrush: TBrush;
    FStroke: TStrokeBrush;
    FMouseDownAt: TPointF;
    FMouseClickPrev: TPointF;
    FMouseClickDelta: TPointF;
    FMouseClicked: Boolean;
    FMouseClickIndex: Integer;
    FMouseEventIndex: Integer;
    FMouseSetItemIndex: Integer;
    FItemSpaces: TBounds;
    FMousePrevScrollPos: Single;
    FClickEventItemIndex: Integer;
    FClickEventMousePos: TPointF;
    [Weak] FClickEventControl: TListItemDrawable;
    FHeightSums: TItemHeightSums;
    FMaxKnownHeight: Integer;
    FSideSpace: Integer;
    FScrollScale: Single;
    FBackgroundStyleColor: TAlphaColor;
    FSelectionStyleColor: TAlphaColor;
    FItemStyleFillColor: TAlphaColor;
    FItemStyleFillAltColor: TAlphaColor;
    FItemStyleFrameColor: TAlphaColor;
    [Weak] FSelectionStyleImage: TStyleObject;
    [Weak] FHeaderStyleImage: TStyleObject;
    FTouchAnimationObject: ITouchAnimationObject;
    FScrollBar: TScrollBar;
    FTransparent: Boolean;
    FAllowSelection: Boolean;
    FAlternatingColors: Boolean;
    FTapSelectItemIndex: Integer;
    FTapSelectNewIndexApplied: Integer;
    FTapSelectStartTime: Double;
    FShowSelection: Boolean;
    FOnChange: TNotifyEvent;
    FOnChangeRepainted: TNotifyEvent;
    FOnItemsChange: TNotifyEvent;
    FOnScrollViewChange: TNotifyEvent;
    FOnSearchChange: TNotifyEvent;
    FOnFilter: TFilterEvent;
    FAutoTapScroll: Boolean;
    FAutoTapTreshold: Integer;
    FAutoTapDistance: Integer;
    FOnListItemClick: TListItemEvent;
    FOnItemClickEx: TListItemClickEventEx;
    FOnItemChange: TListItemEvent;
    FOnEditModeChanging: THandleChangeEvent;
    FOnEditModeChange: TNotifyEvent;
    FOnUpdateItemView: TUpdateItemViewEvent;
    FOnUpdatingItemView: TUpdatingItemViewEvent;
    FOnDeleteChange: TDeleteChangeVisibilityEvent;
    FOnDeletingItem: TDeletingItemEvent;
    FOnDeleteItem: TDeleteItemEvent;
    FOnPullRefresh: TNotifyEvent;
    FDeleteButtonText: string;
    FEditMode: Boolean;
    FCanSwipeDelete: Boolean;
    FDeleteButtonIndex: Integer;
    FPrevDeleteButtonIndex: Integer;
    FStyleResources: TListItemStyleResources;
    FUpdatingStyleResources: Boolean;
    FDisableMouseWheel: Boolean;
    FTransitionStartTime: Double;
    FTransitionType: TTransitionType;
    FEditModeTransitionAlpha: Single;
    FDeleteModeTransitionAlpha: Single;
    FDeleteLayout: TLayout;
    FDeleteButton: TSpeedButton;
    FDragListMode: TInternalDragMode;
    FSearchEdit: TSearchBox;
    FSearchVisible: Boolean;
    FSearchAlwaysOnTop: Boolean;
    FSelectionCrossfade: Boolean;
    FPullToRefresh: Boolean;
    FPullRefreshWait: Boolean;
    FPullRefreshTriggered: Boolean;
    FPullRefreshAnimation: TPullRefreshAnimation;
    FPullRefreshAnimationStartTime: Double;
    FPullRefreshAnimationStopTime: Double;
    FScrollStretchStrength: Single;
    FControlType: TControlType;
    FNativeOptions: TListViewNativeOptions;
    FImageLink: TGlyphImageLink;
    FMessageSender: TMessageSender;
    FItemSelectedBeforeChange: TListItem;
    FEstimatedHeights: TEstimatedHeights;

    FMouseClickSwipeEventSend: Boolean; // ZuBy
    FHeaderStyleColor: TAlphaColor;
    FOnColumnClick: TColumnClick; // ZuBy
    FOnScrollEnd: TScrollEnd; // ZuBy
    FOnSwipe: TSwipeDirectionEvent; // ZuBy
    FTransparentSeparator: Boolean; // ZuBy
    FTransparentItems: Boolean; // ZuBy
    FTransparentHeaders: Boolean; // ZuBy
    FAutoPositionToItem: Boolean; // ZuBy
    FTopItemIndex: Integer; // ZuBy
    FCanSwipeDirection: Boolean; // ZuBy
    FSeparatorLeftOffset: Single; // ZuBy
    FSeparatorRightOffset: Single; // ZuBy
    FHorizontal: Boolean; // ZuBy
    FItemBoxLight: TStyleObject; // ZuBy
    FMakeSelectedItemVisible: Boolean; // ZuBy
    FShowScrollBar: Boolean; // ZuBy
    FColumnWidth: Single; // ZuBy
    FTopOffset: Integer; // ZuBy
    FBottomOffset: Integer; // ZuBy
    FItemBottomOffset: Integer; // ZuBy
    FColumns: Integer; // ZuBy
    FMarg: Integer; // ZuBy
    FAutoColumns: Boolean; // ZuBy
    FCanScroll: Boolean; // ZuBy
    FShowFirstSeparator: Boolean; // sinuke
    FShowLastSeparator: Boolean; // sinuke

    function IsRunningOnDesktop: Boolean;
    function HasTouchTracking: Boolean;
    function HasSearchFeatures: Boolean;
    function HasSearchAsItem: Boolean;
    function IsDeleteModeAllowed: Boolean;
    function HasStretchyScrolling: Boolean;
    function HasPhysicsStretchyScrolling: Boolean;
    function HasScrollingStretchGlow: Boolean;
    function HasPullRefreshStroke: Boolean;

    function CanDisplaySelectionForItem(const Index: Integer; const Item: TListItem = nil;
      const IncludeMultiSelect: Boolean = False; const IncludeCrossFaded: Boolean = False): Boolean;
    function GetDefaultSelectionAlpha: Single;
    function GetItemSelectionAlpha(const Index: Integer): Single;
    procedure DestroyRecurrentTimer;
    procedure UpdateRecurrentTimer;
    function HasRecurrentTimerEvents: Boolean;
    procedure RecurrentTimerEvent;
    procedure StartIncident(const Incident: TDelayedIncident; const Triggered: Boolean = True;
      const TimeToWait: Single = 0; const CustomData: NativeInt = 0);
    procedure ProcessIncident(const Entry: TDelayedIncidentEntry);
    procedure TriggerIncidents(const Incident: TDelayedIncident; const ResetStartupTime: Boolean = True);
    procedure ProcessDelayedIncidents;
    procedure ProcessTransitionAnimation;
    procedure ProcessTapSelectItem;
    procedure ProcessSelectionAlphas;
    procedure InsertItemCrossFade(const Index: Integer; const ShowAnimation: Boolean);
    procedure RemoveItemCrossFade(const Index: Integer);
    procedure StartPullRefreshAnimation;
    procedure ProcessPullRefreshAnimation;
    function GetPullRefreshStrength: Single;
    function GetPullRefreshIndicatorSteps: Integer;
    function GetPullRefreshIndicatorAlpha: Single;
    function GetPullRefreshStrokeWidth: Single;
    procedure PaintPullRefreshIndicator(const ACanvas: TCanvas; const AStrength, AOpacity: Single);
    procedure PaintPullRefreshStroke(const ACanvas: TCanvas; const AStrength, AOpacity: Single);
    procedure PaintScrollingStretchGlow(const ACanvas: TCanvas; const AIntensity, AOpacity: Single);
    procedure UpdatePullRefreshState;
    procedure UpdateScrollStretchStrength(const NewValue: Single);
    procedure DeleteButtonClicked(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure ItemSpacesChange(Sender: TObject);
    procedure AniCalcChange(Sender: TObject);
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemIndexInternal(const Value: Integer; const DisableSelection: Boolean = False;
      const DisableCrossfade: Boolean = False);
    function GetMaxScrollViewPos: Integer;
    procedure UpdateScrollViewPos(const Value: Single);
    procedure UpdateSearchEditPos;
    procedure SetScrollViewPos(const Value: Single);
    procedure UpdateScrollingLimits;
    procedure UpdateScrollBar;
    procedure GetNumberOfRenderingPasses(const StartItem, EndItem: Integer; var Passes, Subpasses: Integer);
    function GetItemHeight(const Index: Integer): Integer; overload; virtual;
    function GetItemRelRect(const Index: Integer; const LocRect: TRectF; const SideSpace: Integer = 0): TRectF; inline;
    function GetItemGroupSeparators(const Index: Integer): Integer; inline;
    function FindLocalItemObjectAtPosition(const ItemIndex: Integer; const Position: TPointF): TListItemDrawable;

    function GetSeparatorLineHeight: Single;
    function AlignValueToPixel(const Value: Single): Single;
    procedure DrawItemsFill(const StartItem, EndItem: Integer; const LocRect: TRectF; const Opacity: Single;
      const HeaderIndex: Integer = -1);
    procedure DrawIndexFill(const AIndex: Integer; const LocRect: TRectF; const Opacity: Single);
    procedure DrawTouchAnimation(const Index: Integer; const LocRect: TRectF; const Opacity: Single);

    function GetHeaderRelRect(const StartItem, HeaderIndex: Integer; const LocRect: TRectF;
      const SideSpace: Integer = 0): TRectF;
    procedure DrawHeaderItem(const LocRect: TRectF; const StartItem, HeaderIndex: Integer; const Opacity: Single);

    procedure DrawListItems(const AbsOpacity: Single);

    procedure UpdateItemLookups;
    function FindItemAbsoluteAt(const ViewAt: Integer): Integer;
    function FindItemAbsoluteAtWithCheck(const ViewAt: Integer): Integer;
    procedure SetSideSpace(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetAlternatingColors(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure RecreateNativePresentation; virtual;

    procedure SetEditMode(const Value: Boolean);
    procedure SetCanSwipeDelete(const Value: Boolean);

    procedure SelectItem(const ItemIndex: Integer); virtual;
    procedure UnselectItem(const ItemIndex: Integer); virtual;
    function GetSelected: TListItem;
    procedure SetSelected(const Value: TListItem);
    procedure SetNewItemIndex(const NewIndex: Integer);

    procedure SetSearchVisible(const Value: Boolean);
    procedure SetSearchAlwaysOnTop(const Value: Boolean);
    procedure SetOnFilter(const Value: TFilterEvent);
    procedure OnSearchEditResize(Sender: TObject);
    procedure OnSearchEditChange(Sender: TObject);
    function DeleteButtonTextStored: Boolean;
    // ISearchResponder
    procedure SetFilterPredicate(const Predicate: TPredicate<string>);
    // IMessageSendingCompatible
    function GetMessageSender: TMessageSender;
    // Custom readers
    procedure ReadCanSwipeDelete(Reader: TReader);
    procedure ReadIsSearchVisible(Reader: TReader);
    procedure ReadIsSearchAlwaysOnTop(Reader: TReader);
    procedure ReadEditModeOptions(Reader: TReader);

    function GetItemCount: Integer;

    { IListViewController }
    procedure RequestReindexing(const Item: TListItem);
    procedure ItemResized(const Item: TListItem);
    procedure ItemInvalidated(const Item: TListItem);
    procedure ControlClicked(const Item: TListItem; const Control: TListItemDrawable);
    procedure CheckStateChanged(const Item: TListItem; const Control: TListItemDrawable);
    function GetScene: IScene;

    procedure RecalcTopViewItemIndex; // ZuBy
    procedure SetSeparatorLeftOffset(const Value: Single); // ZuBy
    procedure SetSeparatorRightOffset(const Value: Single); // ZuBy
    procedure SetHorizontal(const Value: Boolean); // ZuBy
    procedure SetShowScrollBar(const Value: Boolean); // ZuBy
    procedure SetAutoColumns(const Value: Boolean); // ZuBy
    procedure SetBottomOffset(const Value: Integer); // ZuBy
    procedure SetColumnWidth(const Value: Single); // ZuBy
    procedure SetTopOffset(const Value: Integer);
    procedure SetItemBottomOffset(const Value: Integer);
    procedure SetTransparentSeparator(const Value: Boolean);
    procedure SetTransparentItems(const Value: Boolean); // ZuBy
    function getTextSize(const aText: string; aFont: TFont; const aWordWrap: Boolean; aWidth, aHeight: Single): TSizeF; // ZuBy
    procedure SetTransparentHeader(const Value: Boolean); // ZuBy
    procedure SetCanScroll(const Value: Boolean);
    procedure SetShowFirstSeparator(const Value: Boolean);
    procedure SetShowLastSeparator(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    /// <summary><c>True</c> if in Edit Mode</summary>
    function IsEditMode: Boolean; virtual;
    /// <summary>Used internally by presentation hook</summary>
    procedure DoSetItemIndexInternal(const Value: Integer); virtual;
    /// <summary>Used internally by presentation hook</summary>
    procedure DoUpdateScrollViewPos(const Value: Single); virtual;
    /// <summary>Used internally by presentation hook</summary>
    procedure DoSetScrollViewPos(const Value: Single); virtual;
    /// <summary>Invoked when Edit Mode is being changed; if Edit Mode requires a different appearance, this
    /// is where update of appearances should be initiated</summary>
    procedure WillEnterEditMode(const Animated: Boolean); virtual;
    /// <summary>Used internally by presentation hook</summary>
    function HasButtonsInCells: Boolean; virtual;
    /// <summary>Used internally by presentation hook</summary>
    function HasDeletionEditMode: Boolean; virtual;
    /// <summary>Used internally by presentation hook</summary>
    function HasCheckboxMode: Boolean; virtual;

    /// <summary>Stop edit mode transition animation</summary>
    procedure ResetEditModeAnimation;
    /// <summary>Initialize edit mode transition animation</summary>
    procedure InitEditModeAnimation;
    /// <summary>Stop delete mode transition animation</summary>
    procedure ResetDeleteModeAnimation;
    /// <summary>Initialize delete mode transition animation</summary>
    procedure InitDeleteModeAnimation;
    /// <summary>Update layout to place a Delete button</summary>
    procedure UpdateDeleteButtonLayout;
    /// <summary>Perform item deletion</summary>
    procedure ProceedDeleteItem;

    /// <summary>Invokes Pull-to-Refresh when applicable</summary>
    procedure ScrollStretchChanged; virtual;
    /// <summary>Scroll stretch threshold value when Pull-to-Refresh is invoked</summary>
    property ScrollStretchStrength: Single read FScrollStretchStrength;

    procedure SetSelectionCrossfade(const Value: Boolean);
    function GetDeleteButtonText: string;
    procedure SetDeleteButtonText(const Value: string);
    procedure SetPullToRefresh(const Value: Boolean);
    procedure SetControlType(const Value: TControlType);
    procedure SetNativeOptions(const Value: TListViewNativeOptions);

    { IListViewController }
    function GetEditModeTransitionAlpha: Single;
    function GetDeleteModeTransitionAlpha: Single;
    procedure SetDeleteButtonIndex(const NewItemIndex: Integer);
    function GetItemEditOffset(const Item: TListItem): Single;
    function GetItemDeleteCutoff(const Item: TListItem): Single;
    function GetClientMargins: TRectF;
    function GetItemCurrentSelectionAlpha(const Item: TListItem): Single;
    function IListViewController.GetItemSelectionAlpha = GetItemCurrentSelectionAlpha;
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);

    /// <summary>Hook for <c>IListViewController.RequestReindexing</c></summary>
    procedure DoRequestReindexing(const Item: TListItem); virtual;
    /// <summary>Hook for <c>IListViewController.ItemResized</c></summary>
    procedure DoItemResized(const Item: TListItem); virtual;
    /// <summary>Hook for <c>IListViewController.ItemInvalidated</c></summary>
    procedure DoItemInvalidated(const Item: TListItem); virtual;
    /// <summary>Hook for <c>IListViewController.CheckStateChanged</c></summary>
    procedure DoCheckStateChanged(const Item: TListItem; const Control: TListItemDrawable); virtual;
    /// <summary>Hook for <c>IListViewController.ControlClicked</c></summary>
    procedure DoControlClicked(const Item: TListItem; const Control: TListItemDrawable); virtual;

    { IGlyph }
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;   
    { IListItemStyleResources }
    function GetStyleResources: TListItemStyleResources;
    function StyleResourcesNeedUpdate: Boolean;

    procedure SetItemSpaces(const Value: TBounds);
    function GetItemClientRect(const Index: Integer): TRectF; // part of IListViewPresentationParent
    function GetEstimatedItemHeight: Single; // part of IListViewPresentationParent
    function GetEstimatedHeaderHeight: Single; // part of IListViewPresentationParent
    function GetEstimatedFooterHeight: Single; // part of IListViewPresentationParent

    /// <summary>Called when an instance or reference to instance of <b>TBaseImageList</b> or the
    /// <b>ImageIndex</b> property is changed.
    /// <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    procedure ImagesChanged; virtual;
    procedure Paint; override;
    procedure AfterPaint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar;  Shift: TShiftState); override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    function GetDefaultStyleLookupName: string; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Invalidate;
    procedure DoRealign; override;
    procedure DoExit; override;
    /// <summary>General state change. Starts <c>TDelayedIncident.ChangeRepainted</c> incident and
    /// invokes <c>OnChange</c></summary>
    procedure DoChange; virtual;
    /// <summary>Handle <c>TDelayedIncident.ChangeRepainted</c> incident</summary>
    procedure DoChangeRepainted; virtual;
    /// <summary>Invoke <c>OnItemChange</c> handler</summary>
    procedure DoListItemChange(const AListItem: TListItem); virtual;
    /// <summary>Invoke <c>OnListItemClick</c> handler</summary>
    procedure DoListItemClick(const AListItem: TListItem); virtual;
    /// <summary>Invoke <c>OnEditModeChange</c> handler</summary>
    procedure DoEditModeChange; virtual;
    /// <summary>Invoke <c>OnEditModeChanging</c> handler</summary>
    procedure DoEditModeChanging(var AHandled: Boolean); virtual;
    /// <summary>Reset edit mode animation</summary>
    procedure DoResetEditModeAnimation; virtual;
    /// <summary>Update scrolling limits and animation boundaries</summary>
    procedure DoUpdateScrollingLimits; virtual;

    // Notifications from IListViewAdapter
    procedure DoItemsMayChange; override;
    procedure DoItemsCouldHaveChanged; override;
    procedure DoItemsInvalidate; override;
    /// <summary>This virtual method is called immediately after list of items has been changed. </summary>
    procedure DoItemsChange; override;
    procedure DoAdapterSet; override;
    /// <summary>Deletes an item</summary>
    /// <param name="ItemIndex"> index of item to be deleted</param>
    /// <returns><c>True</c> if deleted succesfully</returns>
    function DeleteItem(const ItemIndex: Integer): Boolean;
    /// <summary>Perform actual item deletion. Called from DeleteItem: Boolean</summary>
    procedure DoDeleteItem(const ItemIndex: Integer); virtual;
    /// <summary>Get area available to item layout</summary>
    function GetFinalItemSpaces(const ForceIncludeScrollBar: Boolean = True): TRectF; virtual;
    /// <summary>Get item size</summary>
    function GetFinalItemSize(const ForceIncludeScrollBar: Boolean = True): TSizeF; virtual;
    function CanObserve(const ID: Integer): Boolean; override;
    /// <summary>Notify observers about selection change</summary>
    procedure ObserversBeforeSelection(out LAllowSelection: Boolean);
    /// <summary>Return <c>True</c> if this item should handle input events</summary>
    function ShouldHandleEvents: Boolean; virtual;
    /// <summary>Invoke <c>OnUpdatingItemView</c> handler</summary>
    procedure DoUpdatingItemView(const AListItem: TListItem; var AHandled: Boolean); virtual;
    // <summary>Invoke <c>OnUpdateItemView</c> handler</summary>
    procedure DoUpdateItemView(const AListItem: TListItem); virtual;
    /// <summary>Get glyph button for item <c>Index</c></summary>
    function GetGlyphButton(const Index: Integer): TListItemGlyphButton;
    /// <summary>Invoked before item view will be updated (before calling ResetObjects)</summary>
    property OnUpdatingItemView: TUpdatingItemViewEvent read FOnUpdatingItemView write FOnUpdatingItemView;
    /// <summary>Invoked after item view has been updated (after calling ResetObjects)</summary>
    property OnUpdateItemView: TUpdateItemViewEvent read FOnUpdateItemView write FOnUpdateItemView;
    /// <summary>Invoked after EditMode has been changed</summary>
    property OnEditModeChange: TNotifyEvent read FOnEditModeChange write FOnEditModeChange;
    /// <summary>Invoked before changing EditMode</summary>
    property OnEditModeChanging: THandleChangeEvent read FOnEditModeChanging write FOnEditModeChanging;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDefaultMargins: TRectF;
    procedure EndUpdate; override;
    /// <summary>When using native presentation, re-creates the list and updates visible item content</summary>
    procedure RebuildList; virtual;
    /// <summary>Scrolls the view (instantly) to the desired item placing it within the view</summary>
    procedure ScrollTo(const AItemIndex: Integer);
    /// <summary>Index of selected item</summary>
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    /// <summary>Selected item</summary>
    property Selected: TListItem read GetSelected write SetSelected;
    /// <summary>Scroll position in dimension units</summary>
    property ScrollViewPos: Single read FScrollViewPos write SetScrollViewPos;
    /// <summary>Get rectangle of item, relative control</summary>
    function GetItemRect(const AItemIndex: Integer): TRectF;
    /// <summary>This method should be called when "pull to refresh" mode has been triggered to stop spinning wheel
    /// </summary>
    /// <remarks>This has only effect in native iOS control and only when PullRefreshWait property is set to True
    /// </remarks>
    procedure StopPullRefresh; virtual;
    /// <summary>Space in logical units around the content of each list item</summary>
    property ItemSpaces: TBounds read FItemSpaces write SetItemSpaces;
    /// <summary>The list of images. Can be <c>nil</c>. <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    property Images: TCustomImageList read GetImages write SetImages;
    /// <summary>Space in logical units on all sides around the list box encompassing the items</summary>
    property SideSpace: Integer read FSideSpace write SetSideSpace default 0;
    /// <summary>If the control is transparent, it will not draw its background</summary>
    property Transparent: Boolean read FTransparent write SetTransparent;
    /// <summary>Determines whether the items are selectable or not. If items are not selectable, user will still be
    /// able to click on embedded controls</summary>
    property AllowSelection: Boolean read FAllowSelection write FAllowSelection default True;
    // <summary>Enabling this will switch fill colors for odd and even elements</summary>
    property AlternatingColors: Boolean read FAlternatingColors write SetAlternatingColors default False;
    /// <summary>Determines whether the selection is visible when selecting items.
    /// It may be disabled when using list of checkboxes</summary>
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    /// <summary>Enables swipe delete</summary>
    property CanSwipeDelete: Boolean read FCanSwipeDelete write SetCanSwipeDelete default True;
    /// <summary>Enable automatic scrolling to the top when tapped at the top edge</summary>
    property AutoTapScroll: Boolean read FAutoTapScroll write FAutoTapScroll default False;
    /// <summary>Threshold distance from the top edge at which the tap would initiate autoscroll to top</summary>
    property AutoTapTreshold: Integer read FAutoTapTreshold write FAutoTapTreshold default 8;
    /// <summary>Disables mouse wheel</summary>
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    /// <summary>Item count</summary>
    property ItemCount: Integer read GetItemCount;
    /// <summary>Item click handler</summary>
    property OnListItemClick: TListItemEvent read FOnListItemClick write FOnListItemClick;
    /// <summary>Extended item click handler which gets click coordinates and clicked drawable</summary>
    property OnItemClickEx: TListItemClickEventEx read FOnItemClickEx write FOnItemClickEx;
    /// <summary>Reserved</summary>
    property OnItemChange: TListItemEvent read FOnItemChange write FOnItemChange;
    /// <summary>General OnChange event handler</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary><c>TDelayedIncident.ChangeRepainted</c> delayed incident handler</summary>
    property OnChangeRepainted: TNotifyEvent read FOnChangeRepainted write FOnChangeRepainted;
    /// <summary>This event occurs after list of items has been changed. </summary>
    property OnItemsChange: TNotifyEvent read FOnItemsChange write FOnItemsChange;
    /// <summary>Called when ScrollViewPos has changed (manually or in code)</summary>
    property OnScrollViewChange: TNotifyEvent read FOnScrollViewChange write FOnScrollViewChange;
    /// <summary>Query before item deletion, see <see cref='TDeletingItemEvent'>TDeletingItemEvent</see>.
    ///  Application can veto deletion by returning <c>False</c> in handler Result</summary>
    ///  <remarks>Deletion is performed by <see cref='DoDeleteItem'>DoDeleteItem</see></remarks>
    property OnDeletingItem: TDeletingItemEvent read FOnDeletingItem write FOnDeletingItem;
    /// <summary>Invoked after item has been deleted</summary>
    property OnDeleteItem: TDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
    /// <summary>Invoked when Delete button changes visibility</summary>
    property OnDeleteChangeVisible: TDeleteChangeVisibilityEvent read FOnDeleteChange write FOnDeleteChange;
    /// <summary></summary>
    property OnSearchChange: TNotifyEvent read FOnSearchChange write FOnSearchChange;
    /// <summary>Event handler for setting custom filter on text of <c>TListView</c>.</summary>
    property OnFilter: TFilterEvent read FOnFilter write SetOnFilter;
    /// <summary>Invoked when pull refresh is triggered</summary>
    property OnPullRefresh: TNotifyEvent read FOnPullRefresh write FOnPullRefresh;
    /// <summary>Text to display in Delete button</summary>
    property DeleteButtonText: string read GetDeleteButtonText write SetDeleteButtonText stored
      DeleteButtonTextStored nodefault;
    /// <summary>Enable/disable Edit Mode</summary>
    property EditMode: Boolean read FEditMode write SetEditMode default False;
    /// <summary><c>True</c> if search bar is visible</summary>
    property SearchVisible: Boolean read FSearchVisible write SetSearchVisible default False;
    /// <summary>Always display search bar</summary>
    property SearchAlwaysOnTop: Boolean read FSearchAlwaysOnTop write SetSearchAlwaysOnTop default True;
    /// <summary>Enable selection crossfade animation</summary>
    property SelectionCrossfade: Boolean read FSelectionCrossfade write SetSelectionCrossfade default False;
    /// <summary>Enable pull to refresh</summary>
    property PullToRefresh: Boolean read FPullToRefresh write SetPullToRefresh default False;
    /// <summary>When set to True, the spinning wheel does not disappear automatically and StopPullRefresh method needs
    /// to be called after refresh operation is done. If this is set to False (default), then spinning wheel disappears
    /// automatically shortly after triggering the effect. This option works only in native iOS control and has no
    /// effect otherwise.</summary>
    property PullRefreshWait: Boolean read FPullRefreshWait write FPullRefreshWait default False;
    /// <summary>Control type: Styled or Native</summary>
    property ControlType: TControlType read FControlType write SetControlType default TControlType.Styled;
    /// <summary>Options for Native control; see <see cref='ControlType'>ControlType</see></summary>
    property NativeOptions: TListViewNativeOptions read FNativeOptions write SetNativeOptions default [];

    procedure RebuildOrientation; // ZuBy
    procedure EnableTouchAnimation(Value: Boolean); // ZuBy
    // Scroll Width
    function GetScrollWidth: Single; // ZuBy
    function FindItemByPosition(X, Y: Single): Integer; // ZuBy
    // Get Item Height
    function getHeightByIndex(Index: Integer): Integer; // ZuBy
    function getItemTextHeight(const AItem: TListItemText; const aWidth: Single = 0): Integer; // ZuBy
    function getItemTextButtonHeight(const AItem: TListItemTextButton; const aWidth: Single = 0): Integer; // ZuBy
    function getItemTextWidth(const AItem: TListItemText; const aHeight: Single = 0): Integer; // ZuBy
    function getItemTextButtonWidth(const AItem: TListItemTextButton; const aHeight: Single = 0): Integer; // ZuBy
    // Ani Calculaction
    function getAniCalc: TAniCalculations; // ZuBy
    procedure SearchBoxClear; // ZuBy
    property ShowFirstSeparator: Boolean read FShowFirstSeparator write SetShowFirstSeparator default True;
    property ShowLastSeparator: Boolean read FShowLastSeparator write SetShowLastSeparator default True;
    property TransparentSeparators: Boolean read FTransparentSeparator write SetTransparentSeparator default False; // ZuBy
    property TransparentItems: Boolean read FTransparentItems write SetTransparentItems default False; // ZuBy
    property TransparentHeaders: Boolean read FTransparentHeaders write SetTransparentHeader default False; // ZuBy
    property AutoPositionToItem: Boolean read FAutoPositionToItem write FAutoPositionToItem default False; // ZuBy
    // Separator Draw
    property SeparatorLeftOffset: Single read FSeparatorLeftOffset write SetSeparatorLeftOffset; // ZuBy
    property SeparatorRightOffset: Single read FSeparatorRightOffset write SetSeparatorRightOffset; // ZuBy
    property ItemBottomOffset: Integer read FItemBottomOffset write SetItemBottomOffset; // ZuBy
    property CanSwipeDirection: Boolean read FCanSwipeDirection write FCanSwipeDirection default False; // ZuBy
    property Horizontal: Boolean read FHorizontal write SetHorizontal default False; // ZuBy
    property MakeSelectedItemVisible: Boolean read FMakeSelectedItemVisible write FMakeSelectedItemVisible default True;
    property OffsetTop: Integer read FTopOffset write SetTopOffset; // ZuBy
    property OffsetBottom: Integer read FBottomOffset write SetBottomOffset;
    // ZuBy
    property AutoColumns: Boolean read FAutoColumns write SetAutoColumns;
    // ZuBy
    property CanScroll: Boolean read FCanScroll write SetCanScroll default True;
    // sinuke
    property Columns: Integer read FColumns; // ZuBy
    property ColumnOffset: Integer read FMarg; // ZuBy
    property ColumnWidth: Single read FColumnWidth write SetColumnWidth; // ZuBy
    property ShowScrollBar: Boolean read FShowScrollBar write SetShowScrollBar default True; // ZuBy
    property OnColumnClick: TColumnClick read FOnColumnClick write FOnColumnClick; // ZuBy
    property OnScrollEnd: TScrollEnd read FOnScrollEnd write FOnScrollEnd;
    // ZuBy
    property OnSwipeDirection: TSwipeDirectionEvent read FOnSwipe write FOnSwipe; // ZuBy
  end;

  /// <summary>TListView that supports native presentation</summary>
  TPresentedListView = class(TListViewBase, IListViewPresentationParent, IListViewDesignPresentationParent)
  strict private
    FPresentation: IListViewPresentation;
    FPresentationLocked: Integer;
    FCreatingNativeView: Boolean;
  protected
    /// <summary>Lock presentation and execute <c>P</c></summary>
    procedure ExecuteInterlocked(const P: TProc);
    /// <summary><c>True</c> if item can be selected</summary>
    function CanSelectItem(const AItemIndex: Integer): Boolean;
    /// <summary><c>True</c> if item can be unselected</summary>
    function CanUnselectItem(const AItemIndex: Integer): Boolean;
    /// <summary>Called after item has been selected</summary>
    procedure DidSelectItem(const AItemIndex: Integer);
    /// <summary>Called after item has been unselected</summary>
    procedure DidUnselectItem(const AItemIndex: Integer);
    procedure ChangeOrder; override;
    procedure ParentChanged; override;
    procedure PaintChildren; override;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure DoSetItemIndexInternal(const Value: Integer); override;
    procedure DoEditModeChange; override;
    procedure DoItemsChange; override;
    procedure DoItemsInvalidate; override;
    procedure DoItemInvalidated(const Item: TListItem); override;
    procedure DoCheckStateChanged(const AItem: TListItem; const Control: TListItemDrawable); override;
    procedure DoUpdateScrollViewPos(const Value: Single); override;
    procedure DoSetScrollViewPos(const Value: Single); override;
    procedure DoDeleteItem(const ItemIndex: Integer); override;
    procedure DoResetEditModeAnimation; override;
    procedure DoUpdateScrollingLimits; override;
    procedure DoAbsoluteChanged; override;
    // Presentation
    /// <summary>Parent control has been loaded</summary>
    procedure PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    procedure RecreateNativePresentation; override;
    function ShouldHandleEvents: Boolean; override;
    // IPresentationParent
    function GetRootObject: TObject;
    function GetContentFrame: TRect;
    function GetControlOpacity: Single;
    // IListViewPresentationParent
    function GetAdapter: IListViewAdapter;
    function GetItemText(const ItemIndex: Integer): string;
    function GetItemIndexTitle(const ItemIndex: Integer): string;
    procedure ItemButtonClicked(const ItemIndex: Integer);
    procedure InvokePullRefresh;
    procedure SetSearchFilter(const Filter: string);
    function GetTableViewFlags: TListViewModeFlags;
    function GetTableViewOptions: TListViewNativeOptions;
    function IListViewPresentationParent.GetFlags = GetTableViewFlags;
    function IListViewPresentationParent.GetOptions = GetTableViewOptions;
    procedure SetCreatingNativeView(const Value: Boolean);
    function GetIsTransparent: Boolean;
    function GetOpacity: Single;
    function GetBackgroundStyleColor: TAlphaColor;
    procedure DoItemsResize; override;
    // IListViewDesignPresentationParent
    function HasDesignPresentationAttached: Boolean;
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure RecalcEnabled; override;
    procedure Show; override;
    procedure Hide; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure RebuildList; override;
    procedure StopPullRefresh; override;
    procedure RecalcOpacity; override;
  end;

  /// <summary>TAppearanceListView supports Appearances. Appearances are templates used for dynamic creation
  ///  of item views. Normally all items of the same purpose in the List View share the same appearance, differing
  ///  only in data</summary>
  TAppearanceListView = class(TPresentedListView, IAppearanceItemOwner, IPublishedAppearanceOwner)
  public type
    /// <summary>Generic event invoked on <c>TListViewItem</c></summary>
    TItemEvent = procedure(const Sender: TObject; const AItem: TListViewItem) of object;
    /// <summary>See <c>DoUpdateItemView</c></summary>
    TUpdateObjectsEvent = TItemEvent;
    /// <summary>See <c>DoUpdatingItemView</c></summary>
    TUpdatingObjectsEvent = procedure(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean) of object;

  strict private
    FAppearanceViewItems: TAppearanceListViewItems;
    FAppearanceProperties: TPublishedAppearance;
    FItemAppearanceObjects: TPublishedObjects;
    FItemAppearanceProperties: TItemAppearanceProperties;
    FItemEditAppearanceProperties: TItemAppearanceProperties;
    FHeaderAppearanceProperties: TItemAppearanceProperties;
    FFooterAppearanceProperties: TItemAppearanceProperties;
    FUpdatingAppearance: Integer;
    FChangedAppearanceObjects: TListItemPurposes;
    FChangedAppearanceHeights: TListItemPurposes;
    // See also FItemSelectedBeforeChange
    FItemSelectedBeforeEdit: TListItem;
    FOnButtonClick: TItemControlEvent;
    FOnButtonChange: TItemControlEvent;
    FAppearanceAllowsCheckboxes: Boolean;
    FAppearanceAllowsDeleteMode: Boolean;
    FOnItemClick: TItemEvent;
    FOnUpdatingObjects: TUpdatingObjectsEvent;
    FOnUpdateObjects: TUpdateObjectsEvent;

    function GetFooterAppearanceName: string;
    function GetFooterAppearanceClassName: string;
    function GetHeaderAppearanceName: string;
    function GetHeaderAppearanceClassName: string;
    function GetItemAppearanceName: string;
    function GetItemEditAppearanceName: string;
    function GetItemObjectsClassName: string;
    function GetItemEditObjectsClassName: string;
    procedure SetFooterAppearanceClassName(const Value: string);
    procedure SetHeaderAppearanceClassName(const Value: string);
    procedure SetItemObjectsClassName(const Value: string);
    procedure SetItemEditObjectsClassName(const Value: string);
    procedure SetFooterAppearanceName(const Value: string);
    procedure SetHeaderAppearanceName(const Value: string);
    procedure SetItemAppearanceName(const Value: string);
    procedure SetItemEditAppearanceName(const Value: string);

    procedure SetAppearanceProperties(const Value: TPublishedAppearance);
    procedure SetItemAppearanceObjects(const Value: TPublishedObjects);
    function GetItemAppearanceObjects: TPublishedObjects;
    procedure AppearanceResetObjects(APurposes: TListItemPurposes);
    procedure AppearanceResetHeights(APurposes: TListItemPurposes);

    { IPublishedAppearanceOwner }

    function GetFooterAppearanceProperties: TItemAppearanceProperties;
    function GetHeaderAppearanceProperties: TItemAppearanceProperties;
    function GetItemAppearanceProperties: TItemAppearanceProperties;
    function GetItemEditAppearanceProperties: TItemAppearanceProperties;

    procedure EditorBeforeItemAdded(Sender: IListViewEditor);
    procedure EditorAfterItemAdded(Sender: IListViewEditor; const Item: TListItem);
    procedure EditorBeforeItemDeleted(Sender: IListViewEditor; const Index: Integer);
    procedure EditorAfterItemDeleted(Sender: IListViewEditor);
    procedure ResetViewAppearance(const AItem: TListViewItem);

  protected
    procedure ApplyStyle; override;
    /// <summary>Handler of
    /// <see cref='TAppearanceListViewItems.OnNotify'>TAppearanceListViewItems.OnNotify</see></summary>
    procedure ObjectsNotify(Sender: TObject; const Item: TListItem; Action: TCollectionNotification);
    /// <summary>TAppearanceListView needs adapter to be TAppearanceListViewItems or derivative.
    /// If TAppearanceListView is used with a custom adapter, use <c>Items</c> property to set it
    /// instead of Adapter property of the base class</summary>
    procedure SetAppearanceListViewItems(const AItems: TAppearanceListViewItems);
    procedure DoResetView(const Item: TListItem); override;

    function HasButtonsInCells: Boolean; override;
    function HasDeletionEditMode: Boolean; override;
    function HasCheckboxMode: Boolean; override;
    procedure SetItemHeight(const Value: Integer); virtual;
    procedure SetItemEditHeight(const Value: Integer); virtual;
    procedure SetHeaderHeight(const Value: Integer); virtual;
    procedure SetFooterHeight(const Value: Integer); virtual;

    function GetAppearanceListViewItem(const Index: Integer): TListViewItem; virtual;
    /// <summary>Get height of a specific item</summary>
    function GetItemHeight(const Index: Integer): Integer; overload; override;
    // See respective properties
    function GetItemHeight: Integer; overload; virtual;
    function GetItemEditHeight: Integer; overload; virtual;
    function GetHeaderHeight: Integer; overload; virtual;
    function GetFooterHeight: Integer; overload; virtual;

    procedure WillEnterEditMode(const Animated: Boolean); override;
    procedure DoResetEditModeAnimation; override;

    procedure DoAdapterSet; override;
    // hooks from IListViewController
    procedure DoRequestReindexing(const Item: TListItem); override;
    procedure DoItemResized(const Item: TListItem); override;
    procedure DoCheckStateChanged(const AItem: TListItem; const Control: TListItemDrawable); override;
    procedure DoControlClicked(const Item: TListItem; const Control: TListItemDrawable); override;
    /// <summary>Returns an array of 4 elements comprised by
    /// <para><c>ItemEditAppearanceProperties</c></para>
    /// <para><c>ItemAppearanceProperties</c></para>
    /// <para><c>HeaderAppearanceProperties</c></para>
    /// <para><c>FooterAppearanceProperties</c></para></summary>
    function GetAppearanceProperties: TArray<TItemAppearanceProperties>;
    /// <summary>Refresh items with specified purposes; all items if the set is empty;
    ///  see also <see cref='IListViewAdapter.ResetViews'>IListViewAdapter.ResetViews</see></summary>
    procedure RefreshAppearances(const APurposes: TListItemPurposes = []);
    /// <summary>Same as RefreshAppearances</summary>
    procedure UpdateAppearanceStyleResources;
    /// <summary>Invoked when item appearance changes; resets all item views</summary>
    procedure ItemAppearanceChange(const Sender: TItemAppearanceProperties);
    /// <summary>Invoked when Appearance Objects (view prototype) change</summary>
    procedure ItemAppearanceChangeObjects(const Sender: TItemAppearanceProperties);
    /// <summary>Invoked when appearance height is changed</summary>
    procedure ItemAppearanceChangeHeight(const Sender: TItemAppearanceProperties);
    /// <summary>Resets all item views when entering edit mode</summary>
    procedure EditModeAppearances;
    /// <summary>Reset appearance. When TAppearanceListView is created, it creates appearances
    /// for Item, Edit mode Item, Header, Footer and initializes them by calling this virtual method.
    /// By contract it must select <c>TItemAppearanceProperties.AppearanceClass</c> by searching in
    /// appearances registry for item purpose specified during TItemAppearanceProperties creation</summary>
    /// <param name="AAppearance">instance of <c>TItemAppearanceProperties</c></param>
    /// <remarks>See implementation in
    ///  <see cref='TListView.InitializeItemAppearance'>TListView.InitializeItemAppearance</see>
    ///  <para>See also <see cref='TAppearancesRegistry'>TAppearancesRegistry</see></para>
    /// </remarks>
    procedure InitializeItemAppearance(const AAppearance: TItemAppearanceProperties); virtual;

    procedure DoListItemClick(const AItem: TListItem); override;
    procedure DoUpdatingItemView(const AListItem: TListItem; var AHandled: Boolean); override;
    procedure DoUpdateItemView(const AListItem: TListItem); override;

    // General compatibility properties
    /// <summary>Item height defined by appearance </summary>
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    /// <summary>Item height in edit mode defined by appearance</summary>
    property ItemEditHeight: Integer read GetItemEditHeight write SetItemEditHeight;
    /// <summary>Header height defined by appearance</summary>
    property HeaderHeight: Integer read GetHeaderHeight write SetHeaderHeight;
    /// <summary>Footer height defined by appearance</summary>
    property FooterHeight: Integer read GetFooterHeight write SetFooterHeight;

    // Appearance related properties
    /// <summary>Item in edit mode appearance class name</summary>
    /// <remarks>Must be loaded prior to other Item, header and footer properties
    ///  <para>Changing class name will load new appearance and reinitialize all views</para>
    /// </remarks>
    property ItemEditAppearanceClassName: string read GetItemEditObjectsClassName write SetItemEditObjectsClassName;
    /// <summary>Item appearance class name</summary>
    /// <remarks>Must be loaded prior to other Item, header and footer properties
    ///  <para>Changing class name will load new appearance and reinitialize all views</para>
    /// </remarks>
    property ItemAppearanceClassName: string read GetItemObjectsClassName write SetItemObjectsClassName;
    /// <summary>Header appearance class name</summary>
    /// <remarks>Must be loaded prior to other Item, header and footer properties
    ///  <para>Changing class name will load new appearance and reinitialize all views</para>
    /// </remarks>
    property HeaderAppearanceClassName: string read GetHeaderAppearanceClassName write SetHeaderAppearanceClassName;
    /// <summary>Footer appearance class name</summary>
    /// <remarks>Must be loaded prior to other Item, header and footer properties
    ///  <para>Changing class name will load new appearance and reinitialize all views</para>
    /// </remarks>
    property FooterAppearanceClassName: string read GetFooterAppearanceClassName write SetFooterAppearanceClassName;

    /// <summary>Assign new appearance by name; this will effectively change <c>ItemAppearanceClassName</c> and reload
    /// all views</summary>
    property ItemAppearanceName: string read GetItemAppearanceName write SetItemAppearanceName stored False;
    /// <summary>Assign new appearance by name; this will effectively change <c>ItemEditAppearanceClassName</c> and reload
    /// all views</summary>
    property ItemEditAppearanceName: string read GetItemEditAppearanceName write SetItemEditAppearanceName stored False;
    /// <summary>Assign new appearance by name; this will effectively change <c>HeaderAppearanceClassName</c> and reload
    /// all views</summary>
    property HeaderAppearanceName: string read GetHeaderAppearanceName write SetHeaderAppearanceName stored False;
    /// <summary>Assign new appearance by name; this will effectively change <c>FooterAppearanceClassName</c> and reload
    /// all views</summary>
    property FooterAppearanceName: string read GetFooterAppearanceName write SetFooterAppearanceName stored False;
    // <summary><c>TPublishedAppearance</c> represents appearances in the object inspector</summary>
    property ItemAppearance: TPublishedAppearance read FAppearanceProperties write SetAppearanceProperties;
    /// <summary><c>TPublishedObjects</c> represents appearance items (collections of objects comprising appearances)
    /// in the object inspector</summary>
    property ItemAppearanceObjects: TPublishedObjects read GetItemAppearanceObjects write SetItemAppearanceObjects;
    /// <summary>Invoked on check button state change</summary>
    property OnButtonChange: TItemControlEvent read FOnButtonChange write FOnButtonChange;
    /// <summary>Invoked on embedded button click</summary>
    property OnButtonClick: TItemControlEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure Resize; override;
    /// <summary>Access to the extended adapter that supports appearances and works with extended
    /// items; see <see cref='TListViewItem'>TListViewItem</see></summary>
    property Items: TAppearanceListViewItems read FAppearanceViewItems write SetAppearanceListViewItems;
    /// <summary>Item click event handler</summary>
    property OnItemClick: TItemEvent read FOnItemClick write FOnItemClick;
    /// <summary>Invoked when view is being created by <c>TAppearanceListView.ResetViewAppearance</c> before
    /// calling <c>ResetObjects</c>. If not Handled, the view will be recreated</summary>
    ///  <remarks>Call sequence:
    /// <para><c>TAppearanceListView.ResetViewAppearance:</c></para>
    /// <para><c>OnUpdatingObjects</c> ->
    /// <c>TItemAppearanceObjects.ResetObjects</c> -> <c>OnUpdateObjects</c></para></remarks>
    property OnUpdatingObjects: TUpdatingObjectsEvent read FOnUpdatingObjects write FOnUpdatingObjects;
    /// <summary>Invoked when view is being created by <c>TAppearanceListView.ResetViewAppearance</c> after
    /// calling <c>ResetObjects</c></summary>
    ///  <remarks>Call sequence:
    /// <para><c>TAppearanceListView.ResetViewAppearance:</c></para>
    /// <para><c>OnUpdatingObjects</c> ->
    /// <c>TItemAppearanceObjects.ResetObjects</c> -> <c>OnUpdateObjects</c></para></remarks>
    property OnUpdateObjects: TUpdateObjectsEvent read FOnUpdateObjects write FOnUpdateObjects;

    // True Items Clear
    procedure ItemsClearTrue; // ZuBy
    // Scroll
    function getFirstVisibleItemIndex: Integer; // ZuBy
    function getLastVisibleItemindex: Integer; // ZuBy
    function getVisibleCount: Integer; // ZuBy
    // Custom Item Draw
    procedure SetCustomColorForItem(const ItemIndex: Integer; const aColor: TAlphaColor); // ZuBy
    procedure SetDefaultColorForItem(const ItemIndex: Integer); // ZuBy
    function IsCustomColorUsed(const ItemIndex: Integer): Boolean; // ZuBy
    // Item Draw
    procedure SetColorItemSelected(aColor: TAlphaColor); // ZuBy
    procedure SetColorBackground(aColor: TAlphaColor); // ZuBy
    procedure SetColorItemFill(aColor: TAlphaColor); // ZuBy
    procedure SetColorItemFillAlt(aColor: TAlphaColor); // ZuBy
    procedure SetColorItemSeparator(aColor: TAlphaColor); // ZuBy
    procedure SetColorHeader(aColor: TAlphaColor); // ZuBy
    // Text Draw
    procedure SetColorText(aColor: TAlphaColor); // ZuBy
    procedure SetColorTextSelected(aColor: TAlphaColor); // ZuBy
    procedure SetColorTextDetail(aColor: TAlphaColor); // ZuBy
    procedure SetColorTextHeader(aColor: TAlphaColor); // ZuBy
    procedure SetColorTextHeaderShadow(aColor: TAlphaColor); // ZuBy
    procedure SetColorButtonText(aColor: TAlphaColor); // ZuBy
    procedure SetColorButtonTextPressed(aColor: TAlphaColor); // ZuBy
    procedure SetColorDeleteText(aColor: TAlphaColor); // ZuBy
    procedure SetColorDeleteTintColor(aColor: TAlphaColor); // ZuBy
    procedure SetColorDeleteTextPressed(aColor: TAlphaColor); // ZuBy
    // PullRefresh Draw
    procedure SetColorPullRefresh(aColor: TAlphaColor); // ZuBy
    procedure SetColorPullRefreshIndicator(aColor: TAlphaColor); // ZuBy
    procedure SetColorStretchGlow(aColor: TAlphaColor); // ZuBy
  end;

  TCustomListView = class(TAppearanceListView)
  end;

  TListView = class(TCustomListView)
  protected
    procedure InitializeItemAppearance(const AAppearance: TItemAppearanceProperties); override;
  public
    // Hoist protected appearance properties
    property ItemAppearanceName;
    property ItemEditAppearanceName;
    property HeaderAppearanceName;
    property FooterAppearanceName;
  published
    // Hoist protected appearance properties
    property ItemAppearanceClassName;
    property ItemEditAppearanceClassName;
    property HeaderAppearanceClassName;
    property FooterAppearanceClassName;

    property OnUpdatingObjects;
    property OnUpdateObjects;
    property OnEditModeChange;
    property OnEditModeChanging;
    property EditMode;

    property Transparent default False;
    property AllowSelection;
    property AlternatingColors;
    property ItemIndex;
    property Images;
    property ScrollViewPos;
    property ItemSpaces;
    property SideSpace;

    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;
    property ParentShowHint;
    property ShowHint;

    property ShowFirstSeparator; // zubu
    property ShowLastSeparator; // zubu
    property TransparentSeparators; // zubu
    property TransparentItems; // zubu
    property AutoPositionToItem; // zubu
    property SeparatorLeftOffset; // zubu
    property SeparatorRightOffset; // zubu
    property ItemBottomOffset; // zubu
    property Horizontal; // zubu

    {events}
    property OnApplyStyleLookup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;

    property ItemAppearance;
    property ItemAppearanceObjects;

    property HelpContext;
    property HelpKeyword;
    property HelpType;

    property StyleLookup;
    property TouchTargetExpansion;
    property OnClick;
    property OnDblClick;

    { ListView selection events }
    property CanSwipeDelete;

    property OnChange;
    property OnChangeRepainted;
    property OnItemsChange;
    property OnScrollViewChange;
    property OnItemClick;
    property OnItemClickEx;
    property OnButtonClick;
    property OnButtonChange;

    property OnDeletingItem;
    property OnDeleteItem;
    property OnDeleteChangeVisible;
    property OnSearchChange;
    property OnFilter;
    property OnPullRefresh;
    property DeleteButtonText;

    property AutoTapScroll;
    property AutoTapTreshold;
    property ShowSelection;
    property DisableMouseWheel;

    property SearchVisible;
    property SearchAlwaysOnTop;
    property SelectionCrossfade;
    property PullToRefresh;
    property PullRefreshWait;

    property ControlType;
    property NativeOptions;
  end;
{$ENDREGION}

  EListViewError = class(Exception);

implementation

uses
  {$IFDEF MACOS}Macapi.CoreFoundation,{$ENDIF} System.SyncObjs, System.Math, System.RTLConsts, System.TypInfo,
  System.Math.Vectors, FMX.Consts, FMX.Ani, FMX.Utils, FMX.BehaviorManager {$IFDEF IOS}, FMX.ListView.iOS{$ENDIF}
  {$IFDEF ANDROID}, FMX.ListView.Android{$ENDIF};

var
  LVTextLayout: TTextLayout;

{$REGION 'Types, constants and helper functions'}

const
{$IFDEF IOS}
  DefaultScrollBarWidth = 7;
{$ELSE}
{$IFDEF MACOS}
  DefaultScrollBarWidth = 7;
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  DefaultScrollBarWidth = 16;
{$ENDIF}
{$IFDEF ANDROID}
  DefaultScrollBarWidth = 7;
{$ENDIF}

type
  TOpenBitmap = class(TBitmap);
  TOpenReader = class(TReader);
  TEditModeOption = (DisallowSelection, HideSelection, RadioButtonMode, UncheckMode, ModalMode, ClearWhenStart,
    MultiSelect);
  TEditModeOptions = set of TEditModeOption;

function RectF(X, Y, Width, Height: Single): TRectF; inline;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

procedure DisableHitTestForControl(const AControl: TControl);
var
  LChild: TFmxObject;
begin
  AControl.HitTest := False;
  if AControl.Children <> nil then
    for LChild in AControl.Children do
      if LChild is TControl then
        DisableHitTestForControl(TControl(LChild));
end;

{$ENDREGION}
{$REGION 'TListViewBase'}

class function TListViewBase.TItemSelectionAlpha.Create(const StartTime: Double;
  const Alpha, StartAlpha: Single): TItemSelectionAlpha;
begin
  Result.StartTime := StartTime;
  Result.Alpha := Alpha;
  Result.StartAlpha := StartAlpha;
end;

class function TListViewBase.GetDefaultMargins: TRectF;
begin
  Result := TRectF.Create(DefaultLeftMargin, 0, DefaultRightMargin, 0);
end;

constructor TListViewBase.Create(AOwner: TComponent);
begin
  inherited;

  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');

  FImageLink := TGlyphImageLink.Create(Self);
  FImageLink.IgnoreIndex := True;
  TPlatformServices.Current.SupportsPlatformService(IFMXSystemInformationService, FSystemInformationService);
  TPlatformServices.Current.SupportsPlatformService(IFMXListingService, FListingService);

  FDragListMode := TInternalDragMode.None;
  FDeleteButtonIndex := -1;
  FPrevDeleteButtonIndex := -1;
  FSearchVisible := False;
  FSearchAlwaysOnTop := True;
  FCanSwipeDelete := False;
  FPullToRefresh := False;

  FDelayedIncidents := TDelayedIncidents.Create;
  FSelectionAlphas := TItemSelectionAlphas.Create;

  CanFocus := True;
  DisableFocusEffect := True;
  AutoCapture := True;
  ClipChildren := True;

  FItemBottomOffset := 0; // ZuBy
  FHorizontal := False; // ZuBy
  FTopOffset := 0; // ZuBy
  FBottomOffset := 0; // ZuBy
  FAutoColumns := False; // ZuBy
  FColumnWidth := 180; // ZuBy
  FCanScroll := True; // sinuke
  FColumns := 1; // ZuBy
  FMarg := 0; // ZuBy

  FScrollBar := TScrollBar.Create(nil);
  FScrollBar.Stored := False;
  FScrollBar.Orientation := TOrientation.Vertical;
  FScrollBar.Align := TAlignLayout.Right;
  FScrollBar.Width := DefaultScrollBarWidth;

  FScrollBar.Parent := Self;

  if (not HasTouchTracking) or (csDesigning in ComponentState) then
  begin
    FScrollBar.Visible := False;
    FScrollBar.OnChange := ScrollBarChange;
  end;

  if HasTouchTracking then
  begin
    FAniCalc := TAniCalculations.Create(nil);
    FAniCalc.Animation := True;
    FAniCalc.OnChanged := AniCalcChange;
    FAniCalc.Interval := PhysicsProcessingInterval;
    FAniCalc.OnStart := AniCalcStart;
    FAniCalc.OnStop := AniCalcStop;
    FAniCalc.BoundsAnimation := HasPhysicsStretchyScrolling;
  end;

  FItemSpaces := TBounds.Create(GetDefaultMargins);
  FItemSpaces.OnChange := ItemSpacesChange;

  FBrush := TBrush.Create(TBrushKind.Solid, $FF000000);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, $FF000000);

  FStyleResources := TListItemStyleResources.Create;

  FHeightSums := TItemHeightSums.Create;

  FItemIndex := -1;
  FMouseClickIndex := -1;
  FMouseEventIndex := -1;
  FTransparent := False;
  FAllowSelection := True;
  FShowSelection := True;
  FAutoTapTreshold := 8;
  FTapSelectItemIndex := -1;
  FTapSelectNewIndexApplied := -1;
  FSelectionCrossfade := False;
  FDeleteButtonText := DefaultDeleteButtonText;

  FMaxKnownHeight := 0;
  FScrollScale := 1;

  FTransitionType := TTransitionType.None;
end;

destructor TListViewBase.Destroy;
begin
  FMessageSender.Free;

  DestroyRecurrentTimer;

  if FSearchEdit <> nil then
  begin
    FSearchEdit.Parent := nil;
    FreeAndNil(FSearchEdit);
  end;

  FHeightSums.Free;
  FStroke.Free;
  FBrush.Free;
  FItemSpaces.Free;
  FreeAndNil(FAniCalc);

  FSelectionAlphas.Free;
  FDelayedIncidents.Free;

  FStyleResources.Free;
  FTimerService := nil;
  FListingService := nil;
  FImageLink.DisposeOf;
  inherited;
end;

function TListViewBase.IsRunningOnDesktop: Boolean;
begin
  Result := TOSVersion.Platform in [pfWindows, pfMacOS, pfLinux];
end;

function TListViewBase.HasTouchTracking: Boolean;
begin
  // *** ZuBy
{$IFNDEF MSWINDOWS}
  Result := True;
{$ELSE}
  Result := (FAniCalc <> nil) or ((FSystemInformationService <> nil) and
    (TScrollingBehaviour.TouchTracking in FSystemInformationService.GetScrollingBehaviour));
{$ENDIF}
  // *** ZuBy
end;

function TListViewBase.HasSearchFeatures: Boolean;
begin
  Result := ((FListingService <> nil) and (FListingService.GetSearchFeatures <> [])) or
    (csDesigning in ComponentState);
end;

function TListViewBase.HasSearchAsItem: Boolean;
begin
  Result := (FSearchVisible and (csDesigning in ComponentState)) or
    (FSearchVisible and (FListingService <> nil) and ((not FSearchAlwaysOnTop) or
    (not (TListingSearchFeature.StayOnTop in FListingService.GetSearchFeatures))) and
    (TListingSearchFeature.AsFirstItem in FListingService.GetSearchFeatures) and ((FSearchEdit = nil) or
    (FSearchEdit.Text.Length < 1)));
end;

function TListViewBase.IsDeleteModeAllowed: Boolean;
begin
  Result := TListingEditModeFeature.Delete in FListingService.GetEditModeFeatures;
end;

function TListViewBase.IsEditMode: Boolean;
begin
  Result := FEditMode;
end;

function TListViewBase.HasStretchyScrolling: Boolean;
begin
{$IFNDEF MSWINDOWS}
  Result := True;
{$ELSE}
  Result := HasTouchTracking and (FSystemInformationService <> nil) and
    (TScrollingBehaviour.BoundsAnimation in FSystemInformationService.GetScrollingBehaviour);
{$ENDIF}
end;

function TListViewBase.HasButtonsInCells: Boolean;
begin
  Result := False;
end;

function TListViewBase.HasCheckboxMode: Boolean;
begin
  Result := False;
end;

function TListViewBase.HasDeletionEditMode: Boolean;
begin
  Result := False;
end;

function TListViewBase.HasPhysicsStretchyScrolling: Boolean;
begin
  Result := HasTouchTracking and (FPullToRefresh or HasStretchyScrolling);
end;

function TListViewBase.HasScrollingStretchGlow: Boolean;
begin
  Result := (FListingService <> nil) and (TListingTransitionFeature.ScrollGlow in FListingService.GetTransitionFeatures);
end;

function TListViewBase.HasPullRefreshStroke: Boolean;
begin
  Result := (FListingService <> nil) and
    not (TListingTransitionFeature.PullToRefresh in FListingService.GetTransitionFeatures) and
    ((FPullRefreshAnimation = TPullRefreshAnimation.Playing) or (GetPullRefreshStrength > 0));
end;

function TListViewBase.CanDisplaySelectionForItem(const Index: Integer; const Item: TListItem; const IncludeMultiSelect,
  IncludeCrossFaded: Boolean): Boolean;
var
  ItemAlpha: TItemSelectionAlpha;
  Checkable: IListViewCheckProvider;
  LItem: TListItem;
begin
  LItem := Item;
  if LItem = nil then
    LItem := Adapter[Index];
  Result := ((FItemIndex = Index) and FShowSelection and LItem.View.Initialized and
    (LItem.Purpose = TListItemPurpose.None) and (not FEditMode) and (FDeleteButtonIndex = -1)) or
    (HasCheckboxMode and IncludeMultiSelect and LItem.View.Initialized and
    Supports(Adapter, IListViewCheckProvider, Checkable) and Checkable.Checked[Index]);

  if (not Result) and IncludeCrossFaded and (FSelectionAlphas <> nil) then
    if FSelectionAlphas.TryGetValue(Index, ItemAlpha) then
      Result := ItemAlpha.Alpha > TEpsilon.Vector;
end;

function TListViewBase.GetDefaultSelectionAlpha: Single;
begin
  if FEditMode then
    Result := EditModeSelectionAlpha
  else
    Result := 1;
end;

procedure TListViewBase.OnSearchEditResize(Sender: TObject);
begin
  InvalidateHeights;
  StartIncident(TDelayedIncident.Invalidate);
end;

procedure TListViewBase.OnSearchEditChange(Sender: TObject);
begin
  if Assigned(FOnSearchChange) then
    FOnSearchChange(Self);
end;

function TListViewBase.GetItemCount: Integer;
begin
  if Adapter <> nil then
    Result := Adapter.Count
  else
    Result := -1;
end;

procedure TListViewBase.DoItemsMayChange;
begin
  inherited;
  FItemSelectedBeforeChange := Selected;
end;

procedure TListViewBase.DoItemsCouldHaveChanged;
var
  SelectionChanged: Boolean;
begin
  inherited;
  SelectionChanged := (Selected <> nil) and (FItemSelectedBeforeChange <> nil) and (Selected <> FItemSelectedBeforeChange);

  if (FItemSelectedBeforeChange <> nil) and
    (FItemSelectedBeforeChange.Index >= 0) and (FItemSelectedBeforeChange.Index < Adapter.Count)
    and (Adapter[FItemSelectedBeforeChange.Index] = FItemSelectedBeforeChange) then
    ItemIndex := FItemSelectedBeforeChange.Index
  else
    ItemIndex := -1;

  FItemSelectedBeforeChange := nil;

  if SelectionChanged then
  begin
    TLinkObservers.ListSelectionChanged(Observers);
    DoChange;
  end;

  InvalidateHeights;
  Invalidate;
  RebuildList;
end;

procedure TListViewBase.DoItemsInvalidate;
begin
  inherited;
  Invalidate;
end;

procedure TListViewBase.ItemSpacesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TListViewBase.SetSideSpace(const Value: Integer);
var
  NewValue: Integer;
begin
  NewValue := Max(Value, 0);

  if FSideSpace <> NewValue then
  begin
    FSideSpace := NewValue;
    Invalidate;
  end;
end;

procedure TListViewBase.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
begin
    FTransparent := Value;
    Invalidate;
  end;
end;

function TListViewBase.ShouldHandleEvents: Boolean;
begin
  Result := True;
end;

procedure TListViewBase.SelectItem(const ItemIndex: Integer);
var
  HasChange: Boolean;
  NewItemIndex: Integer;
  Checkable: IListViewCheckProvider;
begin
  NewItemIndex := ItemIndex;

  if (NewItemIndex >= 0) and (NewItemIndex < Adapter.Count) then
  begin
    if FEditMode and Supports(Adapter, IListViewCheckProvider, Checkable) then
      Checkable[NewItemIndex] := True;
  end
  else
    NewItemIndex := -1;

  HasChange := FItemIndex <> NewItemIndex;

  SetItemIndex(NewItemIndex);

  if NewItemIndex <> -1 then
    DoListItemClick(Adapter[NewItemIndex]);

  if HasChange then
    DoChange;

  Invalidate;

  FClickEventControl := nil;
  FClickEventItemIndex := NewItemIndex;
  FClickEventMousePos := TPointF.Zero;
  if not FMouseClickSwipeEventSend then // ZuBy
    StartIncident(TDelayedIncident.ClickEvent);
end;

procedure TListViewBase.UnselectItem(const ItemIndex: Integer);
var
  Checkable: IListViewCheckProvider;
begin
  if (ItemIndex >= 0) and (ItemIndex < Adapter.Count) and
    Supports(Adapter, IListViewCheckProvider, Checkable) then
    Checkable[ItemIndex] := False;
end;

procedure TListViewBase.SetAlternatingColors(const Value: Boolean);
begin
  if FAlternatingColors <> Value then
  begin
    FAlternatingColors := Value;
    Invalidate;
  end;
end;

procedure TListViewBase.SetItemIndexInternal(const Value: Integer; const DisableSelection, DisableCrossfade: Boolean);
var
  NewValue: Integer;
begin
  NewValue := Value;

  if (NewValue < 0) or (NewValue > Adapter.Count - 1) then
    NewValue := -1;

  if NewValue <> FItemIndex then
  begin
    if (not FEditMode) and InRange(FItemIndex, 0, Adapter.Count - 1) and not DisableCrossfade
    then
      InsertItemCrossFade(FItemIndex, False);

    FItemIndex := NewValue;

    // ZuBy ***
    if FMakeSelectedItemVisible then
    begin
      if FItemIndex <> -1 then
        ScrollTo(FItemIndex); // Make selected item visible.
    end;
    // *** ZuBy

    if (not DisableSelection) and (FItemIndex >= 0) and (FItemIndex < Adapter.Count) then
      Adapter[FItemIndex].MouseSelect;

    if (not FEditMode) and (FItemIndex <> -1) and not DisableCrossfade then
      InsertItemCrossFade(FItemIndex, True);

    DoSetItemIndexInternal(FItemIndex);
    Invalidate;
  end;
end;

function TListViewBase.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TListViewBase.SetItemIndex(const Value: Integer);
begin
  // The data may not be ready at the moment of component loading, cannot properly set item index at this time.
  if (not (csLoading in ComponentState)) or (Value = -1) then
    SetItemIndexInternal(Value)
  else
    StartIncident(TDelayedIncident.SetItemIndex, True, 0, Value);
end;

procedure TListViewBase.SetEditMode(const Value: Boolean);
var
  LHandled: Boolean;
  Checkable: IListViewCheckProvider;
begin
  if (FEditMode <> Value) and Supports(Adapter, IListViewCheckProvider, Checkable) then
  begin
    BeginUpdate;
    try
      Checkable.CheckAll(False);

      if FDeleteButtonIndex <> -1 then
      begin
        SetDeleteButtonIndex(-1);
        ResetDeleteModeAnimation;
      end;

      SetItemIndex(-1);

      LHandled := False;

      DoEditModeChanging(LHandled);

      if not LHandled then
      begin
        FEditMode := Value;

        if FSearchEdit <> nil then
          FSearchEdit.Enabled := not FEditMode;

        if (FListingService <> nil) and (TListingTransitionFeature.EditMode in FListingService.GetTransitionFeatures) then
        begin // Animated Edit Mode
          if EditMode then
            WillEnterEditMode(True); //EditModeAppearances;
          InitEditModeAnimation;
        end
        else
        begin // Instant Edit Mode
          if FEditMode then
            FEditModeTransitionAlpha := 1
          else
            FEditModeTransitionAlpha := 0;
          WillEnterEditMode(False);
          Invalidate;
        end;
      end;

      DoEditModeChange;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TListViewBase.WillEnterEditMode(const Animated: Boolean);
begin
end;

procedure TListViewBase.SetCanSwipeDelete(const Value: Boolean);
begin
  if FCanSwipeDelete <> Value then
    FCanSwipeDelete := Value;
end;

procedure TListViewBase.SetSearchVisible(const Value: Boolean);
begin
  if FSearchVisible <> Value then
  begin
    FSearchVisible := Value;

    if FSearchVisible and HasSearchFeatures and (FSearchEdit = nil) then
    begin
      FSearchEdit := TSearchBox.Create(Self);
      FSearchEdit.Stored := False;
      FSearchEdit.Locked := True;
      FSearchEdit.Parent := Self;
      FSearchEdit.OnResize := OnSearchEditResize;
      FSearchEdit.OnChange := OnSearchEditChange;
      FSearchEdit.OnFilter := OnFilter;
    end;

    if FSearchEdit <> nil then
    begin
      FSearchEdit.Visible := FSearchVisible;
      UpdateSearchEditPos;
    end;

    InvalidateHeights;
    Invalidate;
    RecreateNativePresentation;
  end;
end;

procedure TListViewBase.SetSearchAlwaysOnTop(const Value: Boolean);
begin
  if FSearchAlwaysOnTop <> Value then
  begin
    FSearchAlwaysOnTop := Value;
    InvalidateHeights;
    Invalidate;
    RecreateNativePresentation;
  end;
end;

procedure TListViewBase.SetControlType(const Value: TControlType);
begin
  if FControlType <> Value then
  begin
    FControlType := Value;
    if not (csLoading in ComponentState) then
    begin
      RecreateNativePresentation;
      Invalidate;
    end;
  end;
end;

procedure TListViewBase.SetNativeOptions(const Value: TListViewNativeOptions);
begin
  if FNativeOptions <> Value then
  begin
    FNativeOptions := Value;
    RecreateNativePresentation;
  end;
end;

procedure TListViewBase.SetOnFilter(const Value: TFilterEvent);
begin
  FOnFilter := Value;
  if FSearchEdit <> nil then
    FSearchEdit.OnFilter := OnFilter;
end;

function TListViewBase.GetEditModeTransitionAlpha: Single;
begin
  Result:= FEditModeTransitionAlpha;
end;

function TListViewBase.GetDeleteModeTransitionAlpha: Single;
begin
  Result:= FDeleteModeTransitionAlpha;
end;

function TListViewBase.GetItemEditOffset(const Item: TListItem): Single;
var
  Provider: IListViewGlyphButtonProvider;
  Drawable: TListItemGlyphButton;
begin
  Result := 0;
  if (Item <> nil) and Supports(Adapter, IListViewGlyphButtonProvider, Provider) then
  begin
    Drawable := Provider.GlyphButtonDrawable[Item.Index];
    if Drawable <> nil then
      Result := Drawable.Width;
  end;
end;

function TListViewBase.GetItemDeleteCutoff(const Item: TListItem): Single;
begin
  if (Item <> nil) and ((Item.Index = FDeleteButtonIndex) or (Item.Index = FPrevDeleteButtonIndex)) and
    (FDeleteLayout <> nil) then
    Result := FDeleteLayout.Position.X
  else
    Result := 0;
end;

function TListViewBase.GetClientMargins: TRectF;
begin
  Result := LocalRect;
end;

function TListViewBase.GetItemCurrentSelectionAlpha(const Item: TListItem): Single;
begin
  if Item <> nil then
    Result := Min(GetItemSelectionAlpha(Item.Index) / GetDefaultSelectionAlpha, 1)
  else
    Result := 0;
end;

procedure TListViewBase.CheckStateChanged(const Item: TListItem; const Control: TListItemDrawable);
begin
  DoCheckStateChanged(Item, Control);
end;

procedure TListViewBase.ControlClicked(const Item: TListItem; const Control: TListItemDrawable);
begin
  DoControlClicked(Item, Control);
end;

procedure TListViewBase.RequestReindexing(const Item: TListItem);
begin
  DoRequestReindexing(Item);
end;

procedure TListViewBase.ItemInvalidated(const Item: TListItem);
begin
  Invalidate;
  DoItemInvalidated(Item);
end;

procedure TListViewBase.ItemResized(const Item: TListItem);
begin
  DoItemResized(Item);
end;

procedure TListViewBase.DoEditModeChanging(var AHandled: Boolean);
begin
  if Assigned(FOnEditModeChanging) then
    FOnEditModeChanging(Self, AHandled);
end;

procedure TListViewBase.DoEditModeChange;
begin
  if Assigned(FOnEditModeChange) then
    FOnEditModeChange(Self);
end;

procedure TListViewBase.DoItemInvalidated(const Item: TListItem);
begin
end;

procedure TListViewBase.DoItemResized(const Item: TListItem);
begin
end;

procedure TListViewBase.DoRequestReindexing(const Item: TListItem);
begin
end;

procedure TListViewBase.DoCheckStateChanged(const Item: TListItem; const Control: TListItemDrawable);
begin
end;

procedure TListViewBase.DoControlClicked(const Item: TListItem; const Control: TListItemDrawable);
begin
end;

procedure TListViewBase.DoItemsChange;
begin
  UpdateScrollingLimits;
  RebuildList;

  if Assigned(FOnItemsChange) then
    FOnItemsChange(Self);
end;

procedure TListViewBase.DoAdapterSet;
begin
  UpdateScrollingLimits;
end;

procedure TListViewBase.SetFilterPredicate(const Predicate: TPredicate<string>);
var
  Filterable: IListViewFilterable;
begin
  if Supports(Adapter, IListViewFilterable, Filterable) then
    Filterable.Filter := Predicate;
end;

procedure TListViewBase.RebuildList;
begin
end;

procedure TListViewBase.StopPullRefresh;
begin
end;

procedure TListViewBase.SetItemSpaces(const Value: TBounds);
begin
  FItemSpaces.Assign(Value);
end;

function TListViewBase.GetDeleteButtonText: string;
begin
  if FDeleteButton <> nil then
    Result := FDeleteButton.Text
  else
    Result := FDeleteButtonText;
end;

procedure TListViewBase.SetDeleteButtonText(const Value: string);
begin
  FDeleteButtonText := Value;

  if FDeleteButton <> nil then
    FDeleteButton.Text := Value;
end;

function TListViewBase.DeleteButtonTextStored: Boolean;
begin
  Result := FDeleteButtonText <> DefaultDeleteButtonText;
end;

function TListViewBase.DeleteItem(const ItemIndex: Integer): Boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Adapter.Count);

  if Result and Assigned(FOnDeletingItem) then
    FOnDeletingItem(Self, ItemIndex, Result);

  if Result then
  begin
    if (FItemIndex <> -1) and (FItemIndex >= ItemIndex) then
      SetItemIndex(-1);

    DoDeleteItem(ItemIndex);

    if Assigned(FOnDeleteItem) then
      FOnDeleteItem(Self, FDeleteButtonIndex);
  end;
end;

procedure TListViewBase.DoDeleteItem(const ItemIndex: Integer);
var
  Editor: IListViewEditor;

  function Purpose(const AIndex: Integer): TListItemPurpose;
  begin
    Result := TListItemPurpose.None;
    if (AIndex >= 0) and (AIndex < Adapter.Count) then
      Result := Adapter[AIndex].Purpose;
  end;

  procedure DeleteEmptySection(const Index: Integer);
  begin
    if Purpose(Index - 1) = TListItemPurpose.Header then
    begin
      if Purpose(Index) = TListItemPurpose.Footer then
        Editor.Delete(Index);
      if (Index = Adapter.Count) or (Purpose(Index) <> TListItemPurpose.None) then
        Editor.Delete(Index - 1);
    end;
  end;

begin
  if Supports(Adapter, IListViewEditor, Editor) then
  begin
    Editor.Delete(ItemIndex);
    DeleteEmptySection(ItemIndex);
  end;
end;

function TListViewBase.HasRecurrentTimerEvents: Boolean;
begin
  Result :=
    // Delayed Incidents
    ((FDelayedIncidents <> nil) and (FDelayedIncidents.Count > 0)) or
    // Animation/transition
    (FTransitionType <> TTransitionType.None) or
    // Tap Selection
    (FTapSelectItemIndex <> -1) or
    // Selection Crossfading
    ((FSelectionAlphas <> nil) and (FSelectionAlphas.Count > 0)) or
    // Pull to Refresh animation
    (FPullRefreshAnimation = TPullRefreshAnimation.Playing);
end;

procedure TListViewBase.DestroyRecurrentTimer;
begin
  if FRecurrentTimerHandle <> 0 then
  begin
    FTimerService.DestroyTimer(FRecurrentTimerHandle);
    FRecurrentTimerHandle := 0;
  end;
end;

procedure TListViewBase.UpdateRecurrentTimer;
var
  HasEvents: Boolean;
begin
  HasEvents := HasRecurrentTimerEvents;

  if HasEvents and (FRecurrentTimerHandle = 0) then
    FRecurrentTimerHandle := FTimerService.CreateTimer(RecurrentTimerInterval, RecurrentTimerEvent)
  else if (not HasEvents) and (FRecurrentTimerHandle <> 0) then
    DestroyRecurrentTimer;
end;

procedure TListViewBase.RecurrentTimerEvent;
begin
  if (FDelayedIncidents <> nil) and (FDelayedIncidents.Count > 0) then
    ProcessDelayedIncidents;

  if FTransitionType <> TTransitionType.None then
    ProcessTransitionAnimation;

  if FTapSelectItemIndex <> -1 then
    ProcessTapSelectItem;

  if (FSelectionAlphas <> nil) and (FSelectionAlphas.Count > 0) then
    ProcessSelectionAlphas;

  if FPullRefreshAnimation = TPullRefreshAnimation.Playing then
    ProcessPullRefreshAnimation;

  UpdateRecurrentTimer;
end;

procedure TListViewBase.StartIncident(const Incident: TDelayedIncident; const Triggered: Boolean;
  const TimeToWait: Single; const CustomData: NativeInt);
var
  Entry: TDelayedIncidentEntry;
begin
  FillChar(Entry, SizeOf(TDelayedIncidentEntry), 0);
  Entry.Incident := Incident;
  Entry.Triggered := Triggered;
  Entry.StartTime := FTimerService.GetTick;
  Entry.TimeToWait := TimeToWait;
  Entry.CustomData := CustomData;

  FDelayedIncidents.Add(Entry);
  UpdateRecurrentTimer;
end;

procedure TListViewBase.TriggerIncidents(const Incident: TDelayedIncident; const ResetStartupTime: Boolean);
var
  I: Integer;
  CurTime: Double;
  Entry: TDelayedIncidentEntry;
begin
  CurTime := FTimerService.GetTick;

  for I := 0 to FDelayedIncidents.Count - 1 do
  begin
    Entry := FDelayedIncidents[I];

    if Entry.Incident = Incident then
    begin
      Entry.Triggered := True;

      if ResetStartupTime then
        Entry.StartTime := CurTime;
    end;

    FDelayedIncidents[I] := Entry;
  end;
end;

procedure TListViewBase.ProcessIncident(const Entry: TDelayedIncidentEntry);
var
  J, RowColumns: Integer;
  pW, ObjX: Single;
  iObject: TListItemDrawable;
  ObjPoint: TPointF;
  DrawebleName: string;
begin
  case Entry.Incident of
    TDelayedIncident.ChangeRepainted:
      DoChangeRepainted;

    TDelayedIncident.Invalidate:
      Invalidate;

    TDelayedIncident.SetItemIndex:
      SetItemIndexInternal(Entry.CustomData);

    TDelayedIncident.ClickEvent:
      begin
        if Assigned(FOnItemClickEx) and (not FMouseClickSwipeEventSend) then
          FOnItemClickEx(Self, FClickEventItemIndex, FClickEventMousePos,
            FClickEventControl);
        // ZuBy ***
        if (FAutoColumns)
        { and (not FCanSwipeDirection) and (Assigned(FOnColumnClick)) } then
        begin
          pW := 0;
          RowColumns := Min(TListViewItem(Adapter[FClickEventItemIndex]).Tag,
            FColumns);
          for J := 1 to RowColumns do
          begin
            pW := (J * FColumnWidth) - FColumnWidth;
            if InRange(FClickEventMousePos.X, (pW + (J * FMarg)),
              (pW + FColumnWidth) + (J * FMarg)) then
              break;
          end;
          if (J > RowColumns) or (J < 0) then
            exit;
          if Assigned(FOnColumnClick) then
          begin
            ObjX := FClickEventMousePos.X - (pW + (J * FMarg));
            DrawebleName := '';
            iObject := TListViewItem(Adapter[FClickEventItemIndex])
              .Objects.FindObjectT<TListItemDrawable>('bitmap' + J.ToString);
            if iObject <> nil then
            begin
              ObjPoint := iObject.PlaceOffset.Point;
              if InRange(FClickEventMousePos.X, ObjPoint.X,
                ObjPoint.X + iObject.Width) and InRange(FClickEventMousePos.Y,
                ObjPoint.Y, ObjPoint.Y + iObject.Height) then
                DrawebleName := iObject.Name;
            end;
            if DrawebleName.IsEmpty then
            begin
              iObject := TListViewItem(Adapter[FClickEventItemIndex])
                .Objects.FindObjectT<TListItemDrawable>
                ('oi_values' + J.ToString);
              if iObject <> nil then
              begin
                if iObject.Visible then
                begin
                  ObjPoint := iObject.PlaceOffset.Point;
                  if InRange(FClickEventMousePos.X, ObjPoint.X,
                    ObjPoint.X + iObject.Width) and
                    InRange(FClickEventMousePos.Y, ObjPoint.Y,
                    ObjPoint.Y + iObject.Height) then
                    DrawebleName := iObject.Name;
                end;
              end;
            end;
            if DrawebleName.IsEmpty then
            begin
              iObject := TListViewItem(Adapter[FClickEventItemIndex])
                .Objects.FindObjectT<TListItemDrawable>('price' + J.ToString);
              if iObject <> nil then
              begin
                ObjPoint := iObject.PlaceOffset.Point;
                if InRange(FClickEventMousePos.X, ObjPoint.X,
                  ObjPoint.X + iObject.Width) and InRange(FClickEventMousePos.Y,
                  ObjPoint.Y, ObjPoint.Y + iObject.Height) then
                  DrawebleName := iObject.Name;
              end;
            end;
            if DrawebleName.IsEmpty then
              DrawebleName := 'item';
            FOnColumnClick(Self, J, ObjX, FClickEventMousePos.Y,
              TListViewItem(Adapter[FClickEventItemIndex]), DrawebleName);
          end;
          // *** ZuBy
        end;
      end;
  end;
end;

procedure TListViewBase.ProcessDelayedIncidents;

  function IsUnsafeIncident(const Incident: TDelayedIncident): Boolean;
  begin
    Result := Incident in [TDelayedIncident.ChangeRepainted, TDelayedIncident.ClickEvent];
  end;

var
  I: Integer;
  CurTime: Double;
  Entry: TDelayedIncidentEntry;
  UnsafeIncidents: TDelayedIncidents;
begin
  UnsafeIncidents := nil;
  try
    CurTime := FTimerService.GetTick;

    for I := FDelayedIncidents.Count - 1 downto 0 do
    begin
      Entry := FDelayedIncidents[I];

      if Entry.Triggered and (Abs(CurTime - Entry.StartTime) >= Entry.TimeToWait) then
      begin
        if IsUnsafeIncident(Entry.Incident) then
        begin
          if UnsafeIncidents = nil then
            UnsafeIncidents := TDelayedIncidents.Create;

          UnsafeIncidents.Add(Entry);
        end
        else
          ProcessIncident(Entry);

        FDelayedIncidents.Delete(I);
      end;
    end;

    if UnsafeIncidents <> nil then
      for I := 0 to UnsafeIncidents.Count - 1 do
        ProcessIncident(UnsafeIncidents[I]);
  finally
    UnsafeIncidents.Free;
  end;
end;

procedure TListViewBase.ProcessTransitionAnimation;
begin
  case FTransitionType of
    TTransitionType.EditMode:
      if FEditMode then
      begin
        FEditModeTransitionAlpha := Min(Abs(FTimerService.GetTick - FTransitionStartTime) /
          EditModeAnimationDuration, 1);

        if FEditModeTransitionAlpha >= 1 then
          ResetEditModeAnimation;
      end
      else
      begin
        FEditModeTransitionAlpha := Max(1 - (Abs(FTimerService.GetTick - FTransitionStartTime) /
          EditModeAnimationDuration), 0);

        if FEditModeTransitionAlpha <= 0 then
          ResetEditModeAnimation;
      end;

    TTransitionType.DeleteMode:
      begin
        if FDeleteButtonIndex <> -1 then
        begin
          FDeleteModeTransitionAlpha := Min(Abs(FTimerService.GetTick - FTransitionStartTime) /
            DeleteModeAnimationDuration, 1);

          if FDeleteModeTransitionAlpha >= 1 then
            ResetDeleteModeAnimation;
        end
        else
        begin
          FDeleteModeTransitionAlpha := Max(1 - (Abs(FTimerService.GetTick - FTransitionStartTime) /
            DeleteModeAnimationDuration), 0);

          if FDeleteModeTransitionAlpha <= 0 then
            ResetDeleteModeAnimation;
        end;

        UpdateDeleteButtonLayout;
      end;
  end;

  if FTransitionType <> TTransitionType.None then
    Invalidate;
end;

procedure TListViewBase.ProcessTapSelectItem;
var
  Checkable: IListViewCheckProvider;
begin
  if Abs(FTimerService.GetTick - FTapSelectStartTime) >= TapSelectWaitTime then
  begin
    if FAllowSelection then
    begin
      if Adapter[FTapSelectItemIndex].HasClickOnSelectItems then
        FItemIndex := -1;

      SetNewItemIndex(FTapSelectItemIndex);
      FTapSelectNewIndexApplied := FTapSelectItemIndex;

      FClickEventItemIndex := FTapSelectItemIndex;
      if not FMouseClickSwipeEventSend then // ZuBy
        StartIncident(TDelayedIncident.ClickEvent);
    end
    else if FEditMode and Supports(Adapter, IListViewCheckProvider, Checkable) then
     begin
       Checkable[FTapSelectItemIndex] := not Checkable[FTapSelectItemIndex];
       FTapSelectNewIndexApplied := FTapSelectItemIndex;
     end;

    FTapSelectItemIndex := -1;
  end;
end;

procedure TListViewBase.ProcessSelectionAlphas;
type
  TTrashedItems = TList<Integer>;
var
  TrashedItems: TTrashedItems;
  ItemAlpha: TItemSelectionAlpha;
  Index: Integer;
  MaxIndex: Integer;
  CurTime: Double;
  NewAlpha, Theta, FinalAlpha: Single;
  NeedRepaint: Boolean;
begin
  CurTime := FTimerService.GetTick;
  NeedRepaint := False;
  MaxIndex := Adapter.Count - 1;
  TrashedItems := TTrashedItems.Create;
  try
    for Index in FSelectionAlphas.Keys do
    begin
      if not FSelectionAlphas.TryGetValue(Index, ItemAlpha) or not InRange(Index, 0, MaxIndex) then
      begin
        TrashedItems.Add(Index);
        Continue;
      end;

      if CanDisplaySelectionForItem(Index, Adapter[Index], True) then
      begin
        Theta := Abs(CurTime - ItemAlpha.StartTime) / SelectionFadeInTime;
        FinalAlpha := GetDefaultSelectionAlpha;
      end
      else
      begin
        Theta := Abs(CurTime - ItemAlpha.StartTime) / SelectionFadeOutTime;
        FinalAlpha := 0;
      end;

      NewAlpha := ItemAlpha.StartAlpha + (FinalAlpha - ItemAlpha.StartAlpha) * Theta;
      if not SameValue(NewAlpha, ItemAlpha.Alpha, TEpsilon.Vector) then
      begin
        ItemAlpha.Alpha := NewAlpha;

        if Theta >= 1 then
          TrashedItems.Add(Index)
        else
          FSelectionAlphas.AddOrSetValue(Index, ItemAlpha);

        NeedRepaint := True;
      end;
    end;

    for Index in TrashedItems do
      FSelectionAlphas.Remove(Index);
  finally
    TrashedItems.Free;
  end;

  if NeedRepaint then
    Invalidate;
end;

procedure TListViewBase.InsertItemCrossFade(const Index: Integer; const ShowAnimation: Boolean);
var
  ItemAlpha, PrevItemAlpha: TItemSelectionAlpha;
begin
  if (not FSelectionCrossFade) or (FSelectionAlphas = nil) or (Adapter[Index].Purpose <> TListItemPurpose.None) then
    exit;

  if ShowAnimation then
    ItemAlpha := TItemSelectionAlpha.Create(FTimerService.GetTick, 0, 0)
  else
    ItemAlpha := TItemSelectionAlpha.Create(FTimerService.GetTick, GetDefaultSelectionAlpha, GetDefaultSelectionAlpha);

  if FSelectionAlphas.TryGetValue(Index, PrevItemAlpha) then
  begin
    ItemAlpha.StartAlpha := PrevItemAlpha.Alpha;
    ItemAlpha.Alpha := PrevItemAlpha.Alpha;
  end;

  FSelectionAlphas.AddOrSetValue(Index, ItemAlpha);

  UpdateRecurrentTimer;
end;

procedure TListViewBase.RemoveItemCrossFade(const Index: Integer);
begin
  if FSelectionAlphas.ContainsKey(Index) then
    FSelectionAlphas.Remove(Index);
end;

function TListViewBase.GetItemSelectionAlpha(const Index: Integer): Single;
var
  ItemAlpha: TItemSelectionAlpha;
begin
  if (FSelectionAlphas = nil) or (FSelectionAlphas.Count < 1) then
    exit(GetDefaultSelectionAlpha);

  if FSelectionAlphas.TryGetValue(Index, ItemAlpha) then
    Result := ItemAlpha.Alpha
  else
    Result := GetDefaultSelectionAlpha;
end;

procedure TListViewBase.InitEditModeAnimation;
var
  Checkable: IListViewCheckProvider;
begin
  if Supports(Adapter, IListViewCheckProvider, Checkable) then
  begin
    if Checkable.FirstChecked(True) <> -1 then
    begin
      Checkable.CheckAll(False);
      SetDeleteButtonIndex(-1);
    end;

    UpdateDeleteButtonLayout;

    FTransitionType := TTransitionType.EditMode;
    FTransitionStartTime := FTimerService.GetTick;
    UpdateRecurrentTimer;
  end;
end;

procedure TListViewBase.ResetEditModeAnimation;
begin
  FTransitionType := TTransitionType.None;
  UpdateRecurrentTimer;

  if EditMode then
    FEditModeTransitionAlpha := 1
  else
    FEditModeTransitionAlpha := 0;
                                                                       
  InvalidateHeights;
  Invalidate;
end;

procedure TListViewBase.InitDeleteModeAnimation;
begin
  FTransitionType := TTransitionType.DeleteMode;
  FTransitionStartTime := FTimerService.GetTick;
  UpdateRecurrentTimer;

  if FDeleteLayout = nil then
  begin
    FDeleteLayout := TLayout.Create(Self);
    FDeleteLayout.Stored := False;
    FDeleteLayout.Locked := True;
    FDeleteLayout.Width := DefaultDeleteButtonWidth;
    FDeleteLayout.ClipChildren := True;
    FDeleteLayout.Parent := Self;
  end;

  if FDeleteButton = nil then
  begin
    FDeleteButton := TSpeedButton.Create(FDeleteLayout);
    FDeleteButton.Stored := False;
    FDeleteButton.Locked := True;
    FDeleteButton.Align := TAlignLayout.MostRight;
    FDeleteButton.Width := DefaultDeleteButtonWidth;
    FDeleteButton.StyleLookup := 'listitemdeletebutton';
    FDeleteButton.Text := FDeleteButtonText;
    FDeleteButton.OnClick := DeleteButtonClicked;
    FDeleteButton.Parent := FDeleteLayout;
  end;

  UpdateDeleteButtonLayout;
end;

procedure TListViewBase.ResetDeleteModeAnimation;
begin
  FTransitionType := TTransitionType.None;
  UpdateRecurrentTimer;

  FDeleteButton.Visible := Adapter.Count > 0;
  Invalidate;

  if FDeleteButtonIndex <> -1 then
    FDeleteModeTransitionAlpha := 1
  else
  begin
    FPrevDeleteButtonIndex := -1;
    FDeleteModeTransitionAlpha := 0;
  end;

  if FDeleteButtonIndex = -1 then
  begin
    if FDeleteButton <> nil then
    begin
      FDeleteButton.Parent := nil;
      FreeAndNil(FDeleteButton);
    end;

    if FDeleteLayout <> nil then
    begin
      FDeleteLayout.Parent := nil;
      FreeAndNil(FDeleteLayout);
    end;
  end;
end;

procedure TListViewBase.StartPullRefreshAnimation;
begin
  FPullRefreshAnimation := TPullRefreshAnimation.Playing;
  FPullRefreshAnimationStartTime := FTimerService.GetTick;
  FPullRefreshAnimationStopTime := FPullRefreshAnimationStartTime;
  UpdateRecurrentTimer;
end;

procedure TListViewBase.ProcessPullRefreshAnimation;
var
  EndTrigger: Boolean;
begin
  if TListingTransitionFeature.PullToRefresh in FListingService.GetTransitionFeatures then
    EndTrigger := GetPullRefreshIndicatorAlpha <= 0
  else
  begin
    // ZuBy ***
    if FHorizontal then
      EndTrigger := GetPullRefreshStrokeWidth >= Height
    else
      EndTrigger := GetPullRefreshStrokeWidth >= Width;
    // *** ZuBy
  end;

  if EndTrigger then
  begin
    FPullRefreshAnimation := TPullRefreshAnimation.Finished;
    UpdatePullRefreshState;
  end;

  Invalidate;
end;

function TListViewBase.GetPullRefreshStrength: Single;
begin
  if FScrollStretchStrength < 0 then
    Result := -FScrollStretchStrength
  else
    Result := 0;
end;

function TListViewBase.GetPullRefreshIndicatorSteps: Integer;
const
  IndicatorStrengthPerStep = 5;
begin
  case FPullRefreshAnimation of
    TListViewBase.TPullRefreshAnimation.NotPlaying:
      Result := EnsureRange(Round((GetPullRefreshStrength - PullRefreshIndicatorStrengthStart) /
        IndicatorStrengthPerStep), 0, PullRefreshIndicatorMaxSteps);

    TListViewBase.TPullRefreshAnimation.Playing:
      Result := PullRefreshIndicatorMaxSteps;

  else
    Result := 0;
  end;
end;

function TListViewBase.GetPullRefreshIndicatorAlpha: Single;
const
  IndicatorFadeVelocity = 4;
begin
  case FPullRefreshAnimation of
    TListViewBase.TPullRefreshAnimation.NotPlaying:
      Result := 1;

    TListViewBase.TPullRefreshAnimation.Playing:
      if SameValue(FPullRefreshAnimationStartTime, FPullRefreshAnimationStopTime, TEpsilon.Vector) then
        Result := 1
      else
        Result := Max((1 - Abs(FTimerService.GetTick - FPullRefreshAnimationStopTime) * IndicatorFadeVelocity), 0);

  else
    Result := 0;
  end;
end;

procedure TListViewBase.PaintPullRefreshIndicator(const ACanvas: TCanvas; const AStrength, AOpacity: Single);
const
  IndicatorMinRadius = 6.5;
  IndicatorMaxRadius = 13.5;
  IndicatorThickness = 2;
  IndicatorRotation = 2;
  IndicatorDisappearFraction = 0.7;
  PiMulTwo = 2 * Pi;
  PiByTwo = Pi / 2;
var
  Stroke: TStrokeBrush;
  I, LineCount: Integer;
  Center, P1, P2: TPointF;
  LSin, LCos, LOpacity, Angle, TimeElapsed, ShrinkAlpha: Single;
  MinRadius, MaxRadius, Thickness, TopAdjust: Single;
begin
  if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem then
  begin
    // ZuBy ***
    if FHorizontal then
      TopAdjust := FSearchEdit.Width
    else
      TopAdjust := FSearchEdit.Height;
    // *** ZuBy
  end
  else
    TopAdjust := 0;

  LineCount := GetPullRefreshIndicatorSteps;
  if LineCount < 1 then
    exit;

  LOpacity := GetPullRefreshIndicatorAlpha;
  if LOpacity <= 0 then
    exit;

  // ZuBy ***
  if FHorizontal then
  begin
    Center.X := Height / 2;
    Center.Y := TopAdjust + PullRefreshIndicatorStrengthStart +
      IndicatorMaxRadius;
  end
  else
  begin
    Center.X := Width / 2;
    Center.Y := TopAdjust + PullRefreshIndicatorStrengthStart +
      IndicatorMaxRadius;
  end;
  // *** ZuBy

  MinRadius := IndicatorMinRadius;
  MaxRadius := IndicatorMaxRadius;
  Thickness := IndicatorThickness;

  if FPullRefreshAnimation = TPullRefreshAnimation.Playing then
    TimeElapsed := Abs(FTimerService.GetTick - FPullRefreshAnimationStartTime)
  else
    TimeElapsed := 0;

  if LOpacity <= IndicatorDisappearFraction then
  begin
    ShrinkAlpha := (1 - IndicatorDisappearFraction) + LOpacity;
    MaxRadius := MaxRadius * ShrinkAlpha;
    MinRadius := MinRadius * ShrinkAlpha;
    Thickness := Thickness * ShrinkAlpha;
  end;

  Stroke := TStrokeBrush.Create(TBrushKind.Solid, FStyleResources.PullRefreshIndicatorColor);
  try
    Stroke.Thickness := Thickness;

    for I := 0 to LineCount - 1 do
    begin
      Angle := ((I * PiMulTwo) / PullRefreshIndicatorMaxSteps) + TimeElapsed * IndicatorRotation - PiByTwo;

      if not SameValue(FPullRefreshAnimationStartTime, FPullRefreshAnimationStopTime, TEpsilon.Vector) then
        Angle := Angle + Abs(FTimerService.GetTick - FPullRefreshAnimationStopTime) * IndicatorRotation * 2;

      SinCos(Angle, LSin, LCos);
      P1.X := Center.X + LCos * MinRadius;
      P1.Y := Center.Y + LSin * MinRadius;
      P2.X := Center.X + LCos * MaxRadius;
      P2.Y := Center.Y + LSin * MaxRadius;
      // ZuBy ***
      if FHorizontal then
        ACanvas.DrawLine(P2, P1, AOpacity * LOpacity, Stroke)
      else
        ACanvas.DrawLine(P1, P2, AOpacity * LOpacity, Stroke);
      // *** ZuBy
    end;
  finally
    Stroke.Free;
  end;
end;

function TListViewBase.GetPullRefreshStrokeWidth: Single;
const
  StrokeCollapseSpeed1 = 4;
  StrokeCollapseSpeed2 = 256;
  StrokeCollapsePower = 0.75;
  StrokeGrowthSpeed = 0.25;
begin
  if FPullRefreshAnimation = TPullRefreshAnimation.Playing then
  begin
    // ZuBy ***
    if FHorizontal then
      Result := Min
        (Power(Abs(FTimerService.GetTick - FPullRefreshAnimationStartTime) *
        StrokeCollapseSpeed1, StrokeCollapsePower) *
        StrokeCollapseSpeed2, Height)
    else
      Result := Min
        (Power(Abs(FTimerService.GetTick - FPullRefreshAnimationStartTime) *
        StrokeCollapseSpeed1, StrokeCollapsePower) *
        StrokeCollapseSpeed2, Width);
    // *** ZuBy
  end
  else
  begin
    // ZuBy ***
    if FHorizontal then
      Result := Min
        (Sqr(Max(GetPullRefreshStrength - PullRefreshIndicatorStrengthStart,
        0) * StrokeGrowthSpeed), Height)
    else
      Result := Min
        (Sqr(Max(GetPullRefreshStrength - PullRefreshIndicatorStrengthStart,
        0) * StrokeGrowthSpeed), Width);
    // *** ZuBy
  end;
end;

procedure TListViewBase.PaintPullRefreshStroke(const ACanvas: TCanvas; const AStrength, AOpacity: Single);
const
  DefaultStrokeThickness = 2.5;
var
  StrokeBrush: TBrush;
  StrokeLength, TopAdjust: Single;
begin
  if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem then
  begin
    // ZuBy ***
    if FHorizontal then
      TopAdjust := FSearchEdit.Width
    else
      TopAdjust := FSearchEdit.Height;
    // *** ZuBy
  end
  else
    TopAdjust := 0;

  StrokeBrush := TBrush.Create(TBrushKind.Gradient, FStyleResources.PullRefreshStrokeColor);
  try
    StrokeBrush.Kind := TBrushKind.Solid;
    StrokeLength := GetPullRefreshStrokeWidth;

    if FPullRefreshAnimation = TPullRefreshAnimation.Playing then
    begin
      // ZuBy ***
      if FHorizontal then
      begin
        ACanvas.FillRect(TRectF.Create(TopAdjust, 0,
          TopAdjust + DefaultStrokeThickness, (Height - StrokeLength) / 2), 0,
          0, AllCorners, AOpacity, StrokeBrush);
        ACanvas.FillRect(TRectF.Create(TopAdjust, (Height + StrokeLength) / 2,
          TopAdjust + DefaultStrokeThickness, Height), 0, 0, AllCorners,
          AOpacity, StrokeBrush);
      end
      else
      begin
        ACanvas.FillRect(TRectF.Create(0, TopAdjust, (Width - StrokeLength) / 2,
          TopAdjust + DefaultStrokeThickness), 0, 0, AllCorners, AOpacity,
          StrokeBrush);
        ACanvas.FillRect(TRectF.Create((Width + StrokeLength) / 2, TopAdjust,
          Width, TopAdjust + DefaultStrokeThickness), 0, 0, AllCorners,
          AOpacity, StrokeBrush);
      end;
      // *** ZuBy
    end;

    if FPullRefreshAnimation = TPullRefreshAnimation.NotPlaying then
    begin
      // ZuBy ***
      if FHorizontal then
        ACanvas.FillRect(TRectF.Create(TopAdjust, (Height - StrokeLength) / 2,
          TopAdjust + DefaultStrokeThickness, (Height + StrokeLength) / 2), 0,
          0, AllCorners, AOpacity, StrokeBrush)
      else
        ACanvas.FillRect(TRectF.Create((Width - StrokeLength) / 2, TopAdjust,
          (Width + StrokeLength) / 2, TopAdjust + DefaultStrokeThickness), 0, 0,
          AllCorners, AOpacity, StrokeBrush);
      // *** ZuBy
    end;
  finally
    StrokeBrush.Free;
  end;
end;

procedure TListViewBase.PaintScrollingStretchGlow(const ACanvas: TCanvas; const AIntensity, AOpacity: Single);
var
  TempPoint: TGradientPoint;
  TempColor: TAlphaColor;
  GlowBrush: TBrush;
  GlowDepth: Single;
  GlowRect: TRectF;
begin
  GlowBrush := TBrush.Create(TBrushKind.Gradient, FStyleResources.ScrollingStretchGlowColor);
  try
    GlowBrush.Gradient.Style := TGradientStyle.Radial;
    GlowBrush.Gradient.Points.Clear;
    TempColor := FStyleResources.ScrollingStretchGlowColor;

    TempPoint := TGradientPoint.Create(GlowBrush.Gradient.Points);
    TAlphaColorRec(TempColor).A := 0;
    TempPoint.Color := TempColor;
    TempPoint.Offset := 0;

    TempPoint := TGradientPoint.Create(GlowBrush.Gradient.Points);
    TAlphaColorRec(TempColor).A := 255;
    TempPoint.Color := TempColor;
    TempPoint.Offset := 1;

    GlowDepth := Max((Sqrt(Abs(AIntensity)) - 3) * 3, 0);
    if GlowDepth > TEpsilon.Position then
    begin
      if AIntensity < 0 then
      begin
        // ZuBy ***
        if FHorizontal then
          GlowRect := TRectF.Create(-GlowDepth, -Height / 8, GlowDepth,
            Height + Height / 8)
        else
          GlowRect := TRectF.Create(-Width / 8, -GlowDepth, Width + Width / 8,
            GlowDepth);
        // *** ZuBy
        if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem
        then
          GlowRect.Offset(0, FSearchEdit.Height);
      end
      else
      begin
        // ZuBy ***
        if FHorizontal then
          GlowRect := TRectF.Create(Width - GlowDepth, -Height / 8,
            Width + GlowDepth, Height + Height / 8)
        else
          GlowRect := TRectF.Create(-Width / 8, Height - GlowDepth,
            Width + Width / 8, Height + GlowDepth);
        // *** ZuBy
      end;

      ACanvas.FillEllipse(GlowRect, AOpacity, GlowBrush);
    end;
  finally
    GlowBrush.Free;
  end;
end;

procedure TListViewBase.UpdatePullRefreshState;
var
  Trigger: Boolean;
begin
  if FPullRefreshTriggered and (GetPullRefreshStrength < 1) and
    (FPullRefreshAnimation <> TPullRefreshAnimation.Playing) then
  begin
    FPullRefreshTriggered := False;
    FPullRefreshAnimation := TPullRefreshAnimation.NotPlaying;
    FPullRefreshAnimationStopTime := FPullRefreshAnimationStartTime;
  end
  else if not FPullRefreshTriggered then
  begin
    if TListingTransitionFeature.PullToRefresh in FListingService.GetTransitionFeatures then
      Trigger := GetPullRefreshIndicatorSteps >= PullRefreshIndicatorMaxSteps
    else
    begin
      // ZuBy ***
      if FHorizontal then
        Trigger := GetPullRefreshStrokeWidth >= Height
      else
        Trigger := GetPullRefreshStrokeWidth >= Width;
      // *** ZuBy
    end;

    if Trigger then
    begin
      FPullRefreshTriggered := True;
      StartPullRefreshAnimation;

      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnPullRefresh) then
            FOnPullRefresh(Self);
        end);
    end;
  end;
  if FPullRefreshTriggered and (FPullRefreshAnimation = TPullRefreshAnimation.Playing) and
    SameValue(FPullRefreshAnimationStartTime, FPullRefreshAnimationStopTime, TEpsilon.Vector) and (FAniCalc <> nil) and
    (not FAniCalc.Down) then
    FPullRefreshAnimationStopTime := FTimerService.GetTick;
end;

procedure TListViewBase.UpdateScrollStretchStrength(const NewValue: Single);
begin
  if not SameValue(FScrollStretchStrength, NewValue, TEpsilon.Position) then
  begin
    FScrollStretchStrength := NewValue;
    ScrollStretchChanged;
  end;
end;

procedure TListViewBase.ScrollStretchChanged;
begin
  if FPullToRefresh then
    UpdatePullRefreshState;

  if FPullToRefresh or HasScrollingStretchGlow then
    Invalidate;
end;

procedure TListViewBase.UpdateDeleteButtonLayout;
var
  RelRect: TRectF;
begin
  if (Adapter.Count < 1) or (FDeleteLayout = nil) or ((FDeleteButtonIndex = -1) and
    (FPrevDeleteButtonIndex = -1)) then
    exit;

  if (FListingService <> nil) and (TListingTransitionFeature.DeleteButtonSlide in FListingService.GetTransitionFeatures) then
  begin
    FDeleteLayout.Width := DefaultDeleteButtonWidth * FDeleteModeTransitionAlpha;
    FDeleteButton.Opacity := 1;
  end
  else
  begin
    if FDeleteModeTransitionAlpha > 0 then
      FDeleteLayout.Width := DefaultDeleteButtonWidth
    else
      FDeleteLayout.Width := 0;

    FDeleteButton.Opacity := 0.5 + (FDeleteModeTransitionAlpha / 2);
  end;

  FDeleteLayout.Height := GetItemHeight(FDeleteButtonIndex);

  // ZuBy ***
  if FHorizontal then
    FDeleteLayout.Position.X := RelRect.Right - FDeleteLayout.Width
  else
    FDeleteLayout.Position.X := Width - FDeleteLayout.Width;
  // *** ZuBy

  if FDeleteButtonIndex = -1 then
    RelRect := GetItemRelRect(FPrevDeleteButtonIndex, LocalRect)
  else
    RelRect := GetItemRelRect(FDeleteButtonIndex, LocalRect);

  FDeleteLayout.Position.Y :=
    (RelRect.Top + RelRect.Bottom - FDeleteLayout.Height) / 2;
end;

procedure TListViewBase.DeleteButtonClicked(Sender: TObject);
begin
  if DeleteItem(FDeleteButtonIndex) then
  begin
    FSelectionAlphas.Remove(FDeleteButtonIndex);
    SetDeleteButtonIndex(-1);
    ResetDeleteModeAnimation;
  end;
end;

procedure TListViewBase.ProceedDeleteItem;
var
  Editor: IListViewEditor;
begin
  if (FDeleteButtonIndex = -1) or not Supports(Adapter, IListViewEditor, Editor) then
    exit;

  if (FItemIndex <> -1) and (FItemIndex >= FDeleteButtonIndex) then
    SetItemIndex(-1);

  Editor.Delete(FDeleteButtonIndex);

  if Assigned(FOnDeleteItem) then
    FOnDeleteItem(Self, FDeleteButtonIndex);

  SetDeleteButtonIndex(-1);

  ResetDeleteModeAnimation;
end;

procedure TListViewBase.SetDeleteButtonIndex(const NewItemIndex: Integer);
begin
  if FDeleteButtonIndex <> NewItemIndex then
  begin
    if FTransitionType = TTransitionType.DeleteMode then
      ResetDeleteModeAnimation;

    FPrevDeleteButtonIndex := FDeleteButtonIndex;
    FDeleteButtonIndex := NewItemIndex;

    InitDeleteModeAnimation;

    if Assigned(FOnDeleteChange) then
      FOnDeleteChange(Self, (FDeleteButtonIndex = -1) and (not FEditMode));

    if FSearchEdit <> nil then
      FSearchEdit.Enabled := (FDeleteButtonIndex = -1) and (not FEditMode);
  end;
end;

function TListViewBase.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True
  else if ID = TObserverMapping.PositionLinkID then
    Result := True
  else if ID = TObserverMapping.ControlValueID then
    Result := True;
end;

procedure TListViewBase.ObserversBeforeSelection(out LAllowSelection: Boolean);
begin
  LAllowSelection := True;
  if Observers.IsObserving(TObserverMapping.EditLinkID) and not TLinkObservers.EditLinkEdit(Observers) then
    LAllowSelection := False;
  if LAllowSelection then
    TLinkObservers.PositionLinkPosChanging(Observers);
end;

procedure TListViewBase.DoListItemChange(const AListItem: TListItem);
begin
  if Assigned(FOnItemChange) then
    FOnItemChange(Self, AListItem);
end;

procedure TListViewBase.DoListItemClick(const AListItem: TListItem);
begin
  if Assigned(FOnListItemClick) then
    FOnListItemClick(Self, AListItem);
end;

procedure TListViewBase.DoUpdateItemView(const AListItem: TListItem);
begin
  if Assigned(FOnUpdateItemView) then
    FOnUpdateItemView(Self, AListItem);
end;

procedure TListViewBase.DoUpdatingItemView(const AListItem: TListItem; var AHandled: Boolean);
begin
  if Assigned(FOnUpdatingItemView) then
    FOnUpdatingItemView(Self, AListItem, AHandled);
end;

procedure TListViewBase.DoUpdateScrollViewPos(const Value: Single);
begin
end;

procedure TListViewBase.ReadCanSwipeDelete(Reader: TReader);
begin
  CanSwipeDelete := Reader.ReadBoolean;
end;

procedure TListViewBase.ReadIsSearchVisible(Reader: TReader);
begin
  SearchVisible := Reader.ReadBoolean;
end;

procedure TListViewBase.ReadIsSearchAlwaysOnTop(Reader: TReader);
begin
  SearchAlwaysOnTop := Reader.ReadBoolean;
end;

procedure TListViewBase.ReadEditModeOptions(Reader: TReader);
begin
  TOpenReader(Reader).ReadSet(TypeInfo(TEditModeOptions));
end;

procedure TListViewBase.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('CanSwypeDelete', ReadCanSwipeDelete, nil, False);
  Filer.DefineProperty('DeleteButtonEnabled', ReadCanSwipeDelete, nil, False);
  Filer.DefineProperty('EnabledDeleteButton', ReadCanSwipeDelete, nil, False);
  Filer.DefineProperty('IsSearchVisible', ReadIsSearchVisible, nil, False);
  Filer.DefineProperty('IsSearchAlwaysOnTop', ReadIsSearchAlwaysOnTop, nil, False);
  Filer.DefineProperty('EditModeOptions', ReadEditModeOptions, nil, False);
end;

procedure TListViewBase.DoRealign;
begin
  inherited;
  Repaint;
end;

procedure TListViewBase.DoExit;
begin
  inherited;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkUpdate(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueUpdate(Observers);
end;

procedure TListViewBase.UpdateScrollViewPos(const Value: Single);
begin
  if (not SameValue(FScrollViewPos, Value, TEpsilon.Vector) and FCanScroll) then
  // sinuke
  begin
    FScrollViewPos := Value;
    DoUpdateScrollViewPos(Value);
    if Assigned(FOnScrollViewChange) then
      FOnScrollViewChange(Self);
    // ZuBy ***
    if FScrollViewPos >= GetMaxScrollViewPos then
    begin
      if Assigned(FOnScrollEnd) then
        FOnScrollEnd(Self);
    end;
    // *** ZuBy
  end;
  RecalcTopViewItemIndex;
end;

procedure TListViewBase.UpdateSearchEditPos;
var
  NewPos: Single;
begin
  if FSearchEdit <> nil then
  begin
    if HasSearchAsItem then
      NewPos := Max(0, FSearchEdit.Height - FScrollViewPos) - FSearchEdit.Height
    else
      NewPos := 0;

    if not SameValue(FSearchEdit.Position.Y, NewPos, TEpsilon.Position) then
    begin
      FSearchEdit.Position.Y := NewPos;
      Invalidate;
    end;
  end;
end;

function TListViewBase.GetMaxScrollViewPos: Integer;
begin
  // ZuBy ***
  if FHorizontal then
    Result := Max(Trunc(FSideSpace * 2 + FMaxKnownHeight - LocalRect.Width), 0)
  else
    Result := Max(Trunc(FSideSpace * 2 + FMaxKnownHeight -
      LocalRect.Height), 0);
  // *** ZuBy
end;

function TListViewBase.GetMessageSender: TMessageSender;
begin
  if FMessageSender = nil then
  begin
    FMessageSender := TMessageSender.Create;
    FMessageSender.Receiver := Self;
  end;
  Result := FMessageSender;
end;

procedure TListViewBase.SetScrollViewPos(const Value: Single);
var
  NewValue, MaxValue: Single;
begin
  NewValue := Value;

  if NewValue < 0 then
    NewValue := 0;

  MaxValue := GetMaxScrollViewPos;
  if NewValue > MaxValue then
    NewValue := MaxValue;

  UpdateScrollViewPos(NewValue);
  DoSetScrollViewPos(NewValue);
end;

procedure TListViewBase.SetSelected(const Value: TListItem);
begin
  if Value = nil then
    ItemIndex := -1
  else
    ItemIndex := Adapter.IndexOf(Value);
end;

procedure TListViewBase.SetSelectionCrossfade(const Value: Boolean);
begin
  if FSelectionCrossfade <> Value then
  begin
    FSelectionCrossfade := Value;
    Invalidate;
  end;
end;

procedure TListViewBase.SetPullToRefresh(const Value: Boolean);
begin
  if FPullToRefresh <> Value then
  begin
    FPullToRefresh := Value;
    if FAniCalc <> nil then
      FAniCalc.BoundsAnimation := HasPhysicsStretchyScrolling;
    RecreateNativePresentation;
  end;
end;

procedure TListViewBase.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TListViewBase.ScrollBarChange(Sender: TObject);
begin
  UpdateScrollViewPos(FScrollBar.Value);
  UpdateSearchEditPos;
  UpdateDeleteButtonLayout;
end;

procedure TListViewBase.AniCalcChange(Sender: TObject);
var
  NewViewPos, MaxScrollViewPos: Single;
begin
  // ZuBy ***
  if FHorizontal then
    NewViewPos := FAniCalc.ViewportPosition.X
  else
    NewViewPos := FAniCalc.ViewportPosition.Y;
  // *** ZuBy
  MaxScrollViewPos := GetMaxScrollViewPos;

  if NewViewPos < 0 then
    UpdateScrollStretchStrength(NewViewPos)
  else if NewViewPos > MaxScrollViewPos then
    UpdateScrollStretchStrength(NewViewPos - MaxScrollViewPos)
  else
    UpdateScrollStretchStrength(0);

  if not HasStretchyScrolling then
    NewViewPos := EnsureRange(NewViewPos, 0, MaxScrollViewPos);

  if (not SameValue(NewViewPos, FScrollViewPos, TEpsilon.Vector)) and
    (TStateFlag.NeedsScrollBarDisplay in FStateFlags) then
  begin
    FScrollBar.StopPropertyAnimation('Opacity');
    FScrollBar.Opacity := 1;

    Exclude(FStateFlags, TStateFlag.NeedsScrollBarDisplay);
  end;

  if TStateFlag.ScrollingActive in FStateFlags then
  begin
    UpdateScrollViewPos(NewViewPos);
    UpdateSearchEditPos;
    UpdateDeleteButtonLayout;
    UpdateScrollBar;
  end;
end;

procedure TListViewBase.AniCalcStart(Sender: TObject);
begin
  if IsRunningOnDesktop then
    DisableHitTestForControl(FScrollBar);

  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);

  // ZuBy ***
  if FShowScrollBar then
    FStateFlags := FStateFlags + [TStateFlag.NeedsScrollBarDisplay,
      TStateFlag.ScrollingActive]
  else
    FStateFlags := FStateFlags + [TStateFlag.ScrollingActive];
  // *** ZuBy
end;

procedure TListViewBase.AniCalcStop(Sender: TObject);
var
  ScrollPixelAlign: Boolean;
begin
  ScrollPixelAlign := TStateFlag.ScrollingActive in FStateFlags;
  Exclude(FStateFlags, TStateFlag.ScrollingActive);
  TAnimator.AnimateFloat(FScrollBar, 'Opacity', 0, 0.2);

  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);

  if ScrollPixelAlign and (FScrollScale > TEpsilon.Scale) then
  begin
    // ZuBy ***
    if FAutoPositionToItem then
      ScrollTo(TAppearanceListView(Self).getFirstVisibleItemIndex)
    else
      SetScrollViewPos(Round(FScrollViewPos * FScrollScale) / FScrollScale);
    // *** ZuBy
  end;
end;

procedure TListViewBase.UpdateScrollBar;
var
  LocalHeight, ViewSize: Single;
begin
  // ZuBy ***
  if FHorizontal then
    LocalHeight := LocalRect.Width
  else
    LocalHeight := LocalRect.Height;
  // *** ZuBy

  if FScrollViewPos < 0 then
    ViewSize := LocalHeight + FScrollViewPos
  else if FScrollViewPos > FMaxKnownHeight - LocalHeight then
    ViewSize := LocalHeight - (FScrollViewPos - (FMaxKnownHeight - LocalHeight))
  else
    ViewSize := LocalHeight;

  FScrollBar.BeginUpdate;
  try
    FScrollBar.Max := FSideSpace * 2 + FMaxKnownHeight;
    FScrollBar.SmallChange := Adapter.GetDefaultViewHeight * 0.5;
    if not (csDesigning in ComponentState) then  // Don't show at design time
      FScrollBar.Visible := ((FMaxKnownHeight > LocalHeight) or
        (HasTouchTracking and (TStateFlag.ScrollingActive in FStateFlags))) and
        FShowScrollBar // ZuBy
    else
      FScrollBar.Visible := False;
    FScrollBar.Value := FScrollViewPos;
    FScrollBar.ViewportSize := ViewSize;
  finally
    FScrollBar.EndUpdate;
  end;
  RecalcTopViewItemIndex; // ZuBy
end;

procedure TListViewBase.UpdateScrollingLimits;
begin
  if not IsUpdating then
    DoUpdateScrollingLimits
  else
    Include(FStateFlags, TStateFlag.NeedsScrollingLimitsUpdate);
end;

procedure TListViewBase.DoUpdateScrollingLimits;
var
  Targets: array of TAniCalculations.TTarget;
begin
  if FAniCalc <> nil then
  begin
    SetLength(Targets, 2);

    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    // ZuBy ***
    if FHorizontal then
      Targets[1].Point := TPointD.Create
        (Max(FSideSpace * 2 + FMaxKnownHeight - LocalRect.Width, 0), 0)
    else
      Targets[1].Point := TPointD.Create(0,
        Max(FSideSpace * 2 + FMaxKnownHeight - LocalRect.Height, 0));
    // *** ZuBy

    FAniCalc.SetTargets(Targets);
  end;

  if not HasTouchTracking then
    UpdateScrollBar;
end;

procedure TListViewBase.GetNumberOfRenderingPasses(const StartItem, EndItem: Integer; var Passes, Subpasses: Integer);
var
  I, J: Integer;
  ListItem: TListItem;
begin
  Passes := 0;
  Subpasses := 1;
  for J := StartItem to EndItem do
  begin
    ListItem := Adapter[J];
    ListItem.CreateObjects;
    Passes := Max(Passes, ListItem.Count);
    for I := 0 to ListItem.Count - 1 do
      Subpasses := Max(Subpasses, ListItem.View[I].GetRenderPassCount);
  end;
end;

function TListViewBase.GetFinalItemSpaces(const ForceIncludeScrollBar: Boolean): TRectF;
begin
  Result := FItemSpaces.Rect;

  // ZuBy ***
  if FHorizontal then
  begin
    if (FScrollBar <> nil) and (not HasTouchTracking) and
      (ForceIncludeScrollBar or FScrollBar.Visible) then
      Result.Bottom := Result.Bottom - FScrollBar.Height;

    // fix SearchVisible
    if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem then
      Result.Top := Result.Top + FSearchEdit.Height;
  end
  else
  begin
    if (FScrollBar <> nil) and (not HasTouchTracking) and
      (ForceIncludeScrollBar or FScrollBar.Visible) then
      Result.Right := Result.Right + FScrollBar.Width;
  end;
  // *** ZuBy
end;

function TListViewBase.GetFinalItemSize(const ForceIncludeScrollBar: Boolean): TSizeF;
var
  FinalItemSpaces: TRectF;
begin
  FinalItemSpaces := GetFinalItemSpaces(ForceIncludeScrollBar);
  Result := TSizeF.Create(Width - FinalItemSpaces.Left - FinalItemSpaces.Right,
    Height - FinalItemSpaces.Top - FinalItemSpaces.Bottom);
end;

function TListViewBase.GetItemRelRect(const Index: Integer; const LocRect: TRectF;
  const SideSpace: Integer = 0): TRectF;
begin
  // ZuBy ***
  if FHorizontal then
  begin
    Result := RectF(LocRect.Left + FSideSpace + FHeightSums[Index] -
      FScrollViewPos, LocRect.Top + FSideSpace + SideSpace,
      GetItemHeight(Index) - FSideSpace,
      LocRect.Height - ((SideSpace + FSideSpace) * 2));

    if (FScrollBar <> nil) and (not HasTouchTracking) and FScrollBar.Visible
    then
      Result.Bottom := Result.Bottom - FScrollBar.Height;

    // fix SearchVisible
    if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem then
      Result.Top := Result.Top + FSearchEdit.Height;
  end
  else
  begin
    Result := RectF(LocRect.Left + FSideSpace + SideSpace,
      FItemBottomOffset + LocRect.Top + FSideSpace + FHeightSums[Index] -
      FScrollViewPos, LocRect.Width - ((SideSpace + FSideSpace) * 2),
      GetItemHeight(Index) - FItemBottomOffset);

    if (FScrollBar <> nil) and (not HasTouchTracking) and FScrollBar.Visible
    then
      Result.Right := Result.Right - FScrollBar.Width;
  end;
  // *** ZuBy
end;

function TListViewBase.GetScene: IScene;
begin
  Result := Scene;
end;

function TListViewBase.GetSelected: TListItem;
begin
  if (FItemIndex >= 0) and (FItemIndex < Adapter.Count) then
    Result := Adapter[FItemIndex]
  else
    Result := nil;
end;

function TListViewBase.GetStyleResources: TListItemStyleResources;
begin
  ApplyStyleLookup;
  Result := FStyleResources;
end;

function TListViewBase.StyleResourcesNeedUpdate: Boolean;
begin
  Result := FUpdatingStyleResources;
end;

function TListViewBase.GetItemGroupSeparators(const Index: Integer): Integer;
var
  EndIndex: Integer;
  Prev, Next: TListItem;
begin
  Result := 0;

  EndIndex := Adapter.Count - 1;
  if (Index < 0) or (Index > EndIndex) then
    exit;
  if (Index = 0) and (not FShowFirstSeparator) then
    exit;
  if (Index = EndIndex) and (not FShowLastSeparator) then
    exit;

  Prev := nil;
  Next := nil;
  if Index > 0 then
    Prev := Adapter[Index - 1];
  if Index < EndIndex then
    Next := Adapter[Index + 1];

  if (Prev <> nil) and (Next <> nil) and (Prev.Count > 0) and (Next.Count > 0) then
    exit;

  if (Index = 0) or ((Prev.Count < 1) and (Prev.Purpose = TListItemPurpose.None)) then
    Result := Result or ItemSeparatorTop;

  if (Index >= EndIndex) or ((Next.Count < 1) and (Next.Purpose = TListItemPurpose.None)) then
    Result := Result or ItemSeparatorBottom;
end;

function TListViewBase.GetItemHeight(const Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= Adapter.Count) then
    Result := 0
  else
  begin
    Result := Adapter[Index].Height;
    if Result < 1 then
      Result := Adapter.GetDefaultViewHeight;
  end;
end;

function TListViewBase.GetSeparatorLineHeight: Single;
begin
  if FScrollScale > TEpsilon.Scale then
    Result := 1 / FScrollScale
  else
    Result := 1;

  if FScrollScale >= 2 then
    Result := Result * 2;
end;

function TListViewBase.AlignValueToPixel(const Value: Single): Single;
begin
  if FScrollScale > TEpsilon.Scale then
    Result := Int(Value * FScrollScale) / FScrollScale
  else
    Result := Value;
end;

procedure TListViewBase.DrawItemsFill(const StartItem, EndItem: Integer; const LocRect: TRectF; const Opacity: Single;
  const HeaderIndex: Integer);
var
  I, Sep, AltIndex: Integer;
  DrawRect, DrawSubRect, SepRect: TRectF;
  ListItem: TListItem;
  SepHeight: Single;
  HeaderBefore: Boolean;
  DrawPanel: TRectF;
  J, RowColumns: Integer;
begin
  SepHeight := GetSeparatorLineHeight;

  for I := StartItem to EndItem do
    if I <> HeaderIndex then
    begin
      ListItem := Adapter[I];
      HeaderBefore := (I > 0) and (Adapter[I - 1].Purpose <> TListItemPurpose.None);
      if (ListItem <> nil) and ((ListItem.Count > 0) or HeaderBefore) then
      begin
        DrawRect := GetItemRelRect(I, LocRect);

        if ListItem.Purpose = TListItemPurpose.None then
        begin
          // ZuBy ***
          if FItemBoxLight <> nil then
          begin
            if FAutoColumns then
            begin
              RowColumns := Min(TListViewItem(ListItem).Tag, FColumns);
              for J := 1 to RowColumns { FColumns } do
              begin
                DrawPanel := RectF(((J * FColumnWidth) - FColumnWidth) +
                  (FMarg * J), DrawRect.Top, FColumnWidth, DrawRect.Height);
                FItemBoxLight.DrawToCanvas(Canvas, DrawPanel, Opacity);
              end;
            end
            else
              FItemBoxLight.DrawToCanvas(Canvas, DrawRect, Opacity);
          end
          else
          begin
            // ZuBy ***
            if not FTransparentItems then
            begin
              FBrush.Color := FItemStyleFillColor;

              if ListItem.HeaderRef <> -1 then
                AltIndex := Max((I - ListItem.HeaderRef) - 1, 0)
              else
                AltIndex := I;

              if FAlternatingColors and (AltIndex mod 2 = 1) then
                FBrush.Color := FItemStyleFillAltColor;

              // ZuBy ***
              if TListViewItem(ListItem).Data['aUseCustomColor'].AsBoolean then
                FBrush.Color := TListViewItem(ListItem).Data['aCustomColor']
                  .AsInteger;
              // *** ZuBy
            end
            else
              FBrush.Kind := TBrushKind.None;
            Canvas.FillRect(DrawRect, 0, 0, AllCorners, Opacity, FBrush);
            FBrush.Kind := FBrush.DefaultKind;
            // *** ZuBy
          end;
          // *** ZuBy
        end;

        Sep := GetItemGroupSeparators(I);
        // ZuBy ***
        if not FTransparentSeparator then
          FBrush.Color := FItemStyleFrameColor
        else
          FBrush.Kind := TBrushKind.None;
        // *** ZuBy

        if (Sep and ItemSeparatorTop > 0) and (ListItem.Purpose = TListItemPurpose.None) then
        begin
          // ZuBy ***
          if FHorizontal then
          begin
            SepRect.Left := AlignValueToPixel(DrawRect.Left);
            SepRect.Right := SepRect.Right + 1;
            SepRect.Top := DrawRect.Top + FSeparatorLeftOffset;
            SepRect.Bottom := DrawRect.Bottom - FSeparatorRightOffset;
          end
          else
          begin
            SepRect.Left := DrawRect.Left + FSeparatorLeftOffset;
            SepRect.Right := DrawRect.Right - FSeparatorRightOffset;
            SepRect.Top := AlignValueToPixel(DrawRect.Top);
            SepRect.Bottom := SepRect.Top + SepHeight;
          end;
          // *** ZuBy

          Canvas.FillRect(SepRect, 0, 0, AllCorners, Opacity, FBrush);
          FBrush.Kind := FBrush.DefaultKind;
        end;
      end;

      if (not((I >= Adapter.Count - 1) and (not FShowLastSeparator))) and
        (ListItem.Purpose = TListItemPurpose.None) and
        ((I >= Adapter.Count - 1) or
        (Adapter[I + 1].Purpose = TListItemPurpose.None)) then
      begin
        // ZuBy ***
        if not FTransparentSeparator then
          FBrush.Color := FItemStyleFrameColor
        else
          FBrush.Kind := TBrushKind.None;
        // *** ZuBy

        // ZuBy ***
        if FHorizontal then
        begin
          SepRect.Left := AlignValueToPixel(DrawRect.Left);
          SepRect.Right := SepRect.Left + 1;
          SepRect.Top := (DrawRect.Top - 1) + FSeparatorLeftOffset;
          SepRect.Bottom := (DrawRect.Bottom + 1) - FSeparatorRightOffset;
        end
        else
        begin
          SepRect.Left := (DrawRect.Left - 1) + FSeparatorLeftOffset;
          SepRect.Right := (DrawRect.Right + 1) - FSeparatorRightOffset;
          SepRect.Top := AlignValueToPixel(DrawRect.Bottom - SepHeight);
          SepRect.Bottom := SepRect.Top + SepHeight;
        end;
        // *** ZuBy

        Canvas.FillRect(SepRect, 0, 0, AllCorners, Opacity, FBrush);
        FBrush.Kind := FBrush.DefaultKind;
      end;
    end;

  for I := StartItem to EndItem do
    if I <> HeaderIndex then
    begin
      ListItem := Adapter[I];
      HeaderBefore := (I > 0) and
        (Adapter[I - 1].Purpose <> TListItemPurpose.None);

      if (ListItem <> nil) and ((ListItem.Count > 0) or HeaderBefore) then
      begin
        DrawRect := GetItemRelRect(I, LocRect);

        if ListItem.Purpose <> TListItemPurpose.None then
        begin
          DrawSubRect := DrawRect;

          if I = 0 then
            DrawSubRect.Top := DrawSubRect.Top + 1;

          // ZuBy ***
          if not FTransparentHeaders then
          begin
            if FHeaderStyleImage <> nil then
              FHeaderStyleImage.DrawToCanvas(Canvas, DrawSubRect, Opacity)
            else
            begin
              FBrush.Color := FHeaderStyleColor;
              Canvas.FillRect(DrawSubRect, 0, 0, AllCorners, Opacity, FBrush);
            end;
          end
          else
            FBrush.Kind := TBrushKind.None;
          // *** ZuBy
        end;
      end;
    end;
end;

procedure TListViewBase.DrawIndexFill(const AIndex: Integer; const LocRect: TRectF; const Opacity: Single);
var
  DrawRect, DrawPanel: TRectF;
  SepHeight, pW: Single;
  J, RowColumns: Integer;
begin
  DrawRect := GetItemRelRect(AIndex, LocRect);
  SepHeight := GetSeparatorLineHeight;

  if (AIndex >= Adapter.Count - 1) or
    (Adapter[AIndex + 1].Purpose = TListItemPurpose.None) then
    DrawRect.Bottom := DrawRect.Bottom - SepHeight;

{$IFDEF MSWINDOWS}
  // The selection seems to be broken on Windows (looks ugly, needs fixing). Meanwhile, attempt a temporal fix.
  DrawRect.Inflate(-2, -2);
{$ENDIF}
  if FSelectionStyleImage <> nil then
  begin
    DrawRect.Top := AlignValueToPixel(DrawRect.Top - SepHeight) + SepHeight;
    DrawRect.Bottom := AlignValueToPixel(DrawRect.Bottom) + 2;
    // ZuBy ***
    if FItemBoxLight <> nil then
      DrawRect.Top := DrawRect.Top + 1;
    DrawRect.Bottom := DrawRect.Bottom - 2;
    if FAutoColumns then
    begin
      RowColumns := Min(TListViewItem(Adapter[AIndex]).Tag, FColumns);
      for J := 1 to RowColumns do
      begin
        pW := (J * FColumnWidth) - FColumnWidth;
        if InRange(FClickEventMousePos.X, (pW + (J * FMarg)),
          (pW + FColumnWidth) + (J * FMarg)) then
          break;
      end;

      if (J > RowColumns) then
        exit;

      DrawPanel := RectF(((J * FColumnWidth) - FColumnWidth) + (FMarg * J),
        DrawRect.Top, FColumnWidth, DrawRect.Height);
      FSelectionStyleImage.DrawToCanvas(Canvas, DrawPanel, Opacity);
    end
    else
      FSelectionStyleImage.DrawToCanvas(Canvas, DrawRect, Opacity);
    // *** ZuBy
  end
  else
  begin
    FBrush.Color := FSelectionStyleColor;
    // sinuke ***
    DrawRect.Top := AlignValueToPixel(DrawRect.Top - SepHeight) + SepHeight;
    DrawRect.Bottom := AlignValueToPixel(DrawRect.Bottom) + 2;

    if FItemBoxLight <> nil then
      DrawRect.Top := DrawRect.Top + 1;
    DrawRect.Bottom := DrawRect.Bottom - 2;

    if FAutoColumns then
    begin
      RowColumns := Min(TListViewItem(Adapter[AIndex]).Tag, FColumns);
      for J := 1 to RowColumns do
      begin
        pW := (J * FColumnWidth) - FColumnWidth;
        if InRange(FClickEventMousePos.X, (pW + (J * FMarg)),
          (pW + FColumnWidth) + (J * FMarg)) then
          break;
      end;

      if (J > RowColumns) then
        exit;

      DrawPanel := RectF(((J * FColumnWidth) - FColumnWidth) + (FMarg * J),
        DrawRect.Top, FColumnWidth, DrawRect.Height);
      Canvas.FillRect(DrawPanel, 0, 0, AllCorners, Opacity, FBrush);
    end
    else
      Canvas.FillRect(DrawRect, 0, 0, AllCorners, Opacity, FBrush);
    // *** sinuke
  end;
end;

procedure TListViewBase.DrawTouchAnimation(const Index: Integer; const LocRect: TRectF; const Opacity: Single);
var
  R: TRectF;
begin
  if (FTouchAnimationObject <> nil) and (ItemIndex >= 0) then
  begin
    R := GetItemRelRect(Index, LocRect);
    FTouchAnimationObject.TouchAnimation.DrawTouchAnimation(Canvas, R);
  end;
end;

function TListViewBase.GetHeaderRelRect(const StartItem, HeaderIndex: Integer; const LocRect: TRectF;
  const SideSpace: Integer): TRectF;
var
  LimitRect: TRectF;
begin
  Result := GetItemRelRect(HeaderIndex, LocRect, SideSpace);
  if Result.Top < LocRect.Top then
    Result.Offset(0, LocRect.Top - Result.Top);

  if (StartItem < Adapter.Count - 1) and (Adapter[StartItem + 1].HeaderRef <> HeaderIndex) then
  begin
    LimitRect := GetItemRelRect(StartItem, LocRect, SideSpace);

    if Result.Bottom > LimitRect.Bottom then
      Result.Offset(0, LimitRect.Bottom - Result.Bottom);
  end;
end;

procedure TListViewBase.DrawHeaderItem(const LocRect: TRectF; const StartItem, HeaderIndex: Integer;
  const Opacity: Single);
begin
  // ZuBy ***
  if FHeaderStyleImage <> nil then
    FHeaderStyleImage.DrawToCanvas(Canvas, GetHeaderRelRect(StartItem,
      HeaderIndex, LocRect), Opacity)
  else
  begin
    FBrush.Color := FHeaderStyleColor;
    Canvas.FillRect(GetHeaderRelRect(StartItem, HeaderIndex, LocRect), 0, 0,
      AllCorners, Opacity, FBrush);
  end;
  // *** ZuBy
end;

function TListViewBase.GetItemClientRect(const Index: Integer): TRectF;
var
  MarginSize: TPointF;
  FinalItemSpaces: TRectF;
begin
  FinalItemSpaces := GetFinalItemSpaces(False);

  MarginSize.X := FinalItemSpaces.Left + FinalItemSpaces.Right;
  MarginSize.Y := FinalItemSpaces.Top + FinalItemSpaces.Bottom;

  Result.Left := FSideSpace + FinalItemSpaces.Left;
  Result.Top := FSideSpace + FinalItemSpaces.Top;
  Result.Right := Width - (FSideSpace + FinalItemSpaces.Right);
  Result.Bottom := GetItemHeight(Index) - MarginSize.Y;
end;

function TListViewBase.GetEstimatedItemHeight: Single;
begin
  Result := FEstimatedHeights.Item;
end;

function TListViewBase.GetEstimatedHeaderHeight: Single;
begin
  Result := FEstimatedHeights.Header;
end;

function TListViewBase.GetEstimatedFooterHeight: Single;
begin
  Result := FEstimatedHeights.Footer;
end;

procedure TListViewBase.DrawListItems(const AbsOpacity: Single);

  function GetCleanClipRect: TRectF;
  begin
    Result := LocalRect;

    if (FSearchEdit <> nil) and FSearchEdit.Visible and not HasSearchAsItem then
      Result.Top := Result.Top + FSearchEdit.Height;
  end;

const
  DefaultParams: TListItemDrawable.TParams = (AbsoluteOpacity: 1.0; ItemSelectedAlpha: 1.0;
    DeletingUnwantedOpacity: 1.0; ParentAbsoluteRect: (Left:0;Top:0;Right:0;Bottom:0); Images: nil);
  AnimationDeltaEpsilon = 0.01;
var
  I, StartItem, EndItem, MaxHeight, ItemHeaderIndex, SubPassNo: Integer;
  VertMarginHeight, SceneScale: Single;
  PassNo, NumberOfPasses, TopViewIndex, NumberOfSubPasses: Integer;
  MarginSize: TPointF;
  BorderRect, RelRect, LocRect, ClipRect: TRectF;
  State: TCanvasSaveState;
  ListItem: TListItem;
  CurDrawable: TListItemDrawable;
  DrawStates: TListItemDrawStates;
  FinalItemSpaces: TRectF;
  NeedPaintPullRefreshStroke, NeedPaintScrollingStretchGlow: Boolean;
  PullRefreshStrength: Single;
  ItemHeight: Integer;
  MaxItemIndex: Integer;
  Resources: TListItemStyleResources;
  Params: TListItemDrawable.TParams;
  Checkable: IListViewCheckProvider;
begin
  Adapter.CreateNewViews;
  UpdateItemLookups;

  // Precache local rectangle.
  LocRect := LocalRect;
  ClipRect := LocRect;

  PullRefreshStrength := GetPullRefreshStrength;
  Params := DefaultParams;
  Params.Images := GetImages;
  if GetDeleteModeTransitionAlpha > AnimationDeltaEpsilon then
    Params.DeletingUnwantedOpacity := Max(0, 1 - (GetDeleteModeTransitionAlpha * 2));
  Params.ParentAbsoluteRect := AbsoluteRect;


  if FPullToRefresh and ((PullRefreshStrength > 0) or (FPullRefreshAnimation = TPullRefreshAnimation.Playing)) and
    (FListingService <> nil) and (TListingTransitionFeature.PullToRefresh in FListingService.GetTransitionFeatures) then
    PaintPullRefreshIndicator(Canvas, PullRefreshStrength, AbsOpacity);

  if FSearchVisible and (FSearchEdit <> nil) and (FListingService <> nil) then
    if (FSearchAlwaysOnTop or (not (TListingSearchFeature.AsFirstItem in FListingService.GetSearchFeatures))) and
      (TListingSearchFeature.StayOnTop in FListingService.GetSearchFeatures) then
      ClipRect.Top := ClipRect.Top + FSearchEdit.Height
    else
      ClipRect.Top := ClipRect.Top + Max(0, FSearchEdit.Height - FScrollViewPos);

  // Set item clipping.
  State := Canvas.SaveState;
  Canvas.IntersectClipRect(ClipRect);

  // Calculate item height sums.
  // *** ZuBy
  if FHorizontal then
    MaxHeight := Ceil(LocRect.Width + FScrollViewPos)
  else
    MaxHeight := Ceil(LocRect.Height + FScrollViewPos);
  // ZuBy ***

  // Determine starting and final elements that are currently visible.
  TopViewIndex := Trunc(FScrollViewPos - FSideSpace);
  MaxItemIndex := Adapter.Count - 1;
  StartItem := Min(Max(FindItemAbsoluteAt(TopViewIndex), 0), MaxItemIndex);
  EndItem := MaxItemIndex;

  for I := StartItem + 1 to MaxItemIndex - 1 do
    if FHeightSums[I + 1] > MaxHeight then
    begin
      EndItem := I;
      break;
    end;

  if (FListingService <> nil) and (TListingHeaderBehavior.Sticky in FListingService.GetHeaderBehaviors) then
    ItemHeaderIndex := Adapter[StartItem].HeaderRef
  else
    ItemHeaderIndex := -1;

  FinalItemSpaces := GetFinalItemSpaces(False);

  // *** ZuBy
  if FHorizontal then
    VertMarginHeight := FinalItemSpaces.Left + FinalItemSpaces.Right
  else
    VertMarginHeight := FinalItemSpaces.Top + FinalItemSpaces.Bottom;
  // ZuBy ***

  if (TAlphaColorRec(FItemStyleFillColor).A > 0) or (FAlternatingColors and
    (TAlphaColorRec(FItemStyleFillAltColor).A > 0)) then
  begin
    if (TAlphaColorRec(FItemStyleFillColor).A >= 255) and (not FAlternatingColors or
      (TAlphaColorRec(FItemStyleFillAltColor).A >= 255)) and SameValue(AbsOpacity, 1, TEpsilon.Vector) then
    begin
      Canvas.Blending := False;
      try
        DrawItemsFill(StartItem, EndItem, LocRect, 1, ItemHeaderIndex);
      finally
        Canvas.Blending := True;
      end;
    end
    else
      DrawItemsFill(StartItem, EndItem, LocRect, AbsOpacity, ItemHeaderIndex);
  end;

  // Draw regular selection
  if (not FEditMode) and (FItemIndex >= StartItem) and (FItemIndex <= EndItem) and (FSelectionAlphas.Count < 1) and
    CanDisplaySelectionForItem(FItemIndex) then
    DrawIndexFill(FItemIndex, LocRect, AbsOpacity);

  // Edit mode has current item and selected items
  if FEditMode then
  begin
    // Highlight current item (e.g. for keyboard navigation)
    if (FItemIndex >= StartItem) and (FItemIndex <= EndItem) then
      DrawIndexFill(FItemIndex, LocRect, AbsOpacity * 0.5);

    // Highlight items with checkboxes
    if Supports(Adapter, IListViewCheckProvider, Checkable) then
      for I := StartItem to EndItem do
        if Checkable.Checked[I] then
          DrawIndexFill(I, LocRect, AbsOpacity * GetItemSelectionAlpha(I));
  end;

  MarginSize.X := FinalItemSpaces.Left + FinalItemSpaces.Right;
  MarginSize.Y := FinalItemSpaces.Top + FinalItemSpaces.Bottom;

  BorderRect.Left := LocRect.Left + FSideSpace + FinalItemSpaces.Left;
  BorderRect.Top := LocRect.Top + FSideSpace + FinalItemSpaces.Top;
  // ZuBy ***
  if FHorizontal then
  begin
    BorderRect.Right := BorderRect.Left + Adapter.GetDefaultViewHeight -
      MarginSize.X;
    BorderRect.Bottom := LocRect.Bottom + (FSideSpace + FinalItemSpaces.Bottom);
  end
  else
  begin
    BorderRect.Right := LocRect.Right - (FSideSpace + FinalItemSpaces.Right);
    BorderRect.Bottom := BorderRect.Top + Adapter.GetDefaultViewHeight -
      MarginSize.Y;
  end;
  // *** ZuBy

  SceneScale := Canvas.Scale;

  GetNumberOfRenderingPasses(StartItem, EndItem, NumberOfPasses, NumberOfSubPasses);

  DrawTouchAnimation(ItemIndex, LocRect, AbsOpacity);

  Resources := GetStyleResources;

  for PassNo := 0 to NumberOfPasses - 1 do
    for SubPassNo := 0 to NumberOfSubPasses - 1 do
      for I := StartItem to EndItem do
        if I <> ItemHeaderIndex then
        begin
          ListItem := Adapter[I];

          if (ListItem <> nil) and (ListItem.Count > PassNo) then
          begin
            CurDrawable := ListItem.View[PassNo];
            if (CurDrawable = nil) or (not CurDrawable.Visible) then
              Continue;

            DrawStates := [];

            if (FDeleteButtonIndex = I) or (FPrevDeleteButtonIndex = I) then
              Include(DrawStates, TListItemDrawState.Deleting);

            if CanDisplaySelectionForItem(I, ListItem, True, True) and (GetItemSelectionAlpha(I) > TEpsilon.Vector) then
              Include(DrawStates, TListItemDrawState.Selected);

            if (FEditModeTransitionAlpha > 0) and (ListItem.Purpose = TListItemPurpose.None) then
              Include(DrawStates, TListItemDrawState.EditMode);

            // During the first sub-pass the designated areas are calculated.
            if SubPassNo = 0 then
            begin
              if I < MaxItemIndex then
                ItemHeight := FHeightSums[I + 1] - FHeightSums[I]
              else
                ItemHeight := GetItemHeight(I);
              ListItem.WillBePainted;
              CurDrawable.UpdateValuesFromResources(GetStyleResources, ListItem.Purpose);

              // *** ZuBy
              if FHorizontal then
                CurDrawable.CalculateLocalRect
                  ((TRectF.Create(TPointF.Create(BorderRect.Left + FHeightSums
                  [I] - FScrollViewPos, BorderRect.Top),
                  ItemHeight - VertMarginHeight, BorderRect.Height)),
                  SceneScale, DrawStates, ListItem)
              else // *** ZuBy
                CurDrawable.CalculateLocalRect
                  (TRectF.Create(TPointF.Create(BorderRect.Left,
                  BorderRect.Top + FHeightSums[I] - FScrollViewPos),
                  BorderRect.Width, ItemHeight - VertMarginHeight), SceneScale,
                  DrawStates, ListItem);
            end;
            // *** ZuBy

            Params.AbsoluteOpacity := AbsoluteOpacity * CurDrawable.Opacity;
            Params.ItemSelectedAlpha := GetItemSelectionAlpha(ListItem.Index);
            CurDrawable.Render(Canvas, I, DrawStates, Resources, Params,
              SubPassNo);
          end;
        end;

  if ItemHeaderIndex <> -1 then
  begin
    DrawHeaderItem(LocRect, StartItem, ItemHeaderIndex, AbsOpacity);

    for PassNo := 0 to NumberOfPasses - 1 do
      for SubPassNo := 0 to NumberOfSubPasses - 1 do
      begin
        ListItem := Adapter[ItemHeaderIndex];

        if (ListItem <> nil) and (ListItem.Count > PassNo) then
        begin
          CurDrawable := ListItem.View[PassNo];
          if (CurDrawable = nil) or (not CurDrawable.Visible) then
            Continue;

          // During the first sub-pass the designated areas are calculated.
          if SubPassNo = 0 then
          begin
            RelRect := GetHeaderRelRect(StartItem, ItemHeaderIndex, LocRect);

            // *** ZuBy
            if FHorizontal then
            begin
              RelRect.Top := BorderRect.Top;
              RelRect.Bottom := BorderRect.Bottom;
            end
            else
            begin
              RelRect.Left := BorderRect.Left;
              RelRect.Right := BorderRect.Right;
            end; // ZuBy ***
            CurDrawable.UpdateValuesFromResources(GetStyleResources,
              ListItem.Purpose);
            CurDrawable.CalculateLocalRect(RelRect, SceneScale, [], ListItem);
          end;

          Params.AbsoluteOpacity := AbsoluteOpacity * CurDrawable.Opacity;
          Params.ItemSelectedAlpha := GetItemSelectionAlpha(ListItem.Index);
          CurDrawable.Render(Canvas, ItemHeaderIndex, [], Resources, Params, SubPassNo);
        end;
      end;
  end;

  // Restore previous clipping rectangle.
  Canvas.RestoreState(State);

  if FPullToRefresh then
  begin
    NeedPaintScrollingStretchGlow := HasScrollingStretchGlow and (Abs(FScrollStretchStrength) > 0);
    NeedPaintPullRefreshStroke := HasPullRefreshStroke;

    if NeedPaintScrollingStretchGlow or NeedPaintPullRefreshStroke then
    begin
      State := Canvas.SaveState;
      try
        Canvas.IntersectClipRect(GetCleanClipRect);

        if NeedPaintScrollingStretchGlow then
          PaintScrollingStretchGlow(Canvas, FScrollStretchStrength, AbsOpacity);

        if NeedPaintPullRefreshStroke then
          PaintPullRefreshStroke(Canvas, PullRefreshStrength, AbsOpacity);
      finally
        Canvas.RestoreState(State);
      end;
    end;
  end;

  TriggerIncidents(TDelayedIncident.ChangeRepainted);
end;

procedure TListViewBase.Paint;
var
  LOpacity: Single;
begin
  if not (TStateFlag.Painting in FStateFlags) then
  begin
    Include(FStateFlags, TStateFlag.Painting);
    try
      FScrollScale := Canvas.Scale;
      LOpacity := GetAbsoluteOpacity;
      if not FTransparent then
      begin
        FBrush.Color := FBackgroundStyleColor;
        if SameValue(LOpacity, 1, TEpsilon.Vector) then
        begin
          Canvas.Blending := False;
          try
            Canvas.FillRect(LocalRect, 0, 0, AllCorners, 1, FBrush);
          finally
            Canvas.Blending := True;
          end;
        end
        else
          Canvas.FillRect(LocalRect, 0, 0, AllCorners, LOpacity, FBrush);
      end;
      if Adapter.Count > 0 then
        DrawListItems(LOpacity);
    finally
      Exclude(FStateFlags, TStateFlag.Painting);
    end;
  end;
end;

procedure TListViewBase.AfterPaint;
begin
  inherited;
  Exclude(FStateFlags, TStateFlag.Invalid);
end;

procedure TListViewBase.RecreateNativePresentation;
begin
end;

procedure TListViewBase.Loaded;
begin
  inherited;
  ImagesChanged;
  RecreateNativePresentation;
end;

procedure TListViewBase.Resize;
begin
  inherited;
  BeginUpdate;
  try
    Adapter.ResetViews([]);
    if FAutoColumns then
    begin
      FColumns := floor(Width / FColumnWidth);	
{$IFDEF MSWINDOWS} // *** sinuke
      if FScrollBar.Visible then
        FMarg := Trunc(((Width - FScrollBar.Width) - (FColumns * FColumnWidth)) / (FColumns + 1))
      else
        FMarg := Trunc((Width - (FColumns * FColumnWidth)) / (FColumns + 1));
{$ELSE}
      FMarg := Trunc((Width - (FColumns * FColumnWidth)) / (FColumns + 1));
{$ENDIF} // *** sinuke
    end;
  finally
    EndUpdate;
  end;
  UpdateScrollingLimits;
end;

procedure TListViewBase.EndUpdate;
begin
  inherited;
  if not IsUpdating then
  begin
    if TStateFlag.NeedsScrollingLimitsUpdate in FStateFlags then
      UpdateScrollingLimits;
    if TStateFlag.NeedsRebuild in FStateFlags then
      RebuildList;
    FStateFlags := FStateFlags - [TStateFlag.NeedsScrollingLimitsUpdate, TStateFlag.NeedsRebuild];
  end;
end;

function TListViewBase.ObjectAtPoint(P: TPointF): IControl;
var
  LocalPt: TPointF;
  ItemAt: Integer;
  Control: TControl;
  ListItem: TListItem;
begin
  if not FMouseClicked then
  begin
    LocalPt := ScreenToLocal(P);
    // *** ZuBy
    if FHorizontal then
      ItemAt := FindItemAbsoluteAt
        (Round(FScrollViewPos + LocalPt.X - (LocalRect.Left + FSideSpace)))
    else
      ItemAt := FindItemAbsoluteAt
        (Round(FScrollViewPos + LocalPt.Y - (LocalRect.Top + FSideSpace)));
    // ZuBy ***
    if (ItemAt >= 0) and (ItemAt < Adapter.Count) then
    begin
      ListItem := Adapter[ItemAt];
      if ListItem.View.Initialized then
      begin
        Control := ListItem.ObjectAtPoint(P);
        if Control <> nil then
          exit(Control);
      end;
    end;
  end;
  Result := inherited ObjectAtPoint(P);
end;

function TListViewBase.FindLocalItemObjectAtPosition(const ItemIndex: Integer; const Position: TPointF): TListItemDrawable;
var
  I: Integer;
  Item: TListItem;
begin
  if (ItemIndex < 0) or (ItemIndex >= Adapter.Count) then
    exit(nil);

  Item := Adapter[ItemIndex];

  for I := 0 to Item.Count - 1 do
    if Item.View[I].InLocalRect(Position) then
      exit(Item.View[I]);

  Result := nil;
end;

procedure TListViewBase.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  LFirstVisible, LLastVisible: Integer;
  LItemIndex: Integer;

  procedure CalcVisible;
  var
    I: Integer;
    MaxHeight: Integer;
    TopViewIndex: Integer;
    LocRect: TRectF;
  begin
    // Precache local rectangle.
    LocRect := LocalRect;

    // Calculate item height sums.
    // *** ZuBy
    if FHorizontal then
      MaxHeight := Ceil(LocRect.Width + FScrollViewPos)
    else
      MaxHeight := Ceil(LocRect.Height + FScrollViewPos);
    // ZuBy ***

    // Determine starting and final elements that are currently visible.
    TopViewIndex := Trunc(FScrollViewPos - FSideSpace);

    LFirstVisible := Min(Max(FindItemAbsoluteAt(TopViewIndex), 0), Adapter.Count - 1);
    LLastVisible := Adapter.Count - 1;

    for I := LFirstVisible + 1 to Adapter.Count - 2 do
      if FHeightSums[I + 1] >= MaxHeight then
      begin
        LLastVisible := I;
        break;
      end;
  end;

  procedure CycleNewIndexUp;
  begin
    // Cycle through items up until non-header and non-footer is found.
    repeat
      LItemIndex := Max(LItemIndex - 1, 0);
    until (LItemIndex <= 0) or (Adapter[LItemIndex].Purpose = TListItemPurpose.None);
  end;

  procedure CycleNewIndexDown;
  begin
    // Cycle through items down until non-header and non-footer is found.
    repeat
      LItemIndex := Min(LItemIndex + 1, Adapter.Count - 1);
    until (LItemIndex >= ItemCount - 1) or (Adapter[LItemIndex].Purpose = TListItemPurpose.None);
  end;

var
  I: Integer;
  LChanged: Boolean;
  TextProvider: IListViewTextProvider;
begin
  LItemIndex := ItemIndex;
  if FAllowSelection then
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      if (KeyChar > ' ') or (Key in [vkHome, vkEnd, vkUp, vkDown, vkRight, vkLeft]) then
        if TLinkObservers.EditLinkIsReadOnly(Observers) then
          exit
        else if not TLinkObservers.EditLinkEdit(Observers) then
          exit;
  inherited;
  if ItemCount > 0 then
  begin
    if KeyChar <> #0 then
    begin
      if Supports(Adapter, IListViewTextProvider, TextProvider) then
        for I := 0 to ItemCount - 1 do
          if (TextProvider.Text[I] <> '') and (string(TextProvider.Text[I].Chars[0]).ToLower = string(KeyChar).ToLower) then
          begin
            LItemIndex := I;
            break;
          end;

      if KeyChar = #32 then
        Key := vkSpace;

      KeyChar := #0;
    end;
    case Key of
      vkHome:
        begin
          LItemIndex := 0;

          if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
            CycleNewIndexDown;
        end;

      vkEnd:
        begin
          LItemIndex := Adapter.Count - 1;

          if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
            CycleNewIndexUp;
        end;

      vkUp,
      vkLeft:
        begin
          CycleNewIndexUp;

          if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
            CycleNewIndexDown;
        end;

      vkDown,
      vkRight:
      begin
        CycleNewIndexDown;
        if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
          CycleNewIndexUp;
      end;
      vkPrior:
      begin
        CalcVisible;
        LItemIndex := Max(0, LFirstVisible - Max(1, LLastVisible - LFirstVisible + 1));
        if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
          CycleNewIndexDown;
      end;



      vkNext:
      begin
        CalcVisible;
        LItemIndex := Min(Adapter.Count - 1, LLastVisible + Max(1, LLastVisible - LFirstVisible + 1));

        if Adapter[LItemIndex].Purpose <> TListItemPurpose.None then
          CycleNewIndexUp;
      end;

      vkSpace:
      begin
        SetNewItemIndex(FItemIndex);
        if (not HasTouchTracking) and (FItemIndex >= 0) and (FItemIndex < Adapter.Count) then
          Adapter[FItemIndex].MouseSelect;
      end
    else
      exit;
    end;
    LChanged := LItemIndex <> ItemIndex;
    if LChanged then
      TLinkObservers.PositionLinkPosChanging(Observers);  // Validation exception during this call
    SetItemIndexInternal(LItemIndex, True);
    if LChanged then
    begin
      TLinkObservers.ListSelectionChanged(Observers);
      DoChange;
    end;
    Key := 0;
  end;
end;

procedure TListViewBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  function CancelMode: Boolean;
  var
    FirstIndex: Integer;
    Checkable: IListViewCheckProvider;
    GlyphButtonDrawable: TListItemGlyphButton;
  begin
    Result := False;

    if HasTouchTracking then
    begin
      if (not FEditMode) and (FDeleteButtonIndex <> -1) then
      begin
        // hide Delete button
        SetDeleteButtonIndex(-1);
        SetItemIndexInternal(-1);
        FDragListMode := TInternalDragMode.None;
        FMouseClicked := False;
        FTapSelectItemIndex := -1;
        FTapSelectNewIndexApplied := -1;
        exit(True);
      end
      else if Supports(Adapter, IListViewCheckProvider, Checkable) and Checkable.AnyChecked(True) then
      begin
        FirstIndex := Checkable.FirstChecked(True);
        GlyphButtonDrawable := GetGlyphButton(FirstIndex);
        if (FirstIndex <> -1) and (not (TStateFlag.ScrollingActive in FStateFlags)) and
          (GlyphButtonDrawable <> nil) and (GlyphButtonDrawable.ButtonType in [TGlyphButtonType.Delete]) then
        begin
          Checkable.CheckAll(False);
          SetDeleteButtonIndex(-1);
          exit(True);
        end
      end;
    end;
  end;

var
  ItemAt: Integer;
  IsCheckGlyphVisible: Boolean;
  Distance: Single;
  GlyphButtonDrawable: TListItemGlyphButton;
begin
  inherited;

  if not ShouldHandleEvents then
    exit;

  FDragListMode := TInternalDragMode.None;

  if (FAniCalc <> nil) and (TStateFlag.ScrollingActive in FStateFlags) and FAniCalc.Animation then
  begin
    FAniCalc.Averaging := ssTouch in Shift;
    FAniCalc.MouseUp(X, Y);
    FAniCalc.Animation := False;
  end;

  if (FTransitionType <> TTransitionType.None) or CancelMode then
    exit;

  if Button = TMouseButton.mbLeft then
  begin
    FMouseClicked := True;
    FMouseClickIndex := -1;
    FMouseEventIndex := -1;
    FMouseSetItemIndex := -1;

    if HasTouchTracking then
    begin
      StopPropertyAnimation('ScrollViewPos');

      // *** ZuBy
      if FHorizontal then
        Distance := X - LocalRect.Left
      else
        Distance := Y - LocalRect.Top;
      // ZuBy ***

      if (not (TStateFlag.ScrollingActive in FStateFlags)) and FAutoTapScroll and (Distance < FAutoTapTreshold) then
        FAutoTapDistance := Round(FScrollViewPos)
      else
        FAutoTapDistance := 0;
    end;

    if not (TStateFlag.ScrollingActive in FStateFlags) then
    begin
      // ZuBy ***
      if FHorizontal then
        ItemAt := FindItemAbsoluteAtWithCheck
          (Round(FScrollViewPos + X - (LocalRect.Left + FSideSpace)))
      else
        ItemAt := FindItemAbsoluteAtWithCheck
          (Round(FScrollViewPos + Y - (LocalRect.Top + FSideSpace)));
      // *** ZuBy
      if (ItemAt >= 0) and (ItemAt < Adapter.Count) and
        (Adapter[ItemAt].Count > 0) then
      begin
        if Adapter[ItemAt].MouseDown(Button, Shift, PointF(X, Y)) then
        begin
          FMouseEventIndex := ItemAt;

          FClickEventMousePos := TPointF.Create(X, Y) - GetItemRelRect(ItemAt, LocalRect).TopLeft;

          if FAllowSelection then
            SetNewItemIndex(FMouseEventIndex);

          // ZuBy ***
          if FAutoColumns then
            Invalidate;
          // *** ZuBy
        end
        else
        begin
          if Adapter[ItemAt].ObjectAtPoint(TPointF.Create(X, Y)) <> nil then
            FMouseClickIndex := ItemAt;

          if (FMouseClickIndex <> -1) and FAllowSelection then
            SetNewItemIndex(FMouseClickIndex);

          GlyphButtonDrawable := GetGlyphButton(ItemAt);
          IsCheckGlyphVisible := FEditMode and (GlyphButtonDrawable <> nil) and
            (GlyphButtonDrawable.ButtonType in [TGlyphButtonType.Checkbox]);
          if ((FMouseClickIndex = -1) or IsCheckGlyphVisible) and (FDeleteButtonIndex = -1) then
          begin
            FTapSelectItemIndex := ItemAt;
            FTapSelectNewIndexApplied := -1;
            FTapSelectStartTime := FTimerService.GetTick;

            FClickEventMousePos := TPointF.Create(X, Y) - GetItemRelRect(ItemAt, LocalRect).TopLeft;
            FClickEventControl := FindLocalItemObjectAtPosition(ItemAt, TPointF.Create(X, Y));

            if FTouchAnimationObject <> nil then
              FTouchAnimationObject.TouchAnimation.StartAnimation(Self, TTouchAnimationAdapter.TAnimationKind.Pressed);

            UpdateRecurrentTimer;

            // ZuBy ***
            if FAutoColumns then
              Invalidate;
            // *** ZuBy
          end;
        end
      end;
    end
    else
      Include(FStateFlags, TStateFlag.ScrollingMouseTouch);

    if (FMouseClickIndex = -1) and (FMouseEventIndex = -1) then
    begin
      if FAniCalc <> nil then
      begin
        FAniCalc.Averaging := ssTouch in Shift;
        FAniCalc.MouseDown(X, Y);
      end;

      FMouseDownAt := TPointF.Create(X, Y);
      FMouseClickPrev := FMouseDownAt;
      FMouseClickDelta := TPointF.Zero;
      FMousePrevScrollPos := FScrollViewPos;
      FMouseClickSwipeEventSend := False; // ZuBy
    end;
  end;
end;

procedure TListViewBase.MouseMove(Shift: TShiftState; X, Y: Single);
var
  NewDeleteIndex: Integer;
  Checkable: IListViewCheckProvider;
  aMinScroll: Boolean; // ZuBy
begin
  inherited;

  if (FTransitionType <> TTransitionType.None) or (not ShouldHandleEvents) then
    exit;

  if not Enabled then
  begin
    FMouseClicked := False;
    FTapSelectNewIndexApplied := -1;
    FDragListMode := TInternalDragMode.None;
    exit;
  end;

  if FMouseClicked and (FMouseClickIndex = -1) and (FMouseEventIndex = -1) then
  begin
    FMouseClickDelta.X := FMouseClickDelta.X + (X - FMouseClickPrev.X);
    FMouseClickDelta.Y := FMouseClickDelta.Y + (Y - FMouseClickPrev.Y);

    FMouseClickPrev := TPointF.Create(X, Y);

    if FDragListMode = TInternalDragMode.None then
    begin
      if HasTouchTracking and (Abs(FMouseClickDelta.X) > MinSwypeThreshold) 
        and (FCanSwipeDelete or FCanSwipeDirection) and (FTapSelectNewIndexApplied = -1) then
      begin
        if FCanSwipeDirection then
        begin
          if Assigned(FOnSwipe) and (not FMouseClickSwipeEventSend) then
          begin
            FMouseClickSwipeEventSend := True;
            if Abs(FMouseDownAt.X) > (X + MinSwypeThreshold) then
              FOnSwipe(Self, TSwipeDirection.ToLeft)
            else if Abs(FMouseDownAt.X) < (X + MinSwypeThreshold) then
              FOnSwipe(Self, TSwipeDirection.ToRight);
          end;
        end
        else
          FDragListMode := TInternalDragMode.Swype;
      end
      else
      begin
        // ZuBy ***
        if FHorizontal then
          aMinScroll := Abs(FMouseClickDelta.X) > MinScrollThreshold
        else
          aMinScroll := Abs(FMouseClickDelta.Y) > MinScrollThreshold;

        if aMinScroll then
        begin
          FDragListMode := TInternalDragMode.Drag;

          FTapSelectItemIndex := -1;

          if FAniCalc <> nil then
          begin
            FAniCalc.Averaging := ssTouch in Shift;
            FAniCalc.Animation := True;
            FAniCalc.MouseDown(FMouseClickPrev.X, FMouseClickPrev.Y);
          end;
        end;
        // *** ZuBy
      end;
    end;
  end;

  if (FTapSelectNewIndexApplied <> -1) and (FDragListMode = TInternalDragMode.Drag) then
  begin
    if FEditMode and Supports(Adapter, IListViewCheckProvider, Checkable) then
       Checkable[FTapSelectNewIndexApplied] := not Checkable[FTapSelectNewIndexApplied];

    FTapSelectNewIndexApplied := -1;
    SetItemIndexInternal(-1);
  end;

  if (not FEditMode) and (FDragListMode = TInternalDragMode.Swype) and
    (not (TStateFlag.ScrollingMouseTouch in FStateFlags)) and (FDeleteButtonIndex = -1) and HasTouchTracking then
  begin
    // *** ZuBy
    if FHorizontal then
      NewDeleteIndex := FindItemAbsoluteAt
        (Round(FScrollViewPos + X - (LocalRect.Left + FSideSpace)))
    else
      NewDeleteIndex := FindItemAbsoluteAt
        (Round(FScrollViewPos + Y - (LocalRect.Top + FSideSpace)));
    // ZuBy ***

    if (NewDeleteIndex <> -1) and (Adapter[NewDeleteIndex].Purpose <> TListItemPurpose.None) then
      NewDeleteIndex := -1;

    if NewDeleteIndex <> -1 then
    begin
      SetDeleteButtonIndex(NewDeleteIndex);
      SetItemIndexInternal(NewDeleteIndex);
      FTapSelectItemIndex := -1;

      FDragListMode := TInternalDragMode.None;
      FMouseClicked := False;

      if (FAniCalc <> nil) and (TStateFlag.ScrollingActive in FStateFlags) and FAniCalc.Animation then
      begin
        FAniCalc.Averaging := ssTouch in Shift;
        FAniCalc.MouseUp(X, Y);
        FAniCalc.Animation := False;
      end;

      exit;
    end;
  end;

  if (FAniCalc <> nil) and FAniCalc.Down and (FDragListMode = TInternalDragMode.Drag) then
    FAniCalc.MouseMove(X, Y);

  if (FMouseEventIndex <> -1) and (FDragListMode = TInternalDragMode.Drag) then
    Adapter[FMouseEventIndex].MouseMove(Shift, PointF(X, Y));

  if (FTouchAnimationObject <> nil) and (FAniCalc <> nil) and (TStateFlag.ScrollingActive in FStateFlags) then
    FTouchAnimationObject.TouchAnimation.StopAnimation;
end;

function TListViewBase.GetGlyphButton(const Index: Integer): TListItemGlyphButton;
var
  Provider: IListViewGlyphButtonProvider;
begin
  Result := nil;
  if (Index <> -1) and Supports(Adapter, IListViewGlyphButtonProvider, Provider) then
    Result := Provider.GlyphButtonDrawable[Index];
end;

procedure TListViewBase.SetNewItemIndex(const NewIndex: Integer);
var
  AllowChange: Boolean;
  Checkable: IListViewCheckProvider;
  GlyphButtonDrawable: TListItemGlyphButton;
begin
  if FMouseSetItemIndex <> NewIndex then
  begin
    AllowChange := True;

    if NewIndex <> ItemIndex then
      ObserversBeforeSelection(AllowChange);

    if AllowChange then
    begin
      SetItemIndexInternal(NewIndex);
      TLinkObservers.ListSelectionChanged(Observers);
      if FEditMode and Supports(Adapter, IListViewCheckProvider, Checkable) then
      begin
        Checkable[NewIndex] := not Checkable[NewIndex];
        GlyphButtonDrawable := GetGlyphButton(NewIndex);
        if (not (TStateFlag.ScrollingActive in FStateFlags)) and (GlyphButtonDrawable <> nil) and
          (GlyphButtonDrawable.ButtonType in [TGlyphButtonType.Delete]) then
          SetDeleteButtonIndex(NewIndex);
      end;
      DoListItemClick(Adapter[NewIndex]);

      DoChange;
      Invalidate;
    end;
  end;
end;

procedure TListViewBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  NewIndex: Integer;
begin
  inherited;

  if ((not FEditMode) and (FDeleteButtonIndex <> -1)) or (not ShouldHandleEvents) then
  begin
    FMouseClickSwipeEventSend := False;
    exit;
  end;

  if FTapSelectNewIndexApplied <> -1 then
  begin
    FMouseClicked := False;
    FTapSelectNewIndexApplied := -1;
    FDragListMode := TInternalDragMode.None;
    if FAniCalc <> nil then
      FAniCalc.MouseUp(FMouseDownAt.X, FMouseDownAt.Y);
    DoChange;
    Invalidate;
  end;

  if not Enabled then
  begin
    FMouseClicked := False;
    FTapSelectItemIndex := -1;
    FTapSelectNewIndexApplied := -1;
    FDragListMode := TInternalDragMode.None;
    if FAniCalc <> nil then
      FAniCalc.MouseUp(X, Y);
    exit;
  end;

  if (FAniCalc <> nil) and (TStateFlag.ScrollingActive in FStateFlags) then
    FAniCalc.MouseUp(X, Y);

  if (FTouchAnimationObject <> nil) and not ((FAniCalc <> nil) and (TStateFlag.ScrollingActive in FStateFlags)) then
    FTouchAnimationObject.TouchAnimation.StartAnimation(Self, TTouchAnimationAdapter.TAnimationKind.Unpressed);

  if FMouseClicked then
  begin
    FTapSelectItemIndex := -1;

    if not (TStateFlag.ScrollingMouseTouch in FStateFlags) then
    begin
      if FMouseEventIndex <> -1 then
      begin
        Adapter[FMouseEventIndex].MouseUp(Button, Shift, TPointF.Create(X, Y));
        FMouseEventIndex := -1;
      end
      else
      begin
        if (FAutoTapDistance > 0) and (FTapSelectNewIndexApplied = -1) then
        begin
          TAnimator.AnimateFloat(Self, 'ScrollViewPos', 0,     // do not localize
            Min(FAutoTapDistance / AutoTapScrollingSpeed, AutoTapMaxScrollingTime),
            TAnimationType.Out, TInterpolationType.Sinusoidal);
          FAutoTapDistance := 0;
        end
        else if (FMouseClickIndex = -1) and (FTapSelectNewIndexApplied = -1) then
        begin
          if FDragListMode = TInternalDragMode.None then
          begin
            // *** ZuBy
            if FHorizontal then
              NewIndex := FindItemAbsoluteAtWithCheck
                (Round(FScrollViewPos + X - (LocalRect.Left + FSideSpace)))
            else
              NewIndex := FindItemAbsoluteAtWithCheck
                (Round(FScrollViewPos + Y - (LocalRect.Top + FSideSpace)));
            // ZuBy ***

            if (NewIndex >= 0) and (NewIndex < Adapter.Count) and (Adapter[NewIndex].Count > 0) and
              (Adapter[NewIndex].Purpose = TListItemPurpose.None) then
            begin
              if FEditMode or FAllowSelection then
              begin
                if (FMouseClickIndex = -1) and Adapter[NewIndex].HasClickOnSelectItems then
                  FItemIndex := -1;
                SetNewItemIndex(NewIndex);
              end;

              FClickEventItemIndex := NewIndex;
              FClickEventMousePos := TPointF.Create(X, Y) - GetItemRelRect(NewIndex, LocalRect).TopLeft;
              FClickEventControl := FindLocalItemObjectAtPosition(FClickEventItemIndex, TPointF.Create(X, Y));

              if not FMouseClickSwipeEventSend then
                StartIncident(TDelayedIncident.ClickEvent);
              // ZuBy ***
              if FAutoColumns then
                Invalidate;
              // *** ZuBy
            end;
          end;
        end
        else
          FMouseClickIndex := -1;
      end;
    end
    else
      Exclude(FStateFlags, TStateFlag.ScrollingMouseTouch);

    FMouseClicked := False;
    FTapSelectNewIndexApplied := -1;
    FDragListMode := TInternalDragMode.None;
  end;

  FMouseClickSwipeEventSend := False;
end;

procedure TListViewBase.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;

  if not ShouldHandleEvents then
    exit;

  if (not Handled) and (not DisableMouseWheel) then
  begin
    if ssHorizontal in Shift then
    begin
      // ZuBy ***
      if FScrollBar <> nil then
        Offset := FScrollBar.SmallChange
      else
        Offset := Width / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      SetScrollViewPos(ScrollViewPos + Offset);
      Handled := True;
      // *** ZuBy
    end
    else
    begin
      if FScrollBar <> nil then
        Offset := FScrollBar.SmallChange
      else
        Offset := Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      SetScrollViewPos(ScrollViewPos + Offset);
      Handled := True;
    end
  end;
end;

procedure TListViewBase.DoMouseLeave;
begin
  inherited;

  if (FAniCalc <> nil) and ShouldHandleEvents then
    FAniCalc.MouseLeave;
end;

procedure TListViewBase.DoResetEditModeAnimation;
begin
  InvalidateHeights;
end;

procedure TListViewBase.DoSetItemIndexInternal(const Value: Integer);
begin
end;

procedure TListViewBase.DoSetScrollViewPos(const Value: Single);
begin
end;

procedure TListViewBase.Invalidate;
begin
  if not (TStateFlag.Invalid in FStateFlags) then
  begin
    InvalidateRect(LocalRect);
    Include(FStateFlags, TStateFlag.Invalid);
  end;
end;

procedure TListViewBase.DoChange;
begin
  StartIncident(TDelayedIncident.ChangeRepainted, False, ChangeRepaintedIncidentDelay);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TListViewBase.DoChangeRepainted;
begin
  if Assigned(FOnChangeRepainted) then
    FOnChangeRepainted(Self);
end;

procedure TListViewBase.UpdateItemLookups;
const
  ZeroHeights: TEstimatedHeights = (Item: 0; Header: 0; Footer: 0);
var
  I, TotalHeight, PrevItemHeight, CurHeaderRef: Integer;
  ListItem: TListItem;
begin
  if HeightSumsNeedUpdate then
  begin
    BeginUpdate;
    try
      TotalHeight := 0;
      // ZuBy ***
      if not FHorizontal then
      begin
        TotalHeight := FTopOffset;
        if (FSearchEdit <> nil) and FSearchEdit.Visible then
          TotalHeight := TotalHeight + Ceil(FSearchEdit.Height);
      end;
      // *** ZuBy

      FEstimatedHeights := ZeroHeights;

      FHeightSums.Clear;
      FHeightSums.Add(TotalHeight);

      CurHeaderRef := -1;

      for I := 0 to Adapter.Count - 1 do
      begin
        if I > 0 then
        begin
          PrevItemHeight := GetItemHeight(I - 1) + FItemBottomOffset;
          Inc(TotalHeight, PrevItemHeight);

          FHeightSums.Add(TotalHeight);
        end;

        ListItem := Adapter[I];
        case ListItem.Purpose of
          TListItemPurpose.None:
            Adapter[I].HeaderRef := CurHeaderRef;
          TListItemPurpose.Header:
            begin
              FEstimatedHeights.Header := GetItemHeight(I);
              CurHeaderRef := I;
              Adapter[I].HeaderRef := I;
            end;
          TListItemPurpose.Footer:
            FEstimatedHeights.Footer := GetItemHeight(I);
        end;
      end;

      // ZuBy ***
      if not FHorizontal then
        Inc(TotalHeight, FBottomOffset);
      // *** ZuBy

      if Adapter.Count > 0 then
      begin
        Inc(TotalHeight, GetItemHeight(Adapter.Count - 1));
        FEstimatedHeights.Item := TotalHeight / Adapter.Count;
      end;
      if TotalHeight <> FMaxKnownHeight then
      begin
        FMaxKnownHeight := TotalHeight;
        UpdateScrollingLimits;
      end;

      HeightSumsNeedUpdate := False;
    finally
      EndUpdate;
    end;
  end;
end;

function TListViewBase.FindItemAbsoluteAt(const ViewAt: Integer): Integer;
var
  Left, Right, Pivot, Value: Integer;
begin
  UpdateItemLookups;

  if Adapter.Count < 1 then
    exit(-1);
  if ViewAt < 1 then
    exit(0);
  if ViewAt >= FHeightSums[FHeightSums.Count - 1] then
    exit(FHeightSums.Count - 1);

  Left := 0;
  Right := FHeightSums.Count - 1;
  Pivot := 0;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;

    Value := FHeightSums[Pivot];
    if Value = ViewAt then
      exit(Pivot);

    if Value > ViewAt then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := Pivot;

  if (Result > 0) and (Result < FHeightSums.Count) and (FHeightSums[Result] >= ViewAt) then
    Dec(Result);
end;

function TListViewBase.FindItemAbsoluteAtWithCheck(const ViewAt: Integer): Integer;
begin
  Result := -1;
  if Adapter.Count > 0 then
  begin
    UpdateItemLookups;
    if ViewAt < FHeightSums[FHeightSums.Count - 1] + GetItemHeight(Adapter.Count - 1) then
      Result := FindItemAbsoluteAt(ViewAt);
  end;
end;

function TListViewBase.GetDefaultStyleLookupName: string;
begin
  Result := 'listviewstyle';
end;

procedure TListViewBase.ApplyStyle;

  function GetColorFromStyle(const ObjectName: string; const DefaultColor: TAlphaColor): TAlphaColor;
  var
    StyleObject: TFmxObject;
  begin
    StyleObject := FindStyleResource(ObjectName);
    if StyleObject is TColorObject then
      Result := TColorObject(StyleObject).Color
    else if StyleObject is TText then
      Result := TText(StyleObject).Color
    else
      Result := DefaultColor;
  end;

  procedure AssignFontFromStyle(const Font: TFont; const ObjectName: string);
  var
    StyleObject: TFmxObject;
  begin
    StyleObject := FindStyleResource(ObjectName);
    if StyleObject is TFontObject then
      Font.Assign(TFontObject(StyleObject).Font)
    else if StyleObject is TText then
      Font.Assign(TText(StyleObject).Font);
  end;

  function GetStyleObjectFromStyle(const ObjectName: string): TStyleObject;
  var
    StyleObject: TStyleObject;
  begin
    if FindStyleResource<TStyleObject>(ObjectName, StyleObject) then
      Result := StyleObject
    else
      Result := nil;
  end;

begin
  inherited;

  // Control Colors
  FBackgroundStyleColor := GetColorFromStyle('background', claWhite);

  FItemStyleFillColor := GetColorFromStyle('itembackground', claWhite);
  FItemStyleFillAltColor := GetColorFromStyle('alternatingitembackground', claWhite);
  FItemStyleFrameColor := GetColorFromStyle('frame', claBlack);

  // Item Colors
  FStyleResources.DefaultTextColor := GetColorFromStyle('foreground', claBlack);
  FStyleResources.DefaultTextSelectedColor := GetColorFromStyle('selectiontext', claBlack);
  FStyleResources.DetailTextColor := GetColorFromStyle('detailtext', claBlack);
  FStyleResources.HeaderTextColor := GetColorFromStyle('headertext', claWhite);
  FStyleResources.HeaderTextShadowColor := GetColorFromStyle('headertextshadow', claWhite);

  FStyleResources.ButtonTextColor := GetColorFromStyle('buttontext', claWhite);
  FStyleResources.ButtonTextPressedColor := GetColorFromStyle('buttontextpressed', claBlack);
  FStyleResources.DeleteButtonTextColor := GetColorFromStyle('deletebuttontext', claWhite);
  FStyleResources.DeleteButtonTextPressedColor := GetColorFromStyle('deletebuttontextpressed', claWhite);

  // Fonts
  AssignFontFromStyle(FStyleResources.DefaultTextFont, 'font');
  AssignFontFromStyle(FStyleResources.DetailTextFont, 'detailfont');
  AssignFontFromStyle(FStyleResources.HeaderTextFont, 'headerfont');
  AssignFontFromStyle(FStyleResources.ButtonTextFont, 'buttonfont');
  AssignFontFromStyle(FStyleResources.DeleteButtonTextFont, 'deletebuttonfont');

  // Style Images
  // *** ZuBy
  FItemBoxLight := GetStyleObjectFromStyle('boxlight');
  if FItemBoxLight <> nil then
  begin
    FItemStyleFrameColor := TAlphaColorRec.Null;
    TAlphaColorRec(FBackgroundStyleColor).R := 238;
    TAlphaColorRec(FBackgroundStyleColor).G := 238;
    TAlphaColorRec(FBackgroundStyleColor).B := 238;
    TAlphaColorRec(FBackgroundStyleColor).A := 255;
  end;

  FSelectionStyleImage := GetStyleObjectFromStyle('boxdark');
  if FSelectionStyleImage = nil then
    FSelectionStyleImage := GetStyleObjectFromStyle('selection');
  if FSelectionStyleImage = nil then
    FSelectionStyleColor := GetColorFromStyle('selection', claBlue);

  FHeaderStyleImage := GetStyleObjectFromStyle('header');
  if FHeaderStyleImage = nil then
    FHeaderStyleColor := claWhite;

  EnableTouchAnimation(True);
  // ZuBy ***

  FStyleResources.ButtonAddItemStyleImage.Normal := GetStyleObjectFromStyle('additembutton');
  FStyleResources.ButtonAddItemStyleImage.Pressed := GetStyleObjectFromStyle('additembuttonpressed');
  FStyleResources.ButtonDeleteItemStyleImage.Normal := GetStyleObjectFromStyle('deleteitembutton');
  FStyleResources.ButtonDeleteItemStyleImage.Pressed := GetStyleObjectFromStyle('deleteitembuttonglyph');
  FStyleResources.ButtonNormalStyleImage.Normal := GetStyleObjectFromStyle('button');
  FStyleResources.ButtonNormalStyleImage.Pressed := GetStyleObjectFromStyle('buttonpressed');
  FStyleResources.ButtonDeleteStyleImage.Normal := GetStyleObjectFromStyle('deletebutton');
  FStyleResources.ButtonDeleteStyleImage.Pressed := GetStyleObjectFromStyle('deletebuttonpressed');
  FStyleResources.ButtonCheckboxStyleImage.Normal := GetStyleObjectFromStyle('checkboxunchecked');
  FStyleResources.ButtonCheckboxStyleImage.Pressed := GetStyleObjectFromStyle('checkboxchecked');

  // Acessory Images
  FStyleResources.AccessoryImages[TAccessoryType.More].Normal := GetStyleObjectFromStyle('accessorymore');
  FStyleResources.AccessoryImages[TAccessoryType.More].Selected := GetStyleObjectFromStyle('accessorymoreselected');
  FStyleResources.AccessoryImages[TAccessoryType.Checkmark].Normal := GetStyleObjectFromStyle('accessorycheckmark');
  FStyleResources.AccessoryImages[TAccessoryType.Checkmark].Selected := GetStyleObjectFromStyle('accessorycheckmarkselected');
  FStyleResources.AccessoryImages[TAccessoryType.Detail].Normal := GetStyleObjectFromStyle('accessorydetail');
  FStyleResources.AccessoryImages[TAccessoryType.Detail].Selected := GetStyleObjectFromStyle('accessorydetailselected');

  FStyleResources.ScrollingStretchGlowColor := GetColorFromStyle('glow', $FF87C3DC);
  FStyleResources.PullRefreshIndicatorColor := GetColorFromStyle('indicator', $FF686F7B);
  FStyleResources.PullRefreshStrokeColor := GetColorFromStyle('pullrefreshstroke', $FF008CBB);
end;

procedure TListViewBase.FreeStyle;
begin
  if FTouchAnimationObject <> nil then
    FTouchAnimationObject.TouchAnimation.CustomPaint := nil;
  FTouchAnimationObject := nil;
  inherited;
end;

function TListViewBase.GetItemRect(const AItemIndex: Integer): TRectF;
begin
  if (AItemIndex < 0) or (AItemIndex >= Adapter.Count) then
    exit(TRectF.Create(0, 0, 0, 0));
  UpdateItemLookups;
  Result := GetItemRelRect(AItemIndex, LocalRect);
end;

procedure TListViewBase.ScrollTo(const AItemIndex: Integer);
var
  LocRect, ItemRect: TRectF;
  NewPos: Integer;
begin
  if (AItemIndex < 0) or (AItemIndex >= Adapter.Count) then
    exit;

  UpdateItemLookups;

  LocRect := LocalRect;

  ItemRect := GetItemRelRect(AItemIndex, LocRect);
  // ZuBy ***
  if FHorizontal then
  begin
    if ItemRect.Left < LocRect.Left then
    begin
      NewPos := FSideSpace + FHeightSums[AItemIndex];

      // Take into account sticky header, so it does not clutter the view.
      if HasTouchTracking and (Adapter[AItemIndex].HeaderRef <> -1) and
        (Adapter[AItemIndex].HeaderRef <> AItemIndex) then
        Dec(NewPos, GetItemHeight(Adapter[AItemIndex].HeaderRef));

      SetScrollViewPos(NewPos);
    end
    else if ItemRect.Right > LocRect.Right then
      SetScrollViewPos(FSideSpace + FHeightSums[AItemIndex] -
        (LocRect.Width - GetItemHeight(AItemIndex)));
  end
  else
  begin
    if ItemRect.Top < LocRect.Top then
    begin
      NewPos := FSideSpace + FHeightSums[AItemIndex];

      // Take into account sticky header, so it does not clutter the view.
      if HasTouchTracking and (Adapter[AItemIndex].HeaderRef <> -1) and
        (Adapter[AItemIndex].HeaderRef <> AItemIndex) then
        Dec(NewPos, GetItemHeight(Adapter[AItemIndex].HeaderRef));

      SetScrollViewPos(NewPos);
    end
    else if ItemRect.Bottom > LocRect.Bottom then
      SetScrollViewPos(FSideSpace + FHeightSums[AItemIndex] -
        (LocRect.Height - GetItemHeight(AItemIndex)));
  end;
  // *** ZuBy
end;

procedure TListViewBase.ImagesChanged;
begin
  if ([csLoading, csDestroying] * ComponentState = []) and (FImageLink <> nil) then
    Invalidate;
end;

function TListViewBase.GetImages: TCustomImageList;
begin
  if FImageLink <> nil then
    Result := TCustomImageList(FImageLink.Images)
  else
    Result := nil;
end;

procedure TListViewBase.SetImages(const Value: TCustomImageList);
begin
  if FImageLink <> nil then
    FImageLink.Images := Value;
end;

function TListViewBase.GetImageIndex: TImageIndex;
begin
  Result := -1;
end;

procedure TListViewBase.SetImageIndex(const Value: TImageIndex);
begin
  // none
end;

function TListViewBase.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

procedure TListViewBase.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

function TListViewBase.FindItemByPosition(X, Y: Single): Integer;
begin
  // ZuBy ***
  if FHorizontal then
    Result := FindItemAbsoluteAtWithCheck
      (Round(FScrollViewPos + X - (LocalRect.Left + FSideSpace)))
  else
    Result := FindItemAbsoluteAtWithCheck
      (Round(FScrollViewPos + Y - (LocalRect.Top + FSideSpace)));
  // *** ZuBy
end;

procedure TListViewBase.EnableTouchAnimation(Value: Boolean);
var
  TouchAnimation: TCustomStyleObject;
begin
  if (Value) and (FTouchAnimationObject <> nil) then
    exit;

  if Value then
  begin
    // Touch Animation
    if FindStyleResource<TCustomStyleObject>('touchanimation', TouchAnimation)
    then
    begin
      Supports(TouchAnimation, ITouchAnimationObject, FTouchAnimationObject);
      if FTouchAnimationObject <> nil then
        FTouchAnimationObject.TouchAnimation.CustomPaint := Repaint;
    end;
  end
  else
  begin
    if FTouchAnimationObject <> nil then
      FTouchAnimationObject.TouchAnimation.CustomPaint := nil;
    FTouchAnimationObject := nil;
  end;
end;

procedure TListViewBase.SearchBoxClear;
begin
  if FSearchEdit = nil then
    exit;
  FSearchEdit.Text := '';
end;

procedure TListViewBase.RecalcTopViewItemIndex; // ZuBy
var
  TopViewIndex: Integer;
begin
  TopViewIndex := Trunc(FScrollViewPos - FSideSpace);
  FTopItemIndex := Min(Max(FindItemAbsoluteAt(TopViewIndex), 0), Adapter.Count - 1);
end;

function TListViewBase.getItemTextButtonHeight(const AItem: TListItemTextButton; const aWidth: Single = 0): Integer;
// ZuBy
var
  aItemWidth: Single;
begin
  if AItem.Text.IsEmpty then
  begin
    Result := Ceil(AItem.Height);
    exit;
  end;

  aItemWidth := 0;
  if ShowScrollBar then
    aItemWidth := DefaultScrollBarWidth;

  if SameValue(0, aWidth) then
    aItemWidth := Width - (aItemWidth + ItemSpaces.Left + ItemSpaces.Right + (SideSpace * 2))
  else
    aItemWidth := aWidth;

  Result := Ceil(getTextSize(AItem.Text, AItem.Font, AItem.WordWrap, aItemWidth,0).Height);
end;

function TListViewBase.getItemTextButtonWidth(const AItem: TListItemTextButton; const aHeight: Single): Integer;
begin
  if AItem.Text.IsEmpty then
  begin
    Result := Ceil(AItem.Height);
    exit;
  end;

  Result := Ceil(getTextSize(AItem.Text, AItem.Font, AItem.WordWrap, 0, 0).Width);
end;

function TListViewBase.getItemTextHeight(const AItem: TListItemText; const aWidth: Single = 0): Integer; // ZuBy
var
  aItemWidth: Single;
begin
  if AItem.Text.IsEmpty then
  begin
    Result := Ceil(AItem.Height);
    exit;
  end;

  aItemWidth := 0;
  if ShowScrollBar then
    aItemWidth := DefaultScrollBarWidth;

  if SameValue(0, aWidth) then
    aItemWidth := Width - (aItemWidth + ItemSpaces.Left + ItemSpaces.Right + (SideSpace * 2))
  else
    aItemWidth := aWidth;

  Result := Ceil(getTextSize(AItem.Text, AItem.Font, AItem.WordWrap, aItemWidth, 0).Height);

end;

function TListViewBase.getItemTextWidth(const AItem: TListItemText; const aHeight: Single): Integer;
begin
  if AItem.Text.IsEmpty then
  begin
    Result := Ceil(AItem.Height);
    exit;
  end;

  Result := Ceil(getTextSize(AItem.Text, AItem.Font, AItem.WordWrap, 0, 0).Width);
end;

function TAppearanceListView.getLastVisibleItemindex: Integer; // ZuBy
begin
  Result := Min(getFirstVisibleItemIndex + getVisibleCount, TListViewBase(Self).Adapter.Count - 1);
end;

function TListViewBase.getTextSize(const aText: string; aFont: TFont; const aWordWrap: Boolean; aWidth, aHeight: Single): TSizeF; // ZuBy
var
  aMaxSize: TPointF;
begin
  LVTextLayout.BeginUpdate;
  try
    LVTextLayout.Text := aText;

    aMaxSize := PointF(aWidth, aHeight);
    if aWidth = 0 then
      aMaxSize.X := 9999;
    if aHeight = 0 then
      aMaxSize.Y := 9999;

    LVTextLayout.MaxSize := aMaxSize;
    LVTextLayout.Font.Assign(aFont);
    LVTextLayout.Font.Size := aFont.Size;
    LVTextLayout.WordWrap := aWordWrap;
    LVTextLayout.Trimming := TTextTrimming.None;
    LVTextLayout.HorizontalAlign := TTextAlign.Leading;
    LVTextLayout.VerticalAlign := TTextAlign.Leading;
  finally
    LVTextLayout.EndUpdate;
  end;

  Result.Height := LVTextLayout.TextHeight;
  Result.Width := LVTextLayout.TextWidth;
end;

function TAppearanceListView.getVisibleCount: Integer; // ZuBy
begin
  if FHorizontal then
    Result := Round(Width / GetItemAppearanceProperties.Height)
  else
    Result := Round(Height / FItemAppearanceProperties.Height);
end;

function TListViewBase.GetScrollWidth: Single; // ZuBy
begin
  Result := DefaultScrollBarWidth;
end;

procedure TListViewBase.SetHorizontal(const Value: Boolean); // ZuBy
begin
  if FHorizontal <> Value then
  begin
    FHorizontal := Value;
    RebuildOrientation;
    UpdateScrollBar;
  end;
end;

procedure TListViewBase.SetAutoColumns(const Value: Boolean); // ZuBy
begin
  if FAutoColumns <> Value then
  begin
    FAutoColumns := Value;
    Resize;
  end;
end;

procedure TListViewBase.SetBottomOffset(const Value: Integer); // ZuBy
begin
  if FBottomOffset <> Value then
  begin
    FBottomOffset := Value;
    if not FHorizontal then
      DoUpdateScrollViewPos(FScrollViewPos + FBottomOffset); // ???
    InvalidateHeights;
    UpdateItemLookups;
    Invalidate;
  end;
end;

procedure TListViewBase.SetCanScroll(const Value: Boolean); // sinuke
begin
  StopPropertyAnimation('ScrollViewPos');
  FCanScroll := Value;
end;

function TListViewBase.getHeightByIndex(Index: Integer): Integer; // ZuBy
begin
  Result := GetItemHeight(index);
end;
	   
function TListViewBase.getAniCalc: TAniCalculations; // ZuBy
begin
  Result := FAniCalc;
end;

procedure TListViewBase.SetItemBottomOffset(const Value: Integer);
begin
  if FItemBottomOffset <> Value then
  begin
    FItemBottomOffset := Value;
    InvalidateHeights;
    UpdateItemLookups;
    Invalidate;
  end;
end;

procedure TListViewBase.SetSeparatorLeftOffset(const Value: Single); // ZuBy
begin
  if FSeparatorLeftOffset <> Value then
  begin
    FSeparatorLeftOffset := Value;
    InvalidateHeights;
    UpdateItemLookups;
    Invalidate;
  end;
end;

procedure TListViewBase.SetSeparatorRightOffset(const Value: Single); // ZuBy
begin
  if FSeparatorRightOffset <> Value then
  begin
    FSeparatorRightOffset := Value;
    InvalidateHeights;
    UpdateItemLookups;
    Invalidate;
  end;
end;

procedure TListViewBase.SetShowFirstSeparator(const Value: Boolean);
begin
  FShowFirstSeparator := Value;
end;

procedure TListViewBase.SetShowLastSeparator(const Value: Boolean);
begin
  FShowLastSeparator := Value;
end;

procedure TListViewBase.SetShowScrollBar(const Value: Boolean); // ZuBy
begin
  if FShowScrollBar <> Value then
  begin
    FShowScrollBar := Value;
    RebuildOrientation;
    UpdateScrollBar;
  end;
end;

procedure TListViewBase.SetTopOffset(const Value: Integer); // ZuBy
begin
  if FTopOffset <> Value then
  begin
    FTopOffset := Value;
    if not FHorizontal then
      DoUpdateScrollViewPos(FScrollViewPos - FTopOffset);
    InvalidateHeights;
    UpdateItemLookups;
    Invalidate;
  end;
end;

procedure TListViewBase.SetTransparentHeader(const Value: Boolean);
begin
  FTransparentHeaders := Value;
  Invalidate;
end;

procedure TListViewBase.SetTransparentItems(const Value: Boolean);
begin
  FTransparentItems := Value;
  Invalidate;
end;

procedure TListViewBase.SetTransparentSeparator(const Value: Boolean);
begin
  FTransparentSeparator := Value;
  Invalidate;
end;

procedure TListViewBase.SetColumnWidth(const Value: Single); // ZuBy
begin
  if not SameValue(FColumnWidth, Value, TEpsilon.Position) then
  begin
    FColumnWidth := Value;
    Resize;
  end;
end;

procedure TListViewBase.RebuildOrientation; // ZuBy
begin
  if FHorizontal then
  begin
    FScrollBar.Orientation := TOrientation.Horizontal;
    FScrollBar.Align := TAlignLayout.Bottom;
    FScrollBar.Height := DefaultScrollBarWidth;
  end
  else
  begin
    FScrollBar.Orientation := TOrientation.Vertical;
    FScrollBar.Align := TAlignLayout.Right;
    FScrollBar.Width := DefaultScrollBarWidth;
  end;
  Resize;
  FScrollBar.Visible := FShowScrollBar;
  UpdateItemLookups;
end;

{$ENDREGION}
{$REGION 'TPresentedListView'}

procedure TPresentedListView.BeforeDestruction;
var
  PresentationService: IFMXListViewPresentationService;
begin
  inherited;
  if (csDesigning in ComponentState) and
    TPlatformServices.Current.SupportsPlatformService(IFMXListViewPresentationService, PresentationService) then
    PresentationService.DetachPresentation(Self);
end;

destructor TPresentedListView.Destroy;
begin
  FPresentation := nil;
  inherited;
end;

procedure TPresentedListView.PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  ExecuteInterlocked(procedure begin
    FPresentation.ParentChanged;
  end);
end;

procedure TPresentedListView.RecreateNativePresentation;
var
  PresentationService: IFMXListViewPresentationService;
  LPresentation: IInterface;
begin
  if ((FControlType = TControlType.Platform) or (csDesigning in ComponentState)) and
    TPlatformServices.Current.SupportsPlatformService(IFMXListViewPresentationService, PresentationService) then
  begin
    FPresentation := nil; // Make sure that presentation is purged before it's recreated
    LPresentation := PresentationService.AttachPresentation(Self);
    if Supports(LPresentation, IListViewPresentation, FPresentation) and (FItemIndex <> -1) and not FEditMode then
      FPresentation.SetItemSelected(FItemIndex, True);
  end
  else
    FPresentation := nil;
end;

procedure TPresentedListView.ChangeOrder;
begin
  inherited;
  ExecuteInterlocked(procedure begin
    FPresentation.OrderChanged;
  end);
end;

procedure TPresentedListView.AncestorVisibleChanged;
begin
  inherited;
  ExecuteInterlocked(procedure begin
    FPresentation.AncestorVisibleChanged(Visible);
  end);
end;

procedure TPresentedListView.RecalcOpacity;
begin
  inherited;
  ExecuteInterlocked(procedure begin
    FPresentation.AncestorVisibleChanged(Visible);
  end);
end;

procedure TPresentedListView.Paint;
begin
  if FPresentation = nil then
    inherited Paint
  else
  begin
    Adapter.CreateNewViews;
    UpdateItemLookups;
  end;
end;

procedure TPresentedListView.PaintChildren;
const
  LabelMargins = 3;
  LabelPadding = 3;

  function GetOverlayIcon: TBitmap;
  var
    Service: IPresentedControlBehavior;
  begin
    if TBehaviorServices.Current.SupportsBehaviorService(IPresentedControlBehavior, Service, Self) then
      Result := Service.GetOverlayIcon
    else
      Result := nil;
  end;

  procedure PaintDesignTimeCaption;
  const
    ControlTypeCaption = 'P';
  var
    TextWidth: Double;
    TextHeight: Double;
    TextRect: TRectF;
  begin
    TextWidth := Canvas.TextWidth(ControlTypeCaption) + 2 * LabelPadding;
    TextHeight := Canvas.TextHeight(ControlTypeCaption) + 2 * LabelPadding;
    TextRect := TRectF.Create(TPointF.Create(Width - TextWidth - LabelMargins, Height - TextHeight - LabelMargins),
      TextWidth, TextHeight);
    Canvas.Fill.Color := TAlphaColorRec.Black;
    Canvas.FillRect(TextRect, 3, 3, AllCorners, 0.5);
    Canvas.Fill.Color := TAlphaColorRec.White;
    Canvas.FillText(TextRect, ControlTypeCaption, False, 1, [], TTextAlign.Center, TTextAlign.Center);
  end;

  procedure PaintDesignTimeIcon;
  var
    Icon: TBitmap;
    IconRect: TRectF;
    DestRect: TRectF;
  begin
    Icon := GetOverlayIcon;
    if Icon <> nil then
    begin
      IconRect := TRectF.Create(0, 0, Icon.Width, Icon.Height);
      DestRect := TRectF.Create(TPointF.Create(Width - LabelPadding - Icon.Width, Height - LabelPadding - Icon.Height),
        Icon.Width, Icon.Height);
      Canvas.DrawBitmap(Icon, IconRect, DestRect, 0.5);
    end
    else
      PaintDesignTimeCaption;
  end;

begin
  inherited;
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo and (ControlType = TControlType.Platform) then
  begin
    Canvas.SetMatrix(AbsoluteMatrix);
    PaintDesignTimeIcon;
  end;
end;

procedure TPresentedListView.ParentChanged;
begin
  inherited;
  ExecuteInterlocked(procedure begin
    RecalcAbsolute; // This call is required here because it called later in TControl.DoAddObject
    FPresentation.ParentChanged;
  end);
end;

procedure TPresentedListView.RebuildList;
begin
  inherited;
  if not FCreatingNativeView then
  begin
    if (FPresentation <> nil) and not (csLoading in ComponentState) then
      if IsUpdating then
        Include(FStateFlags, TStateFlag.NeedsRebuild)
      else
      begin
        UpdateItemLookups;
        FPresentation.ItemsUpdated;
        if FItemIndex <> -1 then
          FPresentation.SetItemIndex(FItemIndex);
      end;
  end;
end;

procedure TPresentedListView.StopPullRefresh;
begin
  if FPresentation <> nil then
    FPresentation.StopPullRefresh;
end;

function TPresentedListView.GetRootObject: TObject;
begin
  if Root <> nil then
    Result := Root.GetObject
  else
    Result := nil;
end;

function TPresentedListView.GetAdapter: IListViewAdapter;
begin
  Result := Adapter;
end;

function TPresentedListView.GetContentFrame: TRect;
begin
  Result := GetBoundsRect.Round;
end;

function TPresentedListView.GetControlOpacity: Single;
begin
  Result := AbsoluteOpacity;
end;

function TPresentedListView.GetItemText(const ItemIndex: Integer): string;
var
  Provider: IListViewTextProvider;
begin
  if Supports(Adapter, IListViewTextProvider, Provider) then
    Result := Provider.Text[ItemIndex]
  else
    Result := string.Empty;
end;

function TPresentedListView.GetBackgroundStyleColor: TAlphaColor;
begin
  Result := FBackgroundStyleColor;
end;

function TPresentedListView.GetIsTransparent: Boolean;
begin
  Result := FTransparent;
end;

function TPresentedListView.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TPresentedListView.GetItemIndexTitle(const ItemIndex: Integer): string;
var
  Provider: IListViewTextProvider;
begin
  Result := string.Empty;
  if Supports(Adapter, IListViewTextProvider, Provider) then
    Result := Provider.IndexTitle[ItemIndex];
end;

function TPresentedListView.CanSelectItem(const AItemIndex: Integer): Boolean;
begin
  Result := True;
  if ItemIndex <> AItemIndex then
    ObserversBeforeSelection(Result);
end;

procedure TPresentedListView.DidSelectItem(const AItemIndex: Integer);
begin
  ExecuteInterlocked(procedure begin
    inherited SelectItem(AItemIndex);
  end);
  TLinkObservers.ListSelectionChanged(Observers);
end;

function TPresentedListView.CanUnselectItem(const AItemIndex: Integer): Boolean;
begin
  ObserversBeforeSelection(Result);
end;

procedure TPresentedListView.DidUnselectItem(const AItemIndex: Integer);
begin
  ExecuteInterlocked(procedure begin
    inherited UnselectItem(AItemIndex);
  end);
  TLinkObservers.ListSelectionChanged(Observers);
end;

procedure TPresentedListView.DoDeleteItem(const ItemIndex: Integer);
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.ItemsUpdated;
end;

procedure TPresentedListView.DoEditModeChange;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.EditModeChanged;
end;

procedure TPresentedListView.DoResetEditModeAnimation;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.EditModeChanged;
end;

procedure TPresentedListView.DoItemInvalidated(const Item: TListItem);
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.ItemInvalidated(Item);
end;

procedure TPresentedListView.DoItemsChange;
begin
  InvalidateHeights;
  inherited;
  if FPresentation = nil then
    Invalidate;
end;

procedure TPresentedListView.DoItemsInvalidate;
begin
  inherited;
  if not FCreatingNativeView then
  begin
    if IsUpdating then
      Include(FStateFlags, TStateFlag.NeedsRebuild)
    else
      ExecuteInterlocked(procedure begin
        UpdateItemLookups;
        FPresentation.ItemsUpdated;
      end);
  end;
end;

procedure TPresentedListView.DoCheckStateChanged(const AItem: TListItem; const Control: TListItemDrawable);
var
  Checkable: IListViewCheckProvider;
begin
  if Supports(Adapter, IListViewCheckProvider, Checkable) then
  begin
    ExecuteInterlocked(procedure begin
      FPresentation.SetItemSelected(AItem.Index, Checkable[AItem.Index]);
    end);

    if FSelectionCrossfade and (FPresentation = nil) then
      InsertItemCrossFade(AItem.Index, Checkable[AItem.Index]);
  end;
  inherited;
end;

procedure TPresentedListView.DoSetItemIndexInternal(const Value: Integer);
begin
  inherited;
  ExecuteInterlocked(procedure begin
    FPresentation.SetItemIndex(FItemIndex);
  end);
end;

procedure TPresentedListView.DoSetScrollViewPos(const Value: Single);
begin
  inherited;
  if FPresentation = nil then
  begin
    if FAniCalc <> nil then
    begin
      // ZuBy ***
      if FHorizontal then
        FAniCalc.ViewportPosition := TPointD.Create(FScrollViewPos, 0)
      else
        FAniCalc.ViewportPosition := TPointD.Create(0, FScrollViewPos);
      // *** ZuBy
    end;
    if not HasTouchTracking then
      UpdateScrollBar;
  end;
end;

procedure TPresentedListView.DoUpdateScrollingLimits;
begin
  if FPresentation = nil then
    inherited;
end;

procedure TPresentedListView.DoUpdateScrollViewPos(const Value: Single);
begin
  if FPresentation = nil then
    Invalidate;
end;

procedure TPresentedListView.ItemButtonClicked(const ItemIndex: Integer);
var
  Provider: IListViewTextButtonProvider;
begin
  if Supports(Adapter, IListViewTextButtonProvider, Provider) and (Provider.TextButtonDrawable[ItemIndex] <> nil) then
    Provider.TextButtonDrawable[ItemIndex].Click;
end;

procedure TPresentedListView.InvokePullRefresh;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnPullRefresh) then
        FOnPullRefresh(Self);
    end);
end;

function TPresentedListView.HasDesignPresentationAttached: Boolean;
begin
  Result := (csDesigning in ComponentState) and (FPresentation <> nil);
end;

procedure TPresentedListView.SetCreatingNativeView(const Value: Boolean);
begin
  FCreatingNativeView := Value;
end;

procedure TPresentedListView.SetSearchFilter(const Filter: string);
var
  LText: string;
begin
  LText := Filter.Trim.ToLower;
  SetFilterPredicate(
    function (X: string): Boolean
    begin
      Result := LText.IsEmpty or X.ToLower.Contains(LText);
    end);
end;

function TPresentedListView.GetTableViewFlags: TListViewModeFlags;
begin
  Result := [];
  if FEditMode then
    Include(Result, TListViewModeFlag.Edit);
  if Enabled then
    Include(Result, TListViewModeFlag.Enabled);
  if Visible then
    Include(Result, TListViewModeFlag.Visible);
  if HasDeletionEditMode then
    Include(Result, TListViewModeFlag.Deletion);
  if FPullToRefresh then
    Include(Result, TListViewModeFlag.PullRefresh);
  if HasButtonsInCells then
    Include(Result, TListViewModeFlag.Buttons);
  if FSearchVisible then
    Include(Result, TListViewModeFlag.Search);
  if FSearchAlwaysOnTop then
    Include(Result, TListViewModeFlag.SearchOnTop);
  if FPullRefreshWait then
    Include(Result, TListViewModeFlag.PullRefreshWait);
  if FCanSwipeDelete then
    Include(Result, TListViewModeFlag.SwipeDelete);
end;

function TPresentedListView.GetTableViewOptions: TListViewNativeOptions;
begin
  Result := FNativeOptions;
end;

procedure TPresentedListView.RecalcEnabled;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.StatusChanged;
end;

function TPresentedListView.ShouldHandleEvents: Boolean;
begin
  Result := FPresentation = nil;
end;

procedure TPresentedListView.Show;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.StatusChanged;
end;

procedure TPresentedListView.Hide;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.StatusChanged;
end;

procedure TPresentedListView.Resize;
begin
  inherited;

  if FPresentation <> nil then
    FPresentation.SizeChanged
  else if Adapter.Count > 0 then
    FScrollViewPos := Min(FScrollViewPos, GetMaxScrollViewPos);
end;

procedure TPresentedListView.DoItemsResize;
begin
  inherited;
  if IsUpdating then
    Include(FStateFlags, TStateFlag.NeedsRebuild)
  else
    ExecuteInterlocked(procedure begin
      FPresentation.ItemsUpdated;
    end);
end;

procedure TPresentedListView.DoAbsoluteChanged;
begin
  inherited;
  if FPresentation <> nil then
    FPresentation.ParentChanged;
end;

procedure TPresentedListView.ExecuteInterlocked(const P: TProc);
begin
  if (FPresentation <> nil) and (TInterlocked.CompareExchange(FPresentationLocked, 1, 0) = 0) then
  try
    P;
  finally
    TInterlocked.Exchange(FPresentationLocked, 0);
  end;
end;

{$ENDREGION}
{$REGION 'TAppearanceListView'}

constructor TAppearanceListView.Create(AOwner: TComponent);
begin
  inherited;

  FAppearanceProperties := TPublishedAppearance.Create(Self);
  FItemAppearanceObjects := TPublishedObjects.Create(Self);

  FItemAppearanceProperties := TItemAppearanceProperties.Create(Self, TAppearanceType.Item);
  InitializeItemAppearance(FItemAppearanceProperties);
  FItemEditAppearanceProperties := TItemAppearanceProperties.Create(Self, TAppearanceType.ItemEdit);
  InitializeItemAppearance(FItemEditAppearanceProperties);
  FHeaderAppearanceProperties := TItemAppearanceProperties.Create(Self, TAppearanceType.Header);
  InitializeItemAppearance(FHeaderAppearanceProperties);
  FFooterAppearanceProperties := TItemAppearanceProperties.Create(Self, TAppearanceType.Footer);
  InitializeItemAppearance(FFooterAppearanceProperties);

  // Create our own adapter
  Items := TAppearanceListViewItems.Create(Self);

  FTransparentSeparator := False;
  FTransparentItems := False;
  FAutoPositionToItem := False;
  FTopItemIndex := -1; // ZuBy
  FSeparatorLeftOffset := 0; // ZuBy
  FSeparatorRightOffset := 0; // ZuBy
  FMakeSelectedItemVisible := True; // ZuBy
  FHorizontal := False; // ZuBy
  FShowScrollBar := True; // ZuBy
end;

destructor TAppearanceListView.Destroy;
begin
  FAppearanceProperties.Free;
  FreeAndNil(FItemAppearanceObjects);
  FItemAppearanceProperties.Free;
  FItemEditAppearanceProperties.Free;
  FHeaderAppearanceProperties.Free;
  FFooterAppearanceProperties.Free;
  SetAdapter(nil);
  FAppearanceViewItems.Free;
  inherited;
end;

procedure TAppearanceListView.SetAppearanceListViewItems(const AItems: TAppearanceListViewItems);
begin
  FAppearanceViewItems := AItems;
  FAppearanceViewItems.OnNotify := ObjectsNotify;
  SetAdapter(AItems as IListViewAdapter);
end;

procedure TAppearanceListView.DoAdapterSet;
var
  Editor: IListViewEditor;
begin
  if FAppearanceViewItems = nil then
    raise EListViewError.Create(SUseItemsPropertyToSetAdapter);
  if Adapter <> nil then
  begin
    if Supports(Adapter, IListViewEditor, Editor) then
    begin
      Editor.OnBeforeItemAdded := EditorBeforeItemAdded;
      Editor.OnAfterItemAdded := EditorAfterItemAdded;
      Editor.OnBeforeItemDeleted := EditorBeforeItemDeleted;
      Editor.OnAfterItemDeleted := EditorAfterItemDeleted;
    end;

    if Adapter.Count > 0 then
    begin
      ItemAppearanceChange(FItemAppearanceProperties);
      ItemAppearanceChange(FItemEditAppearanceProperties);
      ItemAppearanceChange(FHeaderAppearanceProperties);
      ItemAppearanceChange(FFooterAppearanceProperties);
    end;
  end;
  DoChange;
end;

function TAppearanceListView.GetAppearanceListViewItem(const Index: Integer): TListViewItem;
begin
  Result := FAppearanceViewItems.AppearanceItem[Index];
end;

function TAppearanceListView.GetHeaderAppearanceName: string;
begin
  Result := FHeaderAppearanceProperties.Name;
end;

function TAppearanceListView.GetHeaderAppearanceProperties: TItemAppearanceProperties;
begin
  Result := FHeaderAppearanceProperties;
end;

function TAppearanceListView.GetHeaderAppearanceClassName: string;
begin
  Result := FHeaderAppearanceProperties.AppearanceClassName;
end;

function TAppearanceListView.GetFooterAppearanceName: string;
begin
  Result := FFooterAppearanceProperties.Name;
end;

function TAppearanceListView.GetFooterAppearanceProperties: TItemAppearanceProperties;
begin
  Result := FFooterAppearanceProperties;
end;

function TAppearanceListView.GetFooterAppearanceClassName: string;
begin
  Result := FFooterAppearanceProperties.AppearanceClassName;
end;

function TAppearanceListView.GetItemAppearanceName: string;
begin
  Result := FItemAppearanceProperties.Name;
end;

function TAppearanceListView.GetItemAppearanceObjects: TPublishedObjects;
begin
  Result := FItemAppearanceObjects;
end;

function TAppearanceListView.GetItemEditAppearanceName: string;
begin
  Result := FItemEditAppearanceProperties.Name;
end;

function TAppearanceListView.GetItemAppearanceProperties: TItemAppearanceProperties;
begin
  Result := FItemAppearanceProperties;
end;

function TAppearanceListView.GetItemEditAppearanceProperties: TItemAppearanceProperties;
begin
  Result := FItemEditAppearanceProperties;
end;

function TAppearanceListView.GetItemObjectsClassName: string;
begin
  Result := FItemAppearanceProperties.AppearanceClassName;
end;

function TAppearanceListView.HasButtonsInCells: Boolean;
begin
  Result := ItemAppearance.ItemAppearance = TAppearanceNames.ImageListItemRightButton;
end;

function TAppearanceListView.HasCheckboxMode: Boolean;
begin
  Result := FEditMode and (FAppearanceAllowsCheckboxes or ((not IsDeleteModeAllowed) and HasDeletionEditMode));
end;

function TAppearanceListView.HasDeletionEditMode: Boolean;
begin
  Result := FAppearanceAllowsDeleteMode;
end;

procedure TAppearanceListView.SetItemHeight(const Value: Integer);
begin
  FItemAppearanceProperties.Height := Value;
end;

procedure TAppearanceListView.SetItemEditHeight(const Value: Integer);
begin
  FItemEditAppearanceProperties.Height := Value;
end;

procedure TAppearanceListView.SetHeaderHeight(const Value: Integer);
begin
  FHeaderAppearanceProperties.Height := Value;
end;

procedure TAppearanceListView.SetFooterHeight(const Value: Integer);
begin
  FFooterAppearanceProperties.Height := Value;
end;

function TAppearanceListView.GetItemEditObjectsClassName: string;
begin
  Result := FItemEditAppearanceProperties.AppearanceClassName;
end;

procedure TAppearanceListView.SetFooterAppearanceClassName(const Value: string);
begin
  FFooterAppearanceProperties.AppearanceClassName := Value;
end;

procedure TAppearanceListView.SetAppearanceProperties(const Value: TPublishedAppearance);
begin
  Assert(False);
  // Do nothing
end;

procedure TAppearanceListView.SetItemAppearanceObjects(const Value: TPublishedObjects);
begin
  Assert(False);
  // Do nothing
end;

procedure TAppearanceListView.SetItemObjectsClassName(const Value: string);
begin
  FItemAppearanceProperties.AppearanceClassName := Value;
end;

procedure TAppearanceListView.SetItemEditObjectsClassName(const Value: string);
begin
  FItemEditAppearanceProperties.AppearanceClassName := Value;
end;

procedure TAppearanceListView.SetHeaderAppearanceName(const Value: string);
begin
  if FHeaderAppearanceProperties.Name <> Value then
  begin
    FHeaderAppearanceProperties.Name := Value;
    RecreateNativePresentation;
  end;
end;

procedure TAppearanceListView.SetHeaderAppearanceClassName(const Value: string);
begin
  FHeaderAppearanceProperties.AppearanceClassName := Value;
end;

procedure TAppearanceListView.SetFooterAppearanceName(const Value: string);
begin
  if FFooterAppearanceProperties.Name <> Value then
  begin
    FFooterAppearanceProperties.Name := Value;
    RecreateNativePresentation;
  end;
end;

function TAppearanceListView.GetAppearanceProperties: TArray<TItemAppearanceProperties>;
begin
  SetLength(Result, 4);
  Result[0] := FItemEditAppearanceProperties;
  Result[1] := FItemAppearanceProperties;
  Result[2] := FHeaderAppearanceProperties;
  Result[3] := FFooterAppearanceProperties;
end;

procedure TAppearanceListView.SetItemAppearanceName(const Value: string);
begin
  if FItemAppearanceProperties.Name <> Value then
  begin
    FItemAppearanceProperties.Name := Value;
    RecreateNativePresentation;
  end;
end;

procedure TAppearanceListView.SetItemEditAppearanceName(const Value: string);

  function AllowsCheckboxes(const Name: string): Boolean;
  begin
    Result := (Name = TAppearanceNames.ListItemRightDetailShowCheck) or
      (Name = TAppearanceNames.ImageListItemShowCheck) or
      (Name = TAppearanceNames.ImageListItemRightButtonShowCheck) or
      (Name = TAppearanceNames.ListItemShowCheck) or
      (Name = TAppearanceNames.ImageListItemBottomDetailShowCheck) or
      (Name = TAppearanceNames.ImageListItemBottomDetailRightButtonShowCheck);
  end;

  function AllowsDeleteMode(const Name: string): Boolean;
  begin
    Result := (Name = TAppearanceNames.ListItemDelete) or
      (Name = TAppearanceNames.ListItemRightDetailDelete) or
      (Name = TAppearanceNames.ImageListItemDelete) or
      (Name = TAppearanceNames.ImageListItemRightButtonDelete);
  end;

var
  Checkable: IListViewCheckProvider;
begin
  if FItemEditAppearanceProperties.Name <> Value then
  begin
    FItemEditAppearanceProperties.Name := Value;
    SetDeleteButtonIndex(-1);
    FAppearanceAllowsCheckboxes := AllowsCheckboxes(Value);
    FAppearanceAllowsDeleteMode := AllowsDeleteMode(Value);
    if Supports(Adapter, IListViewCheckProvider, Checkable) then
      Checkable.CheckAll(False);
    RecreateNativePresentation;
  end;
end;

procedure TAppearanceListView.InitializeItemAppearance(const AAppearance: TItemAppearanceProperties);
begin
  AAppearance.AppearanceClass := TNullItemObjects;
end;

procedure TAppearanceListView.ItemAppearanceChange(const Sender: TItemAppearanceProperties);
var
  Purposes: set of TListItemPurpose;
begin
  if TStateFlag.ResettingObjects in FStateFlags then
    exit;
  if Sender = nil then
    Purposes := [Low(TListItemPurpose)..High(TListItemPurpose)]
  else
    Purposes := [Sender.Purpose];
  if FUpdatingAppearance = 0 then
    AppearanceResetObjects(Purposes)
  else
    FChangedAppearanceObjects := FChangedAppearanceObjects + Purposes;
end;

procedure TAppearanceListView.ItemAppearanceChangeObjects(const Sender: TItemAppearanceProperties);
begin
  ItemAppearanceChange(Sender);
end;

procedure TAppearanceListView.ObjectsNotify(Sender: TObject; const Item: TListItem; Action: TCollectionNotification);
begin
  if Action = TCollectionNotification.cnRemoved then
  begin
    FItemSelectedBeforeChange := nil;
    FItemSelectedBeforeEdit := nil;
  end;
end;

procedure TAppearanceListView.AppearanceResetObjects(APurposes: TListItemPurposes);
var
  LPurposes: TListItemPurposes;
  LProperties: TItemAppearanceProperties;
begin
  if Adapter <> nil then
  begin
    LPurposes := [];
    for LProperties in GetAppearanceProperties do
      if LProperties.Active then
        Include(LPurposes, LProperties.Purpose);
    LPurposes := LPurposes * APurposes;
    if LPurposes <> [] then
    begin
      Adapter.ResetViews(LPurposes);
      InvalidateHeights;
      UpdateScrollingLimits;
      Invalidate;
    end;
  end;
end;

procedure TAppearanceListView.ApplyStyle;
begin
  inherited;
  UpdateAppearanceStyleResources;
end;

procedure TAppearanceListView.AppearanceResetHeights(APurposes: TListItemPurposes);
var
  LPurposes: TListItemPurposes;
  LProperties: TItemAppearanceProperties;
begin
  if Adapter <> nil then
  begin
    LPurposes := [];
    for LProperties in GetAppearanceProperties do
      if LProperties.Active then
        Include(LPurposes, LProperties.Purpose);
    LPurposes := LPurposes * APurposes;
    if LPurposes <> [] then
    begin
      InvalidateHeights;
      UpdateScrollingLimits;
      Invalidate;
    end;
  end;
end;

procedure TAppearanceListView.DoResetView(const Item: TListItem);
begin
  inherited;
  ResetViewAppearance(TListViewItem(Item));
end;

procedure TAppearanceListView.ResetViewAppearance(const AItem: TListViewItem);
var
  LHandled: Boolean;
  LItemObjects: TItemAppearanceObjects;
begin
  if TStateFlag.ResettingObjects in FStateFlags then
    exit;
  Include(FStateFlags, TStateFlag.ResettingObjects);
  try
    LItemObjects := nil;
    case AItem.Purpose of
      TListItemPurpose.None:
        if FItemEditAppearanceProperties.Active then
          LItemObjects := FItemEditAppearanceProperties.Objects
        else if FItemAppearanceProperties.Active then
          LItemObjects := FItemAppearanceProperties.Objects;
      TListItemPurpose.Header:
        if FHeaderAppearanceProperties.Active then
          LItemObjects := FHeaderAppearanceProperties.Objects;
      TListItemPurpose.Footer:
        if FFooterAppearanceProperties.Active then
          LItemObjects := FFooterAppearanceProperties.Objects;
    else
      Assert(False);
    end;
    if (LItemObjects <> nil) and not (LItemObjects is TNullItemObjects) then
    begin
      LHandled := False;
      DoUpdatingItemView(AItem, LHandled);
      if not LHandled then
        LItemObjects.ResetObjects(AItem, GetFinalItemSize);
      DoUpdateItemView(AItem);
    end;
  finally
    Exclude(FStateFlags, TStateFlag.ResettingObjects);
  end;
end;

procedure TAppearanceListView.RefreshAppearances(const APurposes: TListItemPurposes);
begin
  Adapter.ResetViews(APurposes);
  InvalidateHeights; // Object heights may have changed
end;

procedure TAppearanceListView.UpdateAppearanceStyleResources;
begin
  Assert(not FUpdatingStyleResources);
  TNonReentrantHelper.Execute(FUpdatingStyleResources,
    procedure begin
      RefreshAppearances;
    end);
end;

procedure TAppearanceListView.WillEnterEditMode(const Animated: Boolean);
begin
  if Animated then
    EditModeAppearances
  else
    RefreshAppearances([TListItemPurpose.None]);
end;

procedure TAppearanceListView.ItemAppearanceChangeHeight(const Sender: TItemAppearanceProperties);
begin
  Assert(Sender <> nil);
  if Sender <> nil then
    if FUpdatingAppearance = 0 then
      AppearanceResetHeights([Sender.Purpose])
    else
      Include(FChangedAppearanceHeights, Sender.Purpose);
end;

procedure TAppearanceListView.DoResetEditModeAnimation;
begin
  inherited;
end;

procedure TAppearanceListView.BeginUpdate;
begin
  inherited;
  Inc(FUpdatingAppearance);
  if FUpdatingAppearance = 1 then
  begin
    FChangedAppearanceObjects := [];
    FChangedAppearanceHeights := [];
  end;
end;

procedure TAppearanceListView.EndUpdate;
begin
  inherited;
  if FUpdatingAppearance > 0 then
  begin
    Dec(FUpdatingAppearance);
    if FUpdatingAppearance = 0 then
    begin
      AppearanceResetObjects(FChangedAppearanceObjects);
      AppearanceResetHeights(FChangedAppearanceHeights - FChangedAppearanceObjects);
    end;
  end;
end;

procedure TAppearanceListView.EditModeAppearances;
  function CanResetObjects: Boolean;
  var
    LProperties: TItemAppearanceProperties;
  begin
    Result := False;
    for LProperties in GetAppearanceProperties do
      if LProperties.Active and not (LProperties.Objects is TNullItemObjects) then
        exit(True);
  end;

var
  Filter: IListViewFilterable;
  Item: TListItem;
begin
  if Supports(Adapter, IListViewFilterable, Filter) and CanResetObjects then
  begin
    BeginUpdate;
    try
      for Item in Filter.UnfilteredItems do
        if Item is TListViewItem and (Item.Purpose = TListItemPurpose.None) then
          Item.View.Initialized := False;
    finally
      EndUpdate;
    end;
  end;
end;

function TAppearanceListView.GetItemHeight(const Index: Integer): Integer;
var
  Item: TListItem;
begin
  if (Index < 0) or (Index >= Adapter.Count) then
    exit(0);
  Item := Adapter[Index];
  Result := Item.Height;
  if Result < 1 then
    case Item.Purpose of
      TListItemPurpose.None:
        if EditMode and FItemEditAppearanceProperties.Active then
          Result := ItemEditHeight
        else
          Result := ItemHeight;
      TListItemPurpose.Header:
        Result := HeaderHeight;
      TListItemPurpose.Footer:
        Result := FooterHeight;
    else
      Assert(False);
    end;
end;

function TAppearanceListView.GetItemHeight: Integer;
begin
  Result := FItemAppearanceProperties.Height;
end;

function TAppearanceListView.GetItemEditHeight: Integer;
begin
  Result := FItemEditAppearanceProperties.Height;
end;

function TAppearanceListView.GetHeaderHeight: Integer;
begin
  Result := FHeaderAppearanceProperties.Height;
end;

function TAppearanceListView.GetFooterHeight: Integer;
begin
  Result := FFooterAppearanceProperties.Height;
end;

procedure TAppearanceListView.Resize;
begin
  inherited;
end;

procedure TAppearanceListView.DoRequestReindexing(const Item: TListItem);
begin
  FAppearanceViewItems.ReindexAndFindItem(TListViewItem(Item));
end;

procedure TAppearanceListView.DoCheckStateChanged(const AItem: TListItem; const Control: TListItemDrawable);
begin
  inherited;
  if Assigned(FOnButtonChange) and (Control is TListItemSimpleControl) then
    FOnButtonChange(Self, AItem, TListItemSimpleControl(Control));
end;

procedure TAppearanceListView.DoControlClicked(const Item: TListItem; const Control: TListItemDrawable);
begin
  inherited;
  if Control is TListItemSimpleControl then
  begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, Item, TListItemSimpleControl(Control));
    FClickEventControl := Control;

    if not FMouseClickSwipeEventSend then // ZuBy
      StartIncident(TDelayedIncident.ClickEvent);
  end;
end;

procedure TAppearanceListView.DoItemResized(const Item: TListItem);
begin
  FAppearanceViewItems.ItemsResize;
end;

procedure TAppearanceListView.DoListItemClick(const AItem: TListItem);
begin
  inherited;
  if Assigned(FOnItemClick) and (AItem is TListViewItem) and
    (not FMouseClickSwipeEventSend) then
    FOnItemClick(Self, TListViewItem(AItem));
end;

procedure TAppearanceListView.DoUpdateItemView(const AListItem: TListItem);
begin
  inherited;
  if Assigned(FOnUpdateObjects) and (AListItem is TListViewItem) then
    FOnUpdateObjects(Self, TListViewItem(AListItem));
end;

procedure TAppearanceListView.DoUpdatingItemView(const AListItem: TListItem; var AHandled: Boolean);
begin
  inherited;
  if Assigned(FOnUpdatingObjects) and (AListItem is TListViewItem) then
    FOnUpdatingObjects(Self, TListViewItem(AListItem), AHandled);
end;

function TAppearanceListView.getFirstVisibleItemIndex: Integer; // ZuBy
begin
  Result := FTopItemIndex;
end;

procedure TAppearanceListView.SetColorBackground(aColor: TAlphaColor); // ZuBy
begin
  FBackgroundStyleColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorButtonText(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.ButtonNormalStyleImage.Normal := nil;
  FStyleResources.ButtonNormalStyleImage.Pressed := nil;
  FStyleResources.ButtonTextColor := aColor;
  FItemAppearanceObjects.ItemObjects.TextButton.TextColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorButtonTextPressed(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.ButtonTextPressedColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorDeleteText(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.DeleteButtonTextColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorDeleteTextPressed(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.DeleteButtonTextPressedColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorDeleteTintColor(aColor: TAlphaColor); // ZuBy
begin
  FDeleteButton.TintColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorHeader(aColor: TAlphaColor); // ZuBy
begin
  FHeaderStyleImage := nil;
  FHeaderStyleColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorItemFill(aColor: TAlphaColor); // ZuBy
begin
  FItemStyleFillColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorItemFillAlt(aColor: TAlphaColor); // ZuBy
begin
  FItemStyleFillAltColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorItemSelected(aColor: TAlphaColor); // ZuBy
begin
  FSelectionStyleImage := nil;
  FSelectionStyleColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorItemSeparator(aColor: TAlphaColor); // ZuBy
begin
  FItemStyleFrameColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorText(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.DefaultTextColor := aColor;
  ItemAppearanceObjects.ItemObjects.Text.TextColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorTextDetail(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.DetailTextColor := aColor;
  ItemAppearanceObjects.ItemObjects.Detail.TextColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorTextHeader(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.HeaderTextColor := aColor;
  ItemAppearanceObjects.HeaderObjects.Text.TextColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorTextHeaderShadow(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.HeaderTextShadowColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorTextSelected(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.DefaultTextSelectedColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.ItemsClearTrue; // ZuBy
begin
  ScrollViewPos := 0;
  while Items.Count > 0 do
    Items.Delete(0);
end;

function TAppearanceListView.IsCustomColorUsed(const ItemIndex: Integer): Boolean; // ZuBy
begin
  Result := Items[ItemIndex].Data['aUseCustomColor'].AsBoolean;
end;


procedure TAppearanceListView.SetCustomColorForItem(const ItemIndex: Integer; const aColor: TAlphaColor); // ZuBy
begin
  with Items[ItemIndex] do
  begin
    Data['aUseCustomColor'] := True;
    Data['aCustomColor'] := aColor;
  end;
  Repaint;
end;

procedure TAppearanceListView.SetDefaultColorForItem(const ItemIndex: Integer); // ZuBy
begin
  Items[ItemIndex].Data['aUseCustomColor'] := False;
  Repaint;
end;

procedure TAppearanceListView.SetColorPullRefresh(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.PullRefreshStrokeColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorPullRefreshIndicator(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.PullRefreshIndicatorColor := aColor;
  Repaint;
end;

procedure TAppearanceListView.SetColorStretchGlow(aColor: TAlphaColor); // ZuBy
begin
  FStyleResources.ScrollingStretchGlowColor := aColor;
  Repaint;
end;

{$ENDREGION}
{$REGION 'TListView'}
procedure TListView.InitializeItemAppearance(const AAppearance: TItemAppearanceProperties);
begin
  case AAppearance.AppearanceType of
    TAppearanceType.Item:
      AAppearance.AppearanceClass := TAppearancesRegistry.FindItemAppearanceObjectsClassByOption(
        TRegisterAppearanceOption.DefaultItem);
    TAppearanceType.ItemEdit:
      AAppearance.AppearanceClass := TAppearancesRegistry.FindItemAppearanceObjectsClassByOption(
        TRegisterAppearanceOption.DefaultItemEdit);
    TAppearanceType.Header:
      AAppearance.AppearanceClass := TAppearancesRegistry.FindItemAppearanceObjectsClassByOption(
        TRegisterAppearanceOption.DefaultHeader);
    TAppearanceType.Footer:
      AAppearance.AppearanceClass := TAppearancesRegistry.FindItemAppearanceObjectsClassByOption(
        TRegisterAppearanceOption.DefaultFooter);
  else
    Assert(False);
  end;
end;

procedure TAppearanceListView.EditorBeforeItemAdded(Sender: IListViewEditor);
begin
  FItemSelectedBeforeEdit := Selected;
end;

procedure TAppearanceListView.EditorAfterItemAdded(Sender: IListViewEditor; const Item: TListItem);
begin
  if (FItemSelectedBeforeEdit <> nil) and (ItemIndex <> FItemSelectedBeforeEdit.Index) then
    SetItemIndexInternal(FItemSelectedBeforeEdit.Index, True, True);
  FItemSelectedBeforeEdit := nil;
end;

procedure TAppearanceListView.EditorBeforeItemDeleted(Sender: IListViewEditor; const Index: Integer);
begin
  if ItemIndex >= Adapter.Count - 1 then
    ItemIndex := -1;
  RemoveItemCrossFade(Index);
  FItemSelectedBeforeEdit := Selected;
  if (FItemSelectedBeforeEdit <> nil) and (FItemSelectedBeforeEdit.Index = Index) then
    FItemSelectedBeforeEdit := nil;
end;

procedure TAppearanceListView.EditorAfterItemDeleted(Sender: IListViewEditor);
begin
  if (FItemSelectedBeforeEdit <> nil) and (ItemIndex <> FItemSelectedBeforeEdit.Index) then
    SetItemIndexInternal(FItemSelectedBeforeEdit.Index, True, True);
  FItemSelectedBeforeEdit := nil;
end;

{$ENDREGION}
{$REGION 'TAdapterListView'}

procedure TAdapterListView.SetAdapter(Adapter: IListViewAdapter);
begin
  FAdapter := Adapter;
  HeightSumsNeedUpdate := True;
  if Adapter <> nil then
  begin
    FAdapter.OnItemsMayChange := ItemsMayChange;
    FAdapter.OnItemsCouldHaveChanged := ItemsCouldHaveChanged;
    FAdapter.OnChanged := ItemsChange;
    FAdapter.OnItemsInvalidate := ItemsInvalidate;
    FAdapter.OnItemsResize := ItemsResize;
    FAdapter.OnResetView := ResetView;
    DoAdapterSet;
  end;
end;

procedure TAdapterListView.ItemsChange(Sender: TObject);
begin
  DoItemsChange;
end;

procedure TAdapterListView.ItemsCouldHaveChanged(Sender: TObject);
begin
  DoItemsCouldHaveChanged;
end;

procedure TAdapterListView.ItemsMayChange(Sender: TObject);
begin
  DoItemsMayChange;
end;

procedure TAdapterListView.ItemsInvalidate(Sender: TObject);
begin
  DoItemsInvalidate;
end;

procedure TAdapterListView.ResetView(Sender: TObject);
begin
  DoResetView(Sender as TListItem);
end;

procedure TAdapterListView.ItemsResize(Sender: TObject);
begin
  DoItemsResize;
end;

procedure TAdapterListView.DoAdapterSet;
begin
end;

procedure TAdapterListView.DoItemsChange;
begin
end;

procedure TAdapterListView.DoItemsCouldHaveChanged;
begin
end;

procedure TAdapterListView.DoItemsMayChange;
begin
end;

procedure TAdapterListView.DoItemsInvalidate;
begin
end;

procedure TAdapterListView.DoResetView(const Sender: TListItem);
begin
end;

procedure TAdapterListView.DoItemsResize;
begin
  InvalidateHeights;
end;

procedure TAdapterListView.InvalidateHeights;
begin
  FHeightSumsNeedUpdate := True;
end;
{$ENDREGION}

initialization

  LVTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  RegisterFmxClasses([TCustomListView, TListView]);
finalization
  LVTextLayout.Free;
end.
