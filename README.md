# ModernListView (Berlin, Tokyo [with fix], Rio)
Modern ListView - Colorizer, Vertical\Horizontal mode, Columns and other

## Colorizer
 ![colorizer](/screenshots/colorizer.png?raw=true)
  * ListView1.SetColorItemSelected(TAlphaColorRec.Orangered);
  * ListView1.SetColorItemFill(TAlphaColorRec.Whitesmoke);
  * ListView1.SetColorItemFillAlt(TAlphaColorRec.Lightgrey);
  * ListView1.SetColorBackground(TAlphaColorRec.Whitesmoke);
  * ListView1.SetColorItemSeparator(TAlphaColorRec.Red);
  * ListView1.SetColorText(TAlphaColorRec.Darkmagenta);
  * ListView1.SetColorTextSelected(TAlphaColorRec.Blueviolet);
  * ListView1.SetColorTextDetail(TAlphaColorRec.Darksalmon);
  * ListView1.SetColorHeader(TAlphaColorRec.Crimson);
  * ListView1.SetColorTextHeader(TAlphaColorRec.Whitesmoke);
  * ListView1.SetColorTextHeaderShadow(TAlphaColorRec.grey);
  * ListView1.SetColorPullRefresh(TAlphaColorRec.Lime);
  * ListView1.SetColorPullRefreshIndicator(TAlphaColorRec.Limegreen);
  * ListView1.SetColorStretchGlow(TAlphaColorRec.Limegreen);
  
###### set custom color for item
```  
  if ListView1.IsCustomColorUsed(ListView1.ItemIndex) then
    ListView1.SetDefaultColorForItem(ListView1.ItemIndex)
  else
    ListView1.SetCustomColorForItem(ListView1.ItemIndex, TAlphaColorF.Create(random(255) / 255, random(255) / 255, random(255) / 255, random(255) / 255).ToAlphaColor);
```

## Horizontal Mode
![horizontal mode](/screenshots/horizontal.png?raw=true)
  * ListView1.Horizontal := true;
  
## Columns Mode (only vertical)
![colorizer](/screenshots/autocolumns.png?raw=true)
  * ListView1.ColumnWidth := 160;
  * ListView1.AutoColumns := true;
  
## Events
event for AutoColumn mode
```
  procedure OnColumnClick(const Column: Integer; const X, Y: Single; const AItem: TListViewItem; const DrawebleName: string);
```
called when end of list
```
  procedure OnScrollEnd(Sender: TObject);
```
  
## Methods
  * Style for ListView Columns Mode
  * ListView1.ShowScrollBar - _hide/show scrollbar_
  * ListView1.ItemsClearTrue - _correct delete items_
  * ListView1.OffsetTop - _indent of the first element_
  * ListView1.OffsetBottom - _indent of the last element_
  * ListView1.getFirstVisibleItemIndex - _first visible ItemIndex_
  * ListView1.getVisibleCount - _amount of visible items_
  * ListView1.getLastVisibleItemindex - _first visible ItemIndex + amount of visible items_
  * ListView1.SeparatorLeftOffset - _indent for separator line_
  * ListView1.SeparatorRightOffset - _indent for separator line_
  * ListView1.EnableTouchAnimation - enable/disable touch animation
 
