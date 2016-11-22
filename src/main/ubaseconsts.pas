{
Copyright (C) 2016 Dmitry Muza <dmitry.muza@gmail.com>

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
}

unit uBaseConsts;

{$mode delphi}

interface

uses
  Classes, SysUtils;

const
  SListItemShouldSupportInterfaceFmt = 'List item should support %s';
  SPropReadOnly = 'Read Only';
  SBoundsError = 'Index out of bounds';
  SWeakPointErrorFmt = 'WeakRef points to interface that does not support support %s';
  SCantDoDueInterface = 'The observer does not support the necessary interface';
  SVariantNotSupprtedFmt = 'VarType %d not support';
  SCloneError = 'Clone error';
  SComparisonerror = 'Comparison error';
  SFunctionNameExpected = 'Function name expected';
  SSubOptionsError = 'Sub options not allowed';
  SUnexpectedBrace = 'Unexpected close brace occurred';
  SSyntaxError = 'Syntax error';
  SOutOfResources = 'Out Of resources';
  SUnsupportedFormatFmt = 'Unsupported format (VarType = %d)';
  SVariantCastErrorFmt = 'Can`t cast variant to "%s"';
  SVariantIntfError = 'Variant value is not interface';
  SListEmpty = 'List is empty';
  SListOutOfBoundsFmt = 'List Index (%d) out of bounds';
  SNotImplemented = 'Not yet implemented!';
  SConcurrencyError = 'Concurrency error';


const
  BaseFormatSettings: TFormatSettings = (CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: #0;
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'dd/mm/yyyy';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
    'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames: ('January', 'February', 'March', 'April', 'May',
    'June', 'July', 'August', 'September', 'October', 'November', 'December');
    ShortDayNames: ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames: ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday',
    'Friday', 'Saturday');
    TwoDigitYearCenturyWindow: 50; );


implementation

end.
