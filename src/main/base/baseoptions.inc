{$IFDEF CPUX64}
  //any 64-bit CPU
  {$DEFINE CPU64}
{$ENDIF}


{.$define WITHLOG}      // use log to consosole
{.$define PROF}         // use profiler

// BaseValue directives

{$define WithCS}       // use critical sections
{.$define SingleCS}     // use single CS
{$ifndef WithCS}

  {$define Use8bitHash}    // choise 8bit hash table
  {.$define Use16bitHash}   // using 16bit hash table
  {.$define Use20bitHash}   // using 20bit hash table

  {$ifdef Use8bitHash}
     {$undef Use20bitHash}{$undef Use16bitHash}
  {$endif}
  {$ifdef Use16bitHash}
     {$undef Use20bitHash}{$undef Use8bitHash}
  {$endif}
  {$ifdef Use20bitHash}
     {$undef Use16bitHash}{$undef Use8bitHash}
  {$endif}
 
  {.$define cahe}         // use cahe mechanism
  {.$define DeadLockCheck} // check for deadlock at execution time
  
  {.$define ExtCahe}      // use shared chahe - experimental
{$endif}

