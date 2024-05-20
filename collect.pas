UNIT Collect;

{ Collection classes for Delphi
  compatible with TPW<=7.0/Delphi1 OWL TCollection
    except for firstthat, foreach not implemented... (now can be just as fast with fcnt
  Alin Flaider, 1996 aflaidar@datalog.ro.

  1997 EDJ. made thread safe. Use ifdef threaded to enable thread safety for threaded applications
  2004 EDJ. Made more memory efficient.  24-28 byte overhead reduced to 12 byte.

  Compiles under D5..XE6.
  Can use QStrings under D5..D7 for optimized string compare
 }

{ Xistext }

INTERFACE

//{$ifdef lowdebug}{$D+}{$O-}{$else}{$O+}{$D-}{$endif}

uses Classes, Sysutils, nobjects;

const
  coIndexError = -1;              { Index out of range }
  coOverflow   = -2;              { Overflow }
  coUnderflow  = -3;              { Underflow }
  coWaitTimeout= -4;              { timeout while waiting for thread }

CONST { Tdcollection Bits }
//      CB_Sorted   = 1; no longer used, available
      CB_OwnItems = 2;
      CB_HaveObjs = 4;
      CB_Duplicates=8;            {used by sorted collection
                                   if true, rejects item whose key already exists}
                                  {override this method to specify relation bewtween two keys
                                  1 if Key1 comes after Key2, -1 if Key1 comes before Key2,
                                  0 if Key1 is equivalent to Key2}
(* not used yet      CB_FixedChildType=16; { used to know if a stream list needs to store the streamid
                              for each child or just once for the whole list }*)
      { 16,32,64,128 unused }

TYPE TCollection = CLASS;
     CollException = class(Exception);
     TInsertProc = PROCEDURE( List : TCollection; Item : Pointer );

     TCollectionInfo = PACKED RECORD { 64 bits; 8 bytes }
        Bits     : BYTE;    { Collection Bits defined above }
        Delta    : WORD;    { Number of items by which the collection grows when full }
        Reserved : BYTE;
        Limit    : INTEGER; { Current Allocated size of array }
      END;

     PPointerList = ^TPointerList;
     TPointerList = array[0..Maxint div 16 - 1] of Pointer;

 TCollection = CLASS( TdObject )
  public
    Count  : INTEGER;          { Current Number of Items, 8 bytes }
    It     : PPointerList;     { array of pointers }
  public
    constructor Create; OVERRIDE;
    constructor init( aLimit, aDelta : INTEGER );
    destructor  Destroy; override; {before deallocating object it disposes all items and the storage array}

    function  At( Index : INTEGER ) : Pointer;            { return item at index position }
    procedure AtPut( Index : INTEGER; Item : Pointer);    { replace item at index position}
    procedure AtInsert( Index : INTEGER; Item : Pointer); {inserts Item at specified position }

    procedure AtFree(Index: INTEGER);  {deletes and disposes Item at specified position}
    procedure FreeAll;     { deletes and disposes all items }

    procedure FreeItem      (Item : Pointer); virtual;

    procedure AtDelete(Index : INTEGER);
    procedure Delete( Item : Pointer); {deletes Item}
    procedure DeleteAll;   {deletes all Items without disposing them }

    procedure Pack;            {packs collection by removing nil Items}
    procedure Clear(Item: Pointer); {formerly Free, renamed to Clear to avoid bypassing inherited TObject.Free;
                                     deletes and disposes Item }

    function  IndexOf( Item : Pointer) : Integer; virtual;{finds position of Item using a linear search}

    property Items[Index: INTEGER]: pointer read At write AtPut; default; {direct access to items through position}

    FUNCTION FirstPtr : POINTER; { returns a pointer to first item, for walking list }
    FUNCTION FCnt( VAR FirstP ) : INTEGER;  { returns a pointer to first item, for walking list }

    PROCEDURE Insert  (Item : Pointer);

    PROTECTED

    InsertProc : TInsertProc;

    PROCEDURE SetLimit( aLimit : INTEGER );

    PRIVATE

    FInfo   : TCollectionInfo;  { Collection information Record }

    PROCEDURE SetSorted( iSorted : BOOLEAN );
    FUNCTION GetSorted : BOOLEAN;

    PROCEDURE SetOwnItems( iOwnItems : BOOLEAN );
    FUNCTION GetOwnItems : BOOLEAN;

    PROCEDURE SetDuplicates( iDuplicates: BOOLEAN );
    FUNCTION GetDuplicates : BOOLEAN;

    PROCEDURE SetHaveObjs( iHaveObjs : BOOLEAN );
    FUNCTION GetHaveObjs : BOOLEAN;

    PROCEDURE Error( Code,Info : Integer); DYNAMIC;

    FUNCTION Grow : BOOLEAN;

    PUBLIC

    PROPERTY Sorted : BOOLEAN READ GetSorted WRITE SetSorted;
    PROPERTY OwnItems : BOOLEAN READ GetOwnItems WRITE SetOwnItems;
    PROPERTY Duplicates : BOOLEAN READ GetDuplicates WRITE SetDuplicates;
    PROPERTY HaveObjs : BOOLEAN READ GetHaveObjs WRITE SetHaveObjs;
    PROPERTY Bits : BYTE READ FInfo.Bits WRITE FInfo.Bits;
    PROPERTY Delta : WORD READ FInfo.Delta;
    PROPERTY Limit : INTEGER Read FInfo.Limit;


 END;

 TSortedCollection = class(TCollection)
    constructor Create; OVERRIDE;
    constructor init( aLimit, aDelta : INTEGER );
    { Duplicates: boolean; now in Bits }
    function IndexOf (Item : Pointer): Integer; override;
                               {finds item required position and performs insertion }
//    procedure Insert  (Item : Pointer); override;
                               {finds index of item by performing an optimised search}
    function Search  (key : Pointer; Var Index : integer) : Boolean;

    function KeyOf   (Item : Pointer): Pointer; virtual;  {finds index of item by calling Search}
    function Compare (Key1,Key2 : Pointer): Integer; virtual; {returns key of Item}

 END;

 TStrCollection = CLASS( TSortedCollection )

   function Compare(Key1, Key2: Pointer): Integer;   override;
   procedure FreeItem(Item: Pointer);                override;

 END;

IMPLEMENTATION {---------------------------------------------------------------}

   PROCEDURE InsertToEnd( List : TCollection; Item : Pointer );
    BEGIN
      WITH List DO
         AtInsert( Count, Item );
    END;

   PROCEDURE InsertSorted( List : TCollection; Item : Pointer);
   VAR I : INTEGER;
    BEGIN
      {$ifdef threaded}IF ThreadLock THEN{$endif} WITH TSortedCollection( List ) DO
       BEGIN
         IF NOT Search(KeyOf(Item), I) OR ( FInfo.Bits AND CB_Duplicates > 0 ) THEN
            AtInsert(I, Item);
         {$ifdef threaded}ThreadRelease;{$endif}
       END
    END;

constructor TCollection.Create;
 begin
   inherited Create;
   Count:=0;
   It := nil;

   {$ifdef QStrings}
   Q_ZeroMem( @Info, SizeOf( Info ));
   {$else}
   FillChar( FInfo, SizeOf( FInfo ), 0 );
   {$endif}

   FInfo.Delta:=10;
   FInfo.Bits := cb_OwnItems OR cb_HaveObjs;
   SetLimit( 1 );
   InsertProc := @InsertToEnd;
 end;

constructor TCollection.Init( ALimit, ADelta: INTEGER );
 begin
   inherited Create;
   Count:=0;
   It := nil;
   {$ifdef QStrings}
   Q_ZeroMem( @Info, SizeOf( Info ));
   {$else}
   FillChar( FInfo, SizeOf( FInfo ), 0 );
   {$endif}

   FInfo.Delta := aDelta;
   FInfo.Bits := cb_OwnItems OR cb_HaveObjs;
   SetLimit( ALimit );
   InsertProc := @InsertToEnd;
 end;

destructor TCollection.Destroy;
 begin
   FreeAll;
   SetLimit(0);
   inherited Destroy;
 end;

function TCollection.At(Index: INTEGER ): Pointer;
 begin
   Result := NIL;
   If ( Index < Count ) THEN
    BEGIN
      {$ifdef threaded}IF ThreadLock THEN{$endif}
       BEGIN
         Result := It^[Index];
         {$ifdef threaded}ThreadRelease;{$endif}
       END
    END
 end;

procedure TCollection.AtPut(Index: INTEGER; Item: Pointer);
 begin
   if (Index >= Count) then
     Error(coIndexError,0)
   else
     {$ifdef threaded}IF ThreadLock THEN{$endif}
      BEGIN
        It^[Index] := Item;
        {$ifdef threaded}ThreadRelease;{$endif}
      END;
 end;

procedure TCollection.AtDelete(Index: INTEGER);
 { removes an index from the collection }
 begin
   if (Index >= Count) then
      Error(coIndexError,0)
   else
    begin
      {$ifdef threaded}IF ThreadLock THEN BEGIN{$endif}
         if Index < pred(Count) then
            {$ifdef QStrings}
            Q_MoveMem( @It^[succ(Index)], @It^[Index], (count-index)*sizeof(pointer));
            {$else}
            Move( It^[succ(Index)], It^[Index], (count-index)*sizeof(pointer));
            {$endif}
         Dec(Count);
      {$ifdef threaded}ThreadRelease;END{$endif}
    end
 end;

FUNCTION TCollection.Grow : BOOLEAN;
 BEGIN
   WITH FInfo DO
    BEGIN
      Result := Count < Limit; { return true if we don't need to grow }
      IF NOT Result THEN
       BEGIN
         Result := Delta > 0; { return false if we need to grow but can't }
         IF Result THEn
            SetLimit( Limit + Delta ); { return true if we grew successfully }
       END;
    END;
 END;

procedure TCollection.AtInsert( Index: INTEGER; Item: pointer);
begin
   if ( Index > Count ) then
      Index := Count;
   IF NOT Grow THEN
    BEGIN
      Error(coOverFlow,0);
      exit;
    end;
   {$ifdef threaded}IF ThreadLock THEN{$endif}
    BEGIN
      If Index <> Count then  {move compensates for overlaps}
         {$ifdef QStrings}
         Q_MoveMem( @It^[Index], @It^[Index+1], (count - index)*sizeof(pointer));
         {$else}
         Move( It^[Index], It^[Index+1], (count - index)*sizeof(pointer));
         {$endif}
      It^[Index] := Item;
      Inc(Count);
      {$ifdef threaded}ThreadRelease;{$endif}
    END
end;

procedure TCollection.Delete( Item: pointer);
 VAR Ix : INTEGER;
 begin
   Ix := IndexOf( Item );
   IF Ix > -1 THEN
      AtDelete( Ix );
 end;

procedure TCollection.DeleteAll;
 begin
   Count:=0
 end;

FUNCTION TCollection.GetSorted : BOOLEAN;
 BEGIN
//   Result := Info.Bits AND CB_Sorted > 0;
   Result := (InsertProc = @InsertSorted)
 END;

PROCEDURE TCollection.SetSorted( iSorted : BOOLEAN );
{ VAR IsSorted : BOOLEAN;}
 BEGIN
(*   IsSorted := Info.Bits AND CB_Sorted > 0;
   IF IsSorted XOR iSorted THEN { if new setting is different }
    BEGIN
      IF iSorted THEN
       BEGIN
         Info.Bits := Info.Bits OR CB_Sorted;{ turn on }
         InsertProc := InsertSorted
       END
      ELSE
       BEGIN
         Info.Bits := Info.Bits XOR CB_Sorted; { turn off }
         InsertProc := InsertToEnd
       END
    END;*)
   IF ISorted THEN
      InsertProc := @InsertSorted
   ELSE
      InsertProc := @InsertToEnd;
 END;

FUNCTION TCollection.GetOwnItems : BOOLEAN;
 BEGIN
   Result := FInfo.Bits AND CB_OwnItems > 0;
 END;

PROCEDURE TCollection.SetOwnItems( iOwnItems : BOOLEAN );
 VAR DoOwnItems : BOOLEAN;
 BEGIN
   DoOwnItems := FInfo.Bits AND CB_OwnItems > 0;
   IF DoOwnItems XOR iOwnItems THEN { if new setting is different }
    BEGIN
      IF iOwnItems THEN
         FInfo.Bits := FInfo.Bits OR CB_OwnItems { turn on }
      ELSE
         FInfo.Bits := FInfo.Bits XOR CB_OwnItems; { turn off }
    END;
 END;

FUNCTION TCollection.GetHaveObjs : BOOLEAN;
 BEGIN
   Result := FInfo.Bits AND CB_HaveObjs > 0;
 END;

PROCEDURE TCollection.SetHaveObjs( iHaveObjs : BOOLEAN );
 VAR DoHaveObjs : BOOLEAN;
 BEGIN
   DoHaveObjs:= FInfo.Bits AND CB_HaveObjs > 0;
   IF DoHaveObjs XOR iHaveObjs THEN { if new setting is different }
    BEGIN
      IF iHaveObjs THEN
         FInfo.Bits := FInfo.Bits OR CB_HaveObjs { turn on }
      ELSE
         FInfo.Bits := FInfo.Bits XOR CB_HaveObjs ; { turn off }
    END;
 END;


FUNCTION TCollection.GetDuplicates : BOOLEAN;
 BEGIN
   Result := FInfo.Bits AND CB_Duplicates > 0;
 END;

PROCEDURE TCollection.SetDuplicates( iDuplicates : BOOLEAN );
 VAR HasDuplicates : BOOLEAN;
 BEGIN
   HasDuplicates := FInfo.Bits AND CB_Duplicates > 0;
   IF HasDuplicates XOR iDuplicates THEN { if new setting is different }
    BEGIN
      IF iDuplicates THEN
         FInfo.Bits := FInfo.Bits OR CB_Duplicates { turn on }
      ELSE
         FInfo.Bits := FInfo.Bits XOR CB_Duplicates; { turn off }
    END;
 END;

procedure TCollection.Error(Code, Info: Integer);
 begin
   case Code of
        coIndexError: raise CollException.Create(ClassName+' error; bad index: '+IntToStr(Info));
        coOverflow:  raise CollException.Create(ClassName+' overflow - can''t grow!');
        coUnderflow: raise CollException.Create(ClassName+' underflow - can''t shrink!');
   end
 end;

procedure TCollection.Clear(Item: Pointer);
 begin
   Delete(Item);
   FreeItem(Item);
 end;

procedure TCollection.FreeAll;
 var Item : ^TObject;
     I : INTEGER;
 begin
   {$ifdef threaded}IF ThreadLock THEN BEGIN{$endif}
      FOR I := FCnt( Item ) DOWNTO 0 DO
       BEGIN
         FreeItem( Item^ );
         INC( Item );
       END;
      Count := 0;
   {$ifdef threaded}ThreadRelease; END{$endif}
 end;

procedure TCollection.FreeItem(Item: Pointer);
begin
  if ASSIGNED( Item ) AND OwnItems THEN
     TObject(Item).Free;
end;

function TCollection.IndexOf(Item: Pointer): integer;
var i : integer;
    It1 : ^TObject;
begin
  Result := -1;
  {$ifdef threaded}IF ThreadLock THEN{$endif}
   BEGIN
     for i := 0 TO FCnt( It1 ) DO
      BEGIN
        if Item = Pointer(It1^) then
         begin
           Result := i;
           break
         end;
        INC( It1 );
      END;
     {$ifdef threaded}ThreadRelease;{$endif}
   END
end;

procedure TCollection.Pack;
var i: integer;
begin
  {$ifdef threaded}IF ThreadLock THEN{$endif}
   BEGIN
     for i := pred(count) downto 0 do if It^[i] = nil then AtDelete(i);
     {$ifdef threaded}ThreadRelease;{$endif}
   END
end;

procedure TCollection.SetLimit( ALimit: INTEGER );
 begin
   if (ALimit < Count) then
      Error( coUnderFlow , 0)
   ELSE
   WITH FInfo DO IF ( ALimit <> Limit ) THEN
    BEGIN
     {$ifdef threaded}IF ThreadLock THEN BEGIN{$endif}
        { reallocate the memory, compiler platform specific }
        ReallocMem( It, ALimit* SizeOf(Pointer));
        Limit := ALimit;
     {$ifdef threaded}ThreadRelease; END{$endif}
   END;
 end;

FUNCTION TCollection.FirstPtr : POINTER;
 { returns a pointer to first item, for walking list.
   This has been replaced by the more versatile FCnt }
 BEGIN
   Result := NIL;
   IF Count > 0 THEN
      Result := @It^
 END;

FUNCTION TCollection.FCnt( VAR FirstP { untyped pointer} ) : INTEGER;
 { Used for walking list in a FOR loop ....
   FOR I := TheList.FCnt( APtr ) DOWNTO 0 DO
    BEGIN
      APtr^.Blah; do something with the item.
      INC( APtr );
    END; }
 BEGIN
   Result := Count - 1;
   IF Result >= 0 THEN
      POINTER( FirstP ) := @It^;  { return pointer to the first item in list }
 END;

PROCEDURE TCollection.Insert  (Item : Pointer);
 begin
   insertproc( self, item );
 end;


{-------------------------------------}
constructor TSortedCollection.Create;
 BEGIN
   INHERITED Create;
   InsertPRoc := @InsertSorted;
 END;

constructor TSortedCollection.init( aLimit, aDelta : INTEGER );
 BEGIN
   INHERITED Init( aLimit, aDelta );
   InsertProc := @InsertSorted;
 END;

function TSortedCollection.Compare( Key1,Key2 : Pointer): Integer;
begin
  Result := 0;
end;

function TSortedCollection.IndexOf(Item: Pointer): Integer;
var i: Integer;
begin
  Result := -1;
  if Search(KeyOf(Item), i) then { use keyof on item to short circuit sorted search }
  begin
    if ( FInfo.Bits AND CB_Duplicates > 0 ) then
      while (i < Count) and (Item <> It^[I]) do
         Inc(i);
    if i < Count then
       Result := i;
  end
  else
     Result := INHERITED IndexOf( Item ); { if not found by key, brute force search for pointer }
end;
                              (*
procedure TSortedCollection.Insert(Item: Pointer);
var i : integer;
begin
  {$ifdef threaded}IF ThreadLock THEN{$endif}
   BEGIN
     if not Search(KeyOf(Item), I) or ( Info.Bits AND CB_Duplicates > 0 ) then
        AtInsert(I, Item);
     {$ifdef threaded}ThreadRelease;{$endif}
   END
end;
                                *)
function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  Result := Item;
end;

function TSortedCollection.Search(key : Pointer; Var Index : integer) : Boolean;
var L, H, I, C: Integer;
    Dups : BOOLEAN;
begin
  Result := False;
  {$ifdef threaded}IF ThreadLock THEN BEGIN {$endif}
     Duplicates := FInfo.Bits AND CB_Duplicates > 1;
     L := 0;
     H := Count - 1;
     while L <= H do
      begin
        I := (L + H) shr 1; { div 2 }
        C := Compare(KeyOf(It^[I]), Key);
        if C < 0 then
           L := I + 1
        else
         begin
           H := I - 1;
           Result := C = 0;
           IF Result AND NOT Dups THEN
              L := I;
         end;
      end;
     Index := L;
     { !!! if duplicates, should we arrange to return the first of the duplicates ?}
  {$ifdef threaded}ThreadRelease; END;{$endif}
end;



procedure TCollection.AtFree(Index: INTEGER);
var Item: Pointer;
begin
  Item := At(Index);
  AtDelete(Index);
  FreeItem(Item);
end;

{-------------------------------------}

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
   {$ifdef QStrings}
   Result := Q_PCompStr(Key1, Key2);
   {$else}
   Result := StrComp( PChar( Key1 ), PChar( Key2 ));
   {$endif}
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(PChar(Item));
end;


END.
