unit BehaviorFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleDownload,
  BehaviorTree, BehaviorRegister;

function writeBehaviorTreeToFile( BehaviorTree : TBehaviorNode;
                                  TreeName : string;
                                  fName : string ) : boolean;

function readBehaviorTreeFromFile( fName : string;
                                   out BehaviorTre : TBehaviorNode ) : boolean;

implementation

procedure writecallback( node : TBehaviorNode;
                         parent : TBehaviorNode;
                         data : pointer;
                         indent : integer );
 var line : string;
     i : integer;
 begin
   for i := 1 to indent do
     line := line + '   ';
   line := line + node.behaviorclass;
   if node.name <> '' then
      line := line + format(' "%s"', [node.name] );
   if node.comment <> '' then
      line := line + ' //'+node.comment;
   TTextWriter( data ).Writeln( line );
 end;

function writeBehaviorTreeToFile( BehaviorTree : TBehaviorNode;
                                  TreeName : string;
                                  fName : string ) : boolean;
 var Writer : TTextWriter;
 begin
   Writer := TTextWriter.create( fName );
   Writer.Writeln( 'Tree(''%s'')', [TreeName] );
   iteratetree( BehaviorTree, @writecallback, Writer );
   Writer.free;
 end;

function readBehaviorTreeFromFile( fName : string;
                                   out BehaviorTre : TBehaviorNode ) : boolean;
 var Reader : TTextReader;
 begin
   Reader := TTextReader.create( fName );

   Reader.Free;
 end;

end.

