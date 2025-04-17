unit basethread;

interface

uses
  Classes, SysUtils,
  collect;

type TTaskThread = class;

     TThreadTask  = class

        parentthread : TTaskThread;
        procedure RunTask; virtual; abstract;

      end;

     TThreadTaskList = class( tcollection )
        locked : boolean;
        constructor create;
        procedure addtask( task : TThreadTask );
        function nexttask( var task : TThreadtask ) : boolean;
      end;

     TTaskThread = class( TThread )
        TaskList : TThreadTaskList;
      end;


implementation

constructor TThreadTaskList.create;
 begin
   inherited create;
   ownitems := true;
   locked := false;
 end;

procedure TThreadTaskList.addtask( task : TThreadTask );
 begin
   while locked do { another thread is adding a tile }
      sleep(1);
   locked := true;
   atinsert( 0, task );
   locked := false;
end;

function TThreadTaskList.nexttask( var task : TThreadTask ) : boolean;

 begin
   result := not locked and ( count > 0 );
   if result then
    begin
      locked := true;
      task := tthreadtask(at( count - 1 ));
      atdelete( count - 1 );
      locked := false;
    end;
 end;



end.

