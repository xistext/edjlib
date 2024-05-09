# edjlib
Object Pascal code I have shared on Castle Game Engine Forum.


x3dtools.pas
  A hodgepodge of classes and routines I used for x3d. Needed for Icosphere.
  Requires Castle Game Engine.

Icosphere.pas
  Core concept based on python code from
    http://blog.andreaskahler.com/2009/06/creating-icosphere-mesh-in-code.html
  Requires Castle Game Engine.
  Requires x3dtools.pas.

BehaviorTree.pas, basedata.pas
  Core concept based on
    https://www.gamedeveloper.com/programming/behavior-trees-for-ai-how-they-work

GPC.pas General Polygon Clipper. I ported to Delphi long long ago and used for decades
in aviation software so it is solid.  Uses all doubles, so not perfect for CGE but
I still find it useful.  It could be converted with some effort to use TVertex2.  
Lets you combine polygon in various ways to create complex polygons with holes and islands, etc.


