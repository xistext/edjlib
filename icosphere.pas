unit icosphere;

{$mode ObjFPC}{$H+}

{
  TIcoSphere is a TCastleScene that wraps the builder if you want to add them
    to your castle scene/viewport/transform and use like TCastleSphere.

  TIcoIcoSphereBuilder
    builds a x3d TShape of an icosphere given a radius and recursion depth.
    depth=0 produces the base shape with 12 vertices and 20 triangles
    each depth subdivides each triangle into 4 triangles and doubles number of vertices
    Only use this directly if you want to build icospheres shapes directly into
    your x3d node structure.

  Core concept based on python code from
    http://blog.andreaskahler.com/2009/06/creating-icosphere-mesh-in-code.html

  Use as you see fit. No warranty. I am sure it could be better.
    erikquiet@gmail.com }

interface

uses
  Classes, Math,
  Generics.Collections,
  CastleVectors, CastleScene, CastleUtils,
  x3dNodes, x3dtools;

type
{ low level classes used internally while building the scene,
     TIcoSphere that wraps these, or you can use it to build shapes and materials
     directly for your x3d nodes }
  tpointcache = class( specialize tdictionary<int64,integer> );
  ticospherebuilder = class( tshapebuilder )
     public
     constructor create( iRadius : single;
                         iWiggle : single;
                         ipointlimit : integer );
     destructor destroy; override;

     procedure buildseed;

     function buildshape : TShapeNode;

     public

     Radius   : single;    { radius to build the shape }
     Wiggle   : single;    { randomamount to deform radius, reduces for depth }
     pointlimit : integer; { prototype/test limit for which points are built }

     private

     Level    : integer;
     middlePointIndexCache : tpointcache;
     function addVertex( const p : TVector3 ) : integer;
     function getMiddlePoint( p1, p2 : integer ) : integer;
     procedure refinetriangles( recursionlevel : integer );
   end;


{ this scene class wraps the builder, instantiate it as normal, then build with the
  desired parameters and add to your view/transform/scene }
type ticosphere = class( TCastleScene )

       procedure build( iradius : single;
                        recursionlevel : integer;
                        const iBaseColor : TVector3;
                        iTextureUrl : string = '';
                        iwiggle : single = 0;
                        ipointlimit : integer = 0 );

      end;

implementation

function calcKey( p1, p2 : integer ) : int64;
 var smallerIndex, greaterIndex : int64;
     p2_gt_p1 : boolean;
 begin
   p2_gt_p1 := p2 > p1;
   smallerindex := ord( p2_gt_p1 ) * p2 + ord( not p2_gt_p1 ) * p1;
   greaterindex := ord( p2_gt_p1 ) * p1 + ord( not p2_gt_p1 ) * p2;
   result := (smallerindex SHL 32 ) + greaterindex;
 end;

constructor ticospherebuilder.create( iRadius : single;
                                      iWiggle : single;
                                      ipointlimit : integer);
 begin
   inherited create;
   Level := 0;
   Wiggle := iWiggle;
   Radius := iRadius;
   BaseColor := vector3(0,0.5,0);
   pointlimit := ipointlimit;
   middlePointIndexCache := tpointcache.Create;
   buildseed;
 end;

destructor ticospherebuilder.destroy;
 begin
   inherited;
   middlePointIndexCache.Free;
 end;

procedure ticospherebuilder.buildseed;
 { build the core shape that will be subdivide.
   this builds a 20sided isohedron }
 var t : single;
 begin
   Vertices.Capacity := 12;
   Indexes.Capacity := 60;
   { initialize the core dodecehedron }
   t := (1.0 + sqrt(5.0))/2.0;
   AddVertex( Vector3( -1, t, 0 ));
   AddVertex( Vector3( 1, t, 0 ));
   AddVertex( Vector3( -1, -t, 0 ));
   AddVertex( Vector3( 1, -t, 0 ));

   AddVertex( Vector3( 0, -1, t ));
   AddVertex( Vector3( 0, 1, t ));
   AddVertex( Vector3( 0, -1, -t ));
   AddVertex( Vector3( 0, 1, -t ));

   AddVertex( Vector3( t, 0, -1 ));
   AddVertex( Vector3( t, 0, 1 ));
   AddVertex( Vector3( -t, 0, -1 ));
   AddVertex( Vector3( -t, 0, 1 ));

   { 5 faces around point 0 }
   Indexes.Add( 0 ); Indexes.Add( 11 ); Indexes.Add( 5 );
   Indexes.Add( 0 ); Indexes.Add( 5 ); Indexes.Add( 1 );
   Indexes.Add( 0 ); Indexes.Add( 1 ); Indexes.Add( 7 );
   Indexes.Add( 0 ); Indexes.Add( 7 ); Indexes.Add( 10 );
   Indexes.Add( 0 ); Indexes.Add( 10 ); Indexes.Add( 11 );
   If pointlimit = 1 then exit;
   { 5 adjacent faces }
   Indexes.Add( 1 ); Indexes.Add( 5 ); Indexes.Add( 9 );
   Indexes.Add( 5 ); Indexes.Add( 11 ); Indexes.Add( 4 );
   Indexes.Add( 11 ); Indexes.Add( 10); Indexes.Add( 2 );
   Indexes.Add( 10 ); Indexes.Add( 7 ); Indexes.Add( 6 );
   Indexes.Add( 7 ); Indexes.Add( 1 ); Indexes.Add( 8 );
   { 5 faces around point 3 }
   Indexes.Add( 3 ); Indexes.Add( 9 ); Indexes.Add( 4 );
   Indexes.Add( 3 ); Indexes.Add( 4 ); Indexes.Add( 2 );
   Indexes.Add( 3 ); Indexes.Add( 2 ); Indexes.Add( 6 );
   Indexes.Add( 3 ); Indexes.Add( 6 ); Indexes.Add( 8 );
   Indexes.Add( 3 ); Indexes.Add( 8 ); Indexes.Add( 9 );
   { 5 adjacent faces }
   Indexes.Add( 4 ); Indexes.Add( 9 ); Indexes.Add( 5 );
   Indexes.Add( 2 ); Indexes.Add( 4 ); Indexes.Add( 11 );
   Indexes.Add( 6 ); Indexes.Add( 2 ); Indexes.Add( 10 );
   Indexes.Add( 8 ); Indexes.Add( 6 ); Indexes.Add( 7 );
   Indexes.Add( 9 ); Indexes.Add( 8 ); Indexes.Add( 1 );
 end;

function ticospherebuilder.addVertex( const p : TVector3 ) : integer;
 var factor : single;
 begin
   factor := radius / sqrt( p.X * p.X + p.Y * p.Y + p.Z * p.Z );
   if wiggle > 0 then
    begin
      wiggle := 0.1 * sqrt( 4 - Level );
      factor := factor * ( 1 + Random * wiggle * 2 - wiggle );
    end;
   result := Vertices.Count;
   Vertices.Add( p * factor );
 end;

function ticospherebuilder.getMiddlePoint( p1, p2 : integer ) : integer;
 var key : int64;
     pointMiddle : TVector3;
 begin
   { look to cache by key }
   key := calcKey( p1, p2 );
   if not middlePointIndexCache.trygetvalue(key, result ) then
    begin { not in cache, calculate }
      pointMiddle := ( Vertices[p1] + Vertices[p2] ) / 2;
      result := AddVertex( pointMiddle );
      middlePointIndexCache.Add(key,result);
    end;
 end;

procedure ticospherebuilder.refinetriangles( recursionlevel : integer );
 var i, j, ix : integer;
     a, b, c, v1, v2, v3 : integer;
     index2 : TInt32List;
     lvl : integer;
 begin
   for lvl := 0 to recursionlevel - 1 do
    begin
      inc( level );
      index2 := TInt32List.create;
      index2.Capacity := Indexes.Count * 2; { double the vertices }
      j := 0;
      while j < Indexes.count do
       begin
         { replace triangle with 4 triangles }
         ix := j;
         v1 := indexes[ix]; v2 := indexes[ix+ 1]; v3 := indexes[ix + 2];
         a := getMiddlePoint( v1, v2);
         b := getMiddlePoint( v2, v3);
         c := getMiddlePoint( v3, v1);

         index2.add(v1); index2.add(a); index2.add(c);
         index2.add(v2); index2.add(b); index2.add(a);
         index2.add(v3); index2.add(c); index2.add(b);
         index2.add(a); index2.add(b); index2.add(c);
         j := j + 3;
       end;
      middlePointIndexCache.Clear;
      Indexes.Free;
      Indexes := index2;
    end;
 end;

function ticospherebuilder.buildshape : TShapeNode;
var Triangles : TIndexedTriangleSetNode;
    Material: TPhysicalMaterialNode;
    Appearance: TAppearanceNode;
    CoordinateNode : TCoordinateNode;
begin
  Result:= TShapeNode.Create;

  Triangles := TIndexedTriangleSetNode.Create;
  Triangles.SetIndex( Indexes );
  CoordinateNode := TCoordinateNode.Create;
  CoordinateNode.SetPoint( Vertices );
  Triangles.Coord := CoordinateNode;

  Result.Geometry := Triangles;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := buildmaterial;

  Result.Appearance := Appearance;
 end;

//-----------------

procedure ticosphere.build( iradius : single;
                            recursionlevel : integer;
                            const iBaseColor : TVector3;
                            iTextureUrl : string = '';
                            iwiggle : single = 0;
                            ipointlimit : integer = 0);
 var builder : ticospherebuilder;
     Root : TX3dRootNode;
 begin
   clear;
   { initialize the builder with build options and build ourself with it }
   builder := ticospherebuilder.create( iradius, iwiggle, ipointlimit );
   builder.basecolor := ibasecolor;
   builder.textureurl := iTextureUrl;
   builder.refinetriangles( recursionlevel );
   Root := TX3DRootNode.Create;
   Root.AddChildren( builder.buildshape );
   Load(Root, true );
   builder.free;
 end;

end.

