unit basetools;

//{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  CastleQuaternions, CastleVectors, CastleTransform;

const zero : single = 0;
      twopi: single = 2 * Pi;
      { height lookup types }
      ht_terrain = 0;
      ht_road    = 1;
      ht_water   = 2;

type
     THeightAboveTerrainEvent = function ( Pos: TVector3;
                                           out Y: Single;
                                           heighttype : byte = ht_terrain ): boolean of object;
     PVector3 = ^TVector3;
     TPoly2 = Array of TVector2;
     TPoly3 = Array of TVector3;
     TBuildOptions = record

        LOD : integer; { Level of detail. 0=lowest level }
        terrainheight : THeightAboveTerrainEvent;
        shownodes     : boolean;

      end;

{ low level functions used by Live* }

function randomrange( range : single ) : single;
function cbrt( x : double ) : double;
function Get2DHeading(const d: TVector3): Single;
function Get2DPitch(const d:TVector3): single;
function RotateVectorY(const Vector: TVector3;
                            Angle : Single) : TVector3;
function RotateVectorXY(const Vector: TVector3;
                             AngleX, AngleY: single): TVector3;
function CalcRotationVector( angle, rotate, camber : single ) : TVector4; overload;
function CalcRotationVector( angle, rotate : single ) : TVector4; overload;
function valueminusminus( var value : integer ) : integer;
function anglefromdelta( const delta : TVector2 ) : single;

procedure rotatetomatchterrain( item : TCastleTransform;
                                heading : single;
                                terrainheight : THeightAboveTerrainEvent;
                                MatchCamber : boolean = true);
procedure calccorners( const p : TVector2;
                       angle, w2 : single;
                       var L, R : TVector3 ); overload;
procedure calccorners( const p : TVector2;
                       angle, w2 : single;
                       var L, R : TVector2 ); overload;
function LineSegmentsIntersect(const A1, A2, B1, B2: TVector2;
                               var Intersection : TVector2;
                               var IntersectionValid : boolean ): Boolean;
function LineSegmentDist(const A, B, C : TVector2;
                         var P : TVector2):single;
function PointDist( const A, B : TVector2 ): single;
function polysintersect( poly1, poly2 : TPoly2 ) : boolean;
function combinerotations( const r1, r2 : TVector4 ) : TVector4;

function ClosestPointOnSegment(const p0, p1, p2: TVector2): TVector2;
function ClosestPointOnArc(const Center: TVector2;
                           Radius: single;
                           Dir : shortint;
                           StartAngle, EndAngle: single;
                           const TargetPoint: TVector2): TVector2;

function normalizeheading( heading : single ): single;

procedure updateminmax( value : single;
                        var min : single;
                        var max : single ); overload;
procedure updateminmax( const pt : TVector3;
                        var min : TVector2;
                        var max : TVector2 ); overload;

procedure limitmax( var value : single;
                     max   : single );
procedure limitminmax( var value : single;
                       min, max : single );
procedure limitmin( var value : single;
                        min   : single ); overload;
procedure limitmin( var value : smallint;
                        min   : smallint ); overload;

function comparesingle( s1, s2 : single ) : integer;
function delta2_3( const p1, p2 : tvector3 ) : tvector2;

implementation //===============================================================

uses CastleUtils;

function Get2DHeading(const d: TVector3): Single;
 begin
   result := arctan2(d.x, d.z);
 end;

function Get2DPitch(const d: TVector3): Single;
 begin
   result := arctan2(d.y, hypot(d.x,d.z));
 end;

function ClosestPointOnSegment(const p0, p1, p2: TVector2): TVector2;
 var v, w : TVector2;
     t: Single;
 begin
   v := p1 - p0;
   w := p2 - p0;
   t := TVector2.DotProduct(w, v) / TVector2.DotProduct(v, v);
   ClampVar(t, 0, 1);
   Result := p0 + t * v;
 end;

function randomrange( range : single ) : single;
 begin
   result := random * range - range * 0.5;
 end;

function cbrt( x : double ) : double;
 begin
   result := exp(ln(x) * 0.333333 );
 end;

function RotateVectorY(const Vector: TVector3;
                            Angle : Single) : TVector3;
{ rotate around Y axis, used for determining rotation axis on rotates }
var CosTheta, SinTheta: Single;
begin
 SinCos( Angle, CosTheta, SinTheta );
 with vector do  { Apply the rotation matrix }
    Result := Vector3( X * CosTheta + Z * SinTheta,
                       Y,
                      -X * SinTheta + Z * CosTheta );
end;

function RotateVectorXY(const Vector: TVector3;
                             AngleX, AngleY: single): TVector3;
var CosX, SinX, CosY, SinY: single;
begin
 SinCos(AngleX, SinX, CosX);
 Sincos(AngleY, SinY, CosY);
 with Vector do { Apply the rotation matrices }
    Result := Vector3( X * CosY + Z * SinY,
                       X * SinX * SinY + Y * CosX - Z * SinX * CosY,
                      -X * CosX * SinY + Y * SinX + Z * CosX * CosY );
end;

function CalcRotationVector( angle, rotate, camber: single ) : TVector4; overload;
 var q1, q2, q3, qc : TQuaternion;
     axis : TVector3;
 begin
   q1 := QuatFromAxisAngle(Vector3(0, 1, 0), rotate );
   q2 := QuatFromAxisAngle(Vector3(1, 0, 0), angle );
   q3 := QuatFromAxisAngle(Vector3(0, 0, 1), camber );
   qc := q1 * q2 * q3;
   qc.ToAxisAngle(Axis, angle);
   Result := Vector4( Axis.x, axis.y, axis.z, angle );
 end;

function CalcRotationVector( angle, rotate : single ) : TVector4; overload;
 var q1, q2, qc : TQuaternion;
     axis : TVector3;
 begin
   q1 := QuatFromAxisAngle(Vector3(0, 1, 0), rotate );
   q2 := QuatFromAxisAngle(Vector3(1, 0, 0), angle );
   qc := q1 * q2;
   qc.ToAxisAngle(Axis, angle);
   Result := Vector4( Axis.x, axis.y, axis.z, angle );
 end;

function valueminusminus( var value : integer ) : integer;
 begin
   value := value - 1;
   Result := value;
 end;

function CalcDirectionVector( angle, rotate : single ) : TVector3;
  begin
    Result := RotateVectorXY(Vector3(0, 1, 0), angle, rotate);
  end;

function anglefromdelta( const delta : TVector2 ) : single;
 begin
   if abs( delta.x ) > 0 then
      Result := arctan2(delta.y,delta.x)
   else
       result := 0;
 end;

function combinerotations( const r1, r2 : TVector4 ) : TVector4;
 var q1, q2, qc : TQuaternion;
     angle : single;
     axis : TVector3;
 begin
   q1 := QuatFromAxisAngle( r1 );
   q2 := QuatFromAxisAngle( r2 );
   qc := q1 * q2;
   qc.ToAxisAngle(Axis, angle);
   Result := Vector4( Axis.x, axis.y, axis.z, angle );
 end;


procedure rotatetomatchterrain( item : TCastleTransform;
                                heading : single;
                                terrainheight : THeightAboveTerrainEvent;
                                MatchCamber : boolean = true);
 var p1, p2 : TVector3;
     length, width, l2, w2 : single;
     angle, camber : single;
     newpos : TVector3;
     d2 : TVector2;
     sinh, cosh : single;
     delta : TVector2;
begin
   length := ( item.LocalBoundingBox.Max.z - item.localboundingBox.Min.z ) * item.Scale.z;
   width := ( item.LocalBoundingBox.Max.x - item.localboundingBox.Min.x ) * item.Scale.x;
   assert( length > 0 );
   l2 := length*0.5;
   w2 := width *0.5;
   newpos := item.translation;

   sincos(heading, sinh, cosh );
   d2 := Vector2(sinh * l2, cosh * l2 );
   p1 := vector3( newpos.x + d2.x, item.translation.y, newpos.z + d2.y ); { front position }
   p2 := vector3( newpos.x - d2.x, item.translation.y, newpos.z - d2.y );  { rear position }
   { determine front and back point and their heights on terrain }
    terrainheight( p1, p1.y, ht_road );
    terrainheight( p2, p2.y, ht_road );

    { calculate the angle forward/back to match the terrain}
    delta := Vector2( length, p2.y - p1.y );
    angle := anglefromdelta( delta );

    { calculate the camber left/right to match the terrain }
    if MatchCamber then
     begin
       sincos(heading + Pi/2, sinh, cosh );
       d2 := Vector2(sinh * w2, cosh * w2 );
       p1 := vector3( newpos.x + d2.x, item.translation.y, newpos.z + d2.y ); { left position }
       p2 := vector3( newpos.x - d2.x, item.translation.y, newpos.z - d2.y ); { right position }
       { determine left and right point and their heights on terrain }
       terrainheight( p1, p1.y, ht_road );             {right}
       terrainheight( p2, p2.y, ht_road );             {left}

       { calculate the camber left/right to match the terrain}
       delta := Vector2( width, p2.y - p1.y );
       camber := -anglefromdelta( delta );
       Item.Rotation := CalcRotationVector( angle, heading, camber );
     end
    else
       Item.Rotation := CalcRotationVector( angle, heading );
 end;

procedure calccorners( const p : TVector2;
                       angle, w2 : single;
                       var L, R : TVector3 );
 { given a point, an angle and a distance, calculate
   the cornerpoints 90deg from the angle at that distance
   p is vector2, returns L and R which are vector 3 without Y set}
 var sina, cosa, dx, dy : single;
 begin
   sincos( angle, sina, cosa );
   dx := cosa * w2;
   dy := sina * w2;
   L := Vector3( p.x + dx, 0, p.y + dy );
   R := Vector3( p.x - dx, 0, p.y - dy );
 end;

procedure calccorners( const p : TVector2;
                       angle, w2 : single;
                       var L, R : TVector2 );
 { given a point, an angle and a distance, calculate
   the cornerpoints 90deg from the angle at that distance
   p is vector2, returns L and R which are vector 3 without Y set}
 var sina, cosa, dx, dy : single;
 begin
   sincos( angle, sina, cosa );
   dx := cosa * w2;
   dy := sina * w2;
   L := Vector2( p.x + dx, p.y + dy );
   R := Vector2( p.x - dx, p.y - dy );
 end;

function isbetween( a, b, c : single ): boolean;
 begin
   result := (( c >= a ) and ( c <= b )) or
             (( c <= a ) and ( c >= b ));
 end;

function LineSegmentsIntersect(const A1, A2, B1, B2: TVector2;
                               var Intersection : TVector2;
                               var IntersectionValid : boolean ): Boolean;
 var x12, x34, y12, y34, a, b,c : single;
 begin
   x12 := a1.x - a2.x;
   x34 := b1.x - b2.x;
   y12 := a1.y - a2.y;
   y34 := b1.y - b2.y;
   c := x12 * y34 - y12 * x34;
   intersectionvalid := abs(c) > 0.001;
   if not intersectionvalid then
    begin
      intersection := vector2( 0, 0 );
      result := false;
      exit;
    end;
   a := a1.x * a2.y - a1.y * a2.x;
   b := b1.x * b2.y - b1.y * b2.x;
   c := 1/c;
   intersection := Vector2(( a * x34 - b * x12 ) * c, (a * y34 - b * y12 ) * c);
   with intersection do
      Result := isbetween( a1.x, a2.x, x ) and isbetween( a1.y, a2.y, y ) and
                isbetween( b1.x, b2.x, x ) and isbetween( b1.y, b2.y, y )
end;

function LineSegmentDist(const A, B, C : TVector2;      {? not used}
                         var P : TVector2):single;
var Lsqr, r, s, Lsqr1div : single;
    r_lte_0, r_gte_1, r_lt_0, r_gt_1 : boolean;
    whichcase : integer;
begin
  Result := 0;
  Lsqr := sqr( B.X - A.X ) + sqr( B.Y - A.Y );
  if Lsqr = 0 then
   begin { A & B are same point }
     Result := hypot( C.X - A.X, C.Y - A.Y );
     P := A;
   end
  else
   begin
     Lsqr1div := 1/Lsqr;
     r := ((A.Y-C.Y)*(A.Y-B.Y) - (A.X-C.X)*(B.X-A.X))*Lsqr1div;
     r_lt_0 := r < 0;
     r_lte_0 := r_lt_0 or ( r = 0 );
     r_gt_1 := not r_lte_0 and ( r > 1 );
     r_gte_1 := r_gt_1 or ( r = 1 );
     whichcase := ord( r_lte_0 ) + ord( r_lt_0 ) shl 2 +
                  ord( r_gte_1 ) shl 3 + ord( r_gt_1 ) shl 4;
     case whichcase of
       1 : { r_lte_0, not r_lt_0, any }
            P := A;
       3 : { r_lte_0, r_lt_0 }
          begin
            Result := hypot( C.X - A.X, C.Y - A.Y );
            P := A;
          end;
       4 : { r_gte_1, not r_gt_1 }
           P := B;
       12 : { r_gte_1, r_gt_1 }
         begin
           Result := hypot( C.X - B.X, C.Y - B.Y );
           P := B;
         end;
       0 : { not r_lte_0, not r_gte_1 }
          begin
            P := Vector2( A.X + r*(B.X-A.X), A.Y + r*(B.Y-A.Y));
            s := abs(((A.Y-C.Y)*(B.X-A.X) - (A.X-C.X)*(B.Y-A.Y))*Lsqr1div);
            Result := s*sqrt(Lsqr);
          end;
     end;
   end
end;

function PointDist( const A, B : TVector2 ): single;
 begin
   result := hypot( B.X - A.X, B.Y - A.Y );
 end;


function polysintersect( poly1, poly2 : TPoly2 ) : boolean;
 var c1, c2, i1, i2 : integer;
     lp1, p1, lp2, p2 : TVector2;
     i : TVector2;
     intersectionvalid : boolean;
 begin
   result := true;
   c1 := length( poly1 )-1;
   c2 := length( poly2 )-1;
   if ( c1 > 0 ) and ( c2 > 0 ) then
    begin
     lp1 := poly1[c1];
     for i1 := 0 to c1 do
      begin
        p1 := poly1[i1];
        lp2 := poly2[c2];
        for i2 := 0 to c2 do
         begin
           p2 := poly2[i2];
           if LineSegmentsIntersect( lp1, p1, lp2, p2, i, intersectionvalid ) then
              exit;
           lp2 := p2;
         end;
        lp1 := p1;
      end;
    end;
   result := false;
 end;

function normalizeheading( heading : single ): single;
 var toobig, toosmall : boolean;
 begin
   toobig := heading >= Pi;
   toosmall := heading <= -Pi;
   case ord( toosmall ) + ord( toobig ) shl 2 of
     1 : heading := heading + twopi; { toosmall }
     2 : heading := heading - twopi; { toobig }
     3 : assert(false); { toobig + toosmall shouldn't happen }
    end;
   result := heading;
 end;

function ClosestPointOnArc(const Center: TVector2; Radius: single; Dir : shortint;
  StartAngle, EndAngle: single; const TargetPoint: TVector2): TVector2;
var
  Angle: Single;
  Delta: TVector2;
  sina, cosa : single;
begin
  Delta := TargetPoint - Center; { Calculate the vector from the center to the target point }
  with Delta do { Calculate the angle between the target point and the arc's center }
     Angle := NormalizeHeading(ArcTan2(Y, X));

  if startangle < -Pi then
   begin
     startangle := startangle + twopi;
     endangle := endangle + twopi;
   end
  else
  if startangle > Pi then
   begin
     startangle := startangle - twopi;
     endangle := endangle - twopi;
   end;

  { Adjust the angle based on the specified direction }
  if Dir < 0 then
   begin
     angle := NormalizeHeading( angle - Pi ); //??? why is this kludge necessary
     if angle > startangle then
        angle := angle - twopi;
     limitminmax( angle, endangle, startangle );
   end
  else
  begin
    if angle < startangle then
       angle := angle + twopi;
    limitminmax( angle, startangle, endangle );
  end;

  // Calculate the coordinates of the closest point on the arc
  Radius := -Dir * Radius;
  sincos( Angle, sina, cosa );
  Result := vector2( Center.X + Radius * cosa, Center.Y + Radius * sina );
end;

procedure updateminmax( value : single;
                        var min : single;
                        var max : single ); overload;
 var belowmin, abovemax : boolean;
 begin
   belowmin := value < min;
   abovemax := value > max;
   min := ord( belowmin ) * value + ord( not belowmin ) * min; { set min.x to pt.x if lower }
   max := ord( abovemax ) * value + ord( not abovemax ) * max;
 end;

procedure updateminmax( const pt : TVector3;
                        var min : TVector2;
                        var max : TVector2 ); overload;
 begin
   updateminmax( pt.x, min.x, max.x );
   updateminmax( pt.z, min.y, max.y )
 end;

{ branchless limit procedures }

procedure limitmin( var value : single;
                        min   : single );
 var limit : boolean;
 begin
   limit := value < min;
   value := ord( not limit ) * value + ord( limit ) * min;
 end;

procedure limitmax( var value : single;
                    max   : single );
 var limit : boolean;
 begin
   limit := value > max;
   value := ord( not limit ) * value + ord( limit ) * max;
 end;

procedure limitminmax( var value : single;
                          min, max : single );
 var limitmax, limitmin : boolean;
 begin
   limitmax := value > max;
   limitmin := value < min;
   value := ord( not limitmax and not limitmin ) * value +
            ord( limitmax ) * max +
            ord( limitmin ) * min;
 end;

procedure limitmin( var value : smallint;
                        min   : smallint ); overload;
 var limit : boolean;
 begin
   limit := value < min;
   value := ord( not limit ) * value + ord( limit ) * min;
 end;

function comparesingle( s1, s2 : single ) : integer;
 { s1 > s2 : 1
   s1 < s2 : -1
   s1 = s2 : 0 }
 begin
   result := ord( s1 > s2 ) - ord( s1 < s2 );
 end;

function delta2_3( const p1, p2 : tvector3 ) : tvector2;
 begin
   result := vector2( p2.X - p1.X, p2.Z - p1.Z );
 end;

end.

