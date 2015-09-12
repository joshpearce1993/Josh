{ Delphi Translation developed by ESB Consultancy
  http://www.esbconsult.com/ mailto:support@esbconsult.com
  
  Also a part of ESBPCS for VCL: http://www.esbconsult.com/esbpcs

  A unit for random number generation from the following
  distributions:

  Distribution Function/subroutine name

  Normal (Gaussian) Random_Normal LogNormal Random_LogNormal
  Gamma Random_Gamma Chi-squared Random_ChiSq Exponential
  Random_Exponential Weibull Random_Weibull Beta Random_Beta t
  Random_T Multivariate normal Random_MVNorm Generalized
  inverse Gaussian Random_Inv_Gauss Poisson Random_Poisson
  Binomial Random_Binomial1 * Random_Binomial2 * Negative
  binomial Random_neg_Binomial von Mises Random_von_Mises
  Cauchy Random_Cauchy
  
  Generate a random ordering of the integers 1 .. N
  Random_Order Initialize (seed) the uniform random number
  generator for ANY compiler Seed_Rrandom_Number
  
    * Two functions are provided for the binomial distribution.
  If the parameter values remain constant, it is recommended
  that the first function is used (random_binomial1). If one or
  both of the parameters change, use the second function
  (random_binomial2).
  
  Delphi's own random number generator, Random, is used to
  provide a source of uniformly distributed random numbers.
  
  N.B. At this stage, only one random number is generated at
  each call to one of the functions above.
  
  The unit uses the following functions which are included
  here: bin_prob to calculate a single binomial probability
  lngamma to calculate the logarithm to base e of the gamma
  function
  
  Some of the code is adapted from Dagpunar's book: Dagpunar,
  J. 'Principles of random variate generation' Clarendon Press,
  Oxford, 1988. ISBN 0&#45;19&#45;852202&#45;9
  
  In most of Dagpunar's routines, there is a test to see
  whether the value of one or two floating&#45;point parameters
  has changed since the last call. These tests have been
  replaced by using a logical variable FIRST. This should be
  set to .TRUE. on the first call using new values of the
  \parameters, and .FALSE. if the parameter values are the same
  as for the previous call.

  First Delphi Version by Glenn Crouch of ESB Consultancy
  support@esbconsult.com http://www.esbconsult.com
  Author
  Alan Miller CSIRO Division of Mathematical &amp; Information
  Sciences Private Bag 10, Clayton South MDC Clayton 3169,
  Victoria, Australia Phone: (+61) 3 9545-8016 Fax: (+61) 3
  9545-8080

  e-mail: amiller @ bigpond.net.au
                                                                                                                             }
unit AMRandom;

interface

{$Define UseMRNG}// Remove this if you don't have EFD's free MRNG.PAS

const
     Version = 4.3;

type
	// Used as a Dynamic Vector
	TAMFloatVector = array of Extended;

type
	// Used for passing Random Number Generators
	TRandomGenFunction = function: Extended;

	{ Used to call Delphi's Inbuilt Random Number Generator. Remember to use
	 Randomize if you don't want repeatable sequences. }
function DelphiRandom: Extended;

{$IFDEF UseMRNG}
{ Used to call EFD System's MRNG Random Number Generator. }
function MRNGRandom: Extended;
{$ENDIF}

{ Adapted from the following Fortran 77 code
  ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
  THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
  VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

  The function random_normal() returns a normally distributed pseudo-random
  number with zero mean and unit variance.

  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  and J.F. Monahan augmented with quadratic bounding curves. }
function Random_Normal: Extended; overload;
function Random_Normal (RandomGenerator: TRandomGenFunction): Extended; overload;
function Random_Normal (const Mean, StdDev: Extended): Extended; overload;
function Random_Normal (const Mean, StdDev: Extended; RandomGenerator: TRandomGenFunction): Extended; overload;

{ Adapted from approach suggested by Greg Hood, used with Permission.
}
function Random_LogNormal (const Mean, StdDev: Extended): Extended; overload;
{ Adapted from approach suggested by Greg Hood, used with Permission.
}
function Random_LogNormal (const Mean, StdDev: Extended; RandomGenerator: TRandomGenFunction): Extended; overload;

{ Adapted from Fortran 77 code from the book:
 Dagpunar, J. 'Principles of random variate generation'
 Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

 FUNCTION GENERATES A RANDOM GAMMA VARIATE.
 CALLS EITHER random_gamma1 (Shape > 1.0)
 OR random_exponential (Shape = 1.0)
 OR random_gamma2 (Shape < 1.0).

 Shape = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
}
function Random_Gamma (const Shape: Extended): Extended; overload;
function Random_Gamma (const Shape: Extended;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{  	Generates a random variate from the chi-squared distribution with
 dff degrees of freedom.
}
function Random_ChiSq (const DF: Integer): Extended; overload;
function Random_ChiSq (const DF: Integer;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
  TO EXP(-random_exponential), USING INVERSION.
}
function Random_Exponential: Extended; overload;
function Random_Exponential (RandomGenerator: TRandomGenFunction): Extended; overload;

{     Generates a random variate from the Weibull distribution with
 probability density:
      a
   a-1  -x
 f(x) = a.x    e
}
function Random_Weibull (const a: Extended): Extended; overload;
function Random_Weibull (const a: Extended;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
  FROM A BETA DISTRIBUTION WITH DENSITY
  PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
  USING CHENG'S LOG LOGISTIC METHOD.

  AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
}

function Random_Beta (const aa, bb: Extended): Extended; overload;
function Random_Beta (const aa, bb: Extended;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{  Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE FROM A
  T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

  DF = DEGREES OF FREEDOM OF DISTRIBUTION
    (DF >= 1)
}
function Random_T (const DF: Integer): Extended; overload;
function Random_T (const DF: Integer;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{  Adapted from Fortran 77 code from the book:
 Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  N.B. An extra argument, ier, has been added to Dagpunar's routine

  SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
  VECTOR USING A CHOLESKY DECOMPOSITION.

  ARGUMENTS:
     N = NUMBER OF VARIATES IN VECTOR
    (INPUT,INTEGER >= 1)
  H(J) = J'TH ELEMENT OF VECTOR OF MEANS
    (INPUT,REAL)
  X(J) = J'TH ELEMENT OF DELIVERED VECTOR
    (OUTPUT,REAL)

 D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
     (INPUT,REAL)
 F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
   DECOMPOSITION OF VARIANCE MATRIX (J <= I)
     (OUTPUT,REAL)

    ier = 1 if the input covariance matrix is not +ve definite
    = 0 otherwise
}
procedure Random_MVNorm (const h, d: TAMFloatVector;
     var f, x: TAMFloatVector; var ier: Integer); overload;
procedure Random_MVNorm (const h, d: TAMFloatVector;
     var f, x: TAMFloatVector; var ier: Integer;
     RandomGenerator: TRandomGenFunction); overload;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY] FROM
  A REPARAMETERISED GENERALISED INVERSE GAUSSIAN (GIG) DISTRIBUTION
  WITH DENSITY PROPORTIONAL TO  GIG**(H-1) * EXP(-0.5*B*(GIG+1/GIG))
  USING A RATIO METHOD.

  h = PARAMETER OF DISTRIBUTION (0 <= REAL)
  b = PARAMETER OF DISTRIBUTION (0 < REAL)
}

function Random_Inv_Gauss (const h, b: Extended): Extended; overload;
function Random_Inv_Gauss (const h, b: Extended;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{  FUNCTION GENERATES A RANDOM BINOMIAL VARIATE USING C.D.Kemp's method.
  This algorithm is suitable when many random variates are required
  with the SAME parameter values for n & p.

 P = BERNOULLI SUCCESS PROBABILITY
    (0 <= REAL <= 1)
 N = NUMBER OF BERNOULLI TRIALS
    (1 <= INTEGER)

  Reference: Kemp, C.D. (1986). `A modal method for generating binomial
     variables', Commun. Statist. - Theor. Meth. 15(3), 805-813.
}
function Random_Binomial1 (const n: Integer; const p: Extended): Int64; overload;
function Random_Binomial1 (const n: Integer; const p: Extended;
	RandomGenerator: TRandomGenFunction): Int64; overload;

{**********************************************************************
  Translated to Fortran 90 by Alan Miller from:
       RANLIB

  Library of Fortran Routines for Random Number Generation

       Compiled and Written by:

        Barry W. Brown
         James Lovato

    Department of Biomathematics, Box 237
    The University of Texas, M.D. Anderson Cancer Center
    1515 Holcombe Boulevard
    Houston, TX      77030

  This work was supported by grant CA-16672 from the National Cancer Institute.

     GENerate BINomial random deviate

       Function

  Generates a single random deviate from a binomial
  distribution whose number of trials is N and whose
  probability of an event in each trial is P.

       Arguments

  N  --> The number of trials in the binomial distribution
     from which a random deviate is to be generated.
       INTEGER N

  P  --> The probability of an event in each trial of the
     binomial distribution from which a random deviate
     is to be generated.
       REAL P

}
function Random_Binomial2 (const n: Int64; const pp: Extended): Int64; overload;
function Random_Binomial2 (const n: Int64; const pp: Extended;
     RandomGenerator: TRandomGenFunction): Int64; overload;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM NEGATIVE BINOMIAL VARIATE USING UNSTORED
  INVERSION AND/OR THE REPRODUCTIVE PROPERTY.

 SK = NUMBER OF FAILURES REQUIRED (Dagpunar's words!)
    = the `power' parameter of the negative binomial
    (0 < REAL)
 P = BERNOULLI SUCCESS PROBABILITY
    (0 < REAL < 1)

  THE PARAMETER H IS SET SO THAT UNSTORED INVERSION ONLY IS USED WHEN P <= H,
  OTHERWISE A COMBINATION OF UNSTORED INVERSION AND
  THE REPRODUCTIVE PROPERTY IS USED.
}
function Random_Neg_Binomial (const sk, p: Extended): Int64; overload;
function Random_Neg_Binomial (const sk, p: Extended; RandomGenerator: TRandomGenFunction): Int64; overload;

{     Algorithm VMD from:
  Dagpunar, J.S. (1990) `Sampling from the von Mises distribution via a
  comparison of random numbers', J. of Appl. Statist., 17, 165-168.

  Fortran 90 code by Alan Miller
  CSIRO Division of Mathematical & Information Sciences

  Arguments:
  k (real)        parameter of the von Mises distribution.
}

function Random_von_Mises (const k: Extended): Extended; overload;
function Random_von_Mises (const k: Extended;
     RandomGenerator: TRandomGenFunction): Extended; overload;

{      Generate a random deviate from the standard Cauchy distribution
}
function Random_Cauchy: Extended; overload;
function Random_Cauchy (RandomGenerator: TRandomGenFunction): Extended; overload;

{**********************************************************************
  Translated to Fortran 90 by Alan Miller from:
        RANLIB

  Library of Fortran Routines for Random Number Generation

     Compiled and Written by:

      Barry W. Brown
	  James Lovato

      Department of Biomathematics, Box 237
      The University of Texas, M.D. Anderson Cancer Center
      1515 Holcombe Boulevard
      Houston, TX      77030

  This work was supported by grant CA-16672 from the National Cancer Institute.

     GENerate POIsson random deviate

         Function

  Generates a single random deviate from a Poisson distribution with mean mu.

         Arguments

  mu --> The mean of the Poisson distribution from which
     a random deviate is to be generated.
       REAL mu

       Method

  For details see:

    Ahrens, J.H. and Dieter, U.
    Computer Generation of Poisson Deviates
    From Modified Normal Distributions.
    ACM Trans. Math. Software, 8, 2
    (June 1982),163-179

  TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
  COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
}
function Random_Poisson (var mu: Extended): Int64; overload;
{ \ \  }
function Random_Poisson (var mu: Extended;
     RandomGenerator: TRandomGenFunction): Int64; overload;

{ Logarithm to base e of the gamma function.

  Accurate to about 1.e-14.
  Programmer: Alan Miller

  Latest revision of Fortran 77 version - 28 February 1988
}
function lngamma (const x: Extended): Extended;

implementation

uses
     {$IFDEF UseMRNG}
     MRNG,
     {$ENDIF}
     Math, SysUtils,
     AMRandomRS;

const
     VSmall = MinDouble;
     VLarge = MaxDouble;

function DelphiRandom: Extended;
begin
     Result := Random;
end;

{$IFDEF UseMRNG}

function MRNGRandom: Extended;
begin
     Result := MRandom;
end;
{$ENDIF}

{ Adapted from the following Fortran 77 code
  ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
  THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
  VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

  The function random_normal() returns a normally distributed pseudo-random
  number with zero mean and unit variance.

  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  and J.F. Monahan augmented with quadratic bounding curves. }

function Random_Normal (RandomGenerator: TRandomGenFunction): Extended;
const
     s = 0.449871;
     t = -0.386595;
     a = 0.19600;
     b = 0.25472;
     r1 = 0.27597;
     r2 = 0.27846;
var
     u, v, x, y, q: Extended;
	Done: Boolean;

begin
     //     Generate P = (u,v) uniform in rectangle enclosing acceptance region

     Done := False;
     repeat
          u := RandomGenerator;
          v := RandomGenerator;
          v := 1.7156 * (v - 0.5);

          // Evaluate the quadratic form
          x := u - s;
          Y := abs (v) - t;
          q := Sqr (x) + y * (a * y - b * x);

          // Accept P if inside inner ellipse
          if (q < r1) then
               Done := True
          else if (q <= r2) and (Sqr (v) < -4.0 * Ln (u) * Sqr (u)) then
               Done := True;
     until Done;
     // Return ratio of P's coordinates as the normal deviate
     if u < VSmall then
          raise EMathError.Create (rsDivideByZero);

     Result := v / u;
end;

function Random_Normal: Extended;
begin
	{$IFDEF UseMRNG}
	Result := Random_Normal (MRNGRandom);
	{$ELSE}
	Result := Random_Normal (DelphiRandom);
	{$ENDIF}
end;

function Random_Normal (const Mean, StdDev: Extended;
	RandomGenerator: TRandomGenFunction): Extended;
begin
	Result := Random_Normal (RandomGenerator) * StdDev + Mean;
end;

function Random_Normal (const Mean, StdDev: Extended): Extended;
begin
	{$IFDEF UseMRNG}
	Result := Random_Normal (Mean, StdDev, MRNGRandom);
	{$ELSE}
	Result := Random_Normal (Mean, StdDev, DelphiRandom);
	{$ENDIF}
end;

function Random_LogNormal (const Mean, StdDev: Extended;
	RandomGenerator: TRandomGenFunction): Extended;
var
	C, M, S: Extended;
begin
	if Mean <= 0 then
		raise EMathError.Create (rsInvalidMean);
	if StdDev <= 0 then
		raise EMathError.Create (rsInvalidStdDev);

     C := StdDev / Mean;
     M := Ln (Mean) - 0.5 * Ln (Sqr (C) + 1);
     S := Sqrt (Ln (Sqr (C) + 1));
	Result := Exp (Random_Normal (M, S, RandomGenerator));
end;

function Random_LogNormal (const Mean, StdDev: Extended): Extended;
begin
	{$IFDEF UseMRNG}
	Result := Random_LogNormal (Mean, StdDev, MRNGRandom);
	{$ELSE}
	Result := Random_LogNormal (Mean, StdDev, DelphiRandom);
	{$ENDIF}
end;

{ Uses the algorithm in
  Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
 gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.

  Generates a random gamma deviate for shape parameter Shape >= 1.
}

function Random_Gamma1 (const Shape: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
var
     c, d: Extended;
     u, v, x: Extended;
     Done: Boolean;
begin
	if (Shape <= 1.0) then
		raise EMathError.Create (rsInvalidShape);

	d := Shape - 1 / 3;
	c := 1 / SQRT (9.0 * d);

	Result := 0.0;

	Done := False;
	repeat
		// Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.

		repeat
			x := Random_Normal (RandomGenerator);
			v := Power (1 + c * x, 3);
		until v > 0;

		// Generate uniform variable U

		u := RandomGenerator;

          if (u < 1.0 - 0.0331 * Sqr (Sqr (x))) then
          begin
               Result := d * v;
               Done := True;
          end
          else if Ln (u) < 0.5 * Sqr (x) + d * (1.0 - v + Ln (v)) then
          begin
               Result := d * v;
               Done := True;
          end
     until Done;
end;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
  GAMMA2**(S-1) * EXP(-GAMMA2),
  USING A SWITCHING METHOD.

 S = SHAPE PARAMETER OF DISTRIBUTION
   (REAL < 1.0)
 }

function Random_Gamma2 (const Shape: Extended;
	RandomGenerator: TRandomGenFunction): Extended;
var
     r, x, w: Extended;
     a, p, c, uf, vr, d: Extended;
     Done: Boolean;
begin
     if (Shape <= 0.0) and (Shape >= 1.0) then
          raise EMathError.Create (rsInvalidShape);
     if (Shape < VSmall) then
          raise EMathError.Create (rsShapeTooSmall);

     a := 1.0 - Shape;
     p := a / (a + Shape * EXP (-a));
     c := 1.0 / Shape;
     uf := p * Power (VSmall / a, Shape);
     vr := 1.0 - VSmall;
     d := a * Ln (a);
     w := 0;
     x := 0;

     Done := False;
     repeat
          r := RandomGenerator;
          if (r >= vr) then
               Continue;

          if (r > p) then
          begin
               x := a - Ln ((1.0 - r) / (1.0 - p));
               w := a * Ln (x) - d;
          end
          else if (r > uf) then
          begin
               x := a * Power (r / p, c);
               w := x;
          end
          else
          begin
               Result := 0.0;
               Exit;
          end;

          r := RandomGenerator;

          if (1.0 - r <= w) and (r > 0.0) then
          begin
               if (r * (w + 1.0) >= 1.0) then
				Continue;

               if (-Ln (r) <= w) then
                    Continue;

          end;
          Done := True;
     until Done;
     Result := x;
end;

{ Adapted from Fortran 77 code from the book:
 Dagpunar, J. 'Principles of random variate generation'
 Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

 FUNCTION GENERATES A RANDOM GAMMA VARIATE.
 CALLS EITHER random_gamma1 (Shape > 1.0)
 OR random_exponential (Shape = 1.0)
 OR random_gamma2 (Shape < 1.0).

 Shape = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
}

function Random_Gamma (const Shape: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
begin
     if (Shape <= 0.0) then
          raise EMathError.Create (rsInvalidShape);

     if (Shape > 1.0) then
          Result := Random_Gamma1 (Shape, RandomGenerator)
     else if (Shape < 1.0) then
          Result := Random_Gamma2 (Shape, RandomGenerator)
     else
          Result := Random_Exponential (RandomGenerator);
end;

function Random_Gamma (const Shape: Extended): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Gamma (Shape, MRNGRandom);
     {$ELSE}
     Result := Random_Gamma (Shape, DelphiRandom);
     {$ENDIF}
end;

{  	Generates a random variate from the chi-squared distribution with
 dff degrees of freedom.
}

function Random_ChiSq (const DF: Integer;
     RandomGenerator: TRandomGenFunction): Extended;
begin
     Result := 2.0 * Random_Gamma (0.5 * DF, RandomGenerator)
end;

function Random_ChiSq (const DF: Integer): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_ChiSq (DF, MRNGRandom);
     {$ELSE}
     Result := Random_ChiSq (DF, DelphiRandom);
     {$ENDIF}
end;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
  TO EXP(-random_exponential), USING INVERSION.
}

function Random_Exponential (RandomGenerator: TRandomGenFunction): Extended;
var
     r: Extended;
     Done: Boolean;
begin
     Done := False;
     repeat
          r := RandomGenerator;
          if (r > vSmall) then
               Done := True;
     until Done;
     Result := -Ln (r)
end;

function Random_Exponential: Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Exponential (MRNGRandom);
     {$ELSE}
     Result := Random_Exponential (DelphiRandom);
	{$ENDIF}
end;

{     Generates a random variate from the Weibull distribution with
 probability density:
      a
   a-1  -x
 f(x) = a.x    e
}

function Random_Weibull (const a: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
begin
     if Abs (a) <= VSmall then
          raise EMathError.Create (rsInvalidValue);

     Result := Power (Random_Exponential (RandomGenerator), 1.0 / a);
end;

function Random_Weibull (const a: Extended): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Weibull (a, MRNGRandom);
     {$ELSE}
     Result := Random_Weibull (a, DelphiRandom);
     {$ENDIF}
end;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
  FROM A BETA DISTRIBUTION WITH DENSITY
  PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
  USING CHENG'S LOG LOGISTIC METHOD.

  AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
}

function Random_Beta (const aa, bb: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
const
     aln4 = 1.3862943611198906;
var
     a, b, g, r, s, x, y, z: Extended;
	d, f, h, t, c: Extended;
     Swap: Boolean;
     Done: Boolean;
begin
     if (aa <= 0.0) or (bb <= 0.0) then
          raise EMathError.Create (rsInvalidValue);

     a := aa;
     b := bb;
     swap := b > a;
     if swap then
     begin
          g := b;
          b := a;
          a := g;
     end;
     d := a / b;
     f := a + b;
     if (b > 1.0) then
     begin
          h := Sqrt ((2.0 * a * b - f) / (f - 2.0));
          t := 1.0;
     end
     else
     begin
          h := b;
          t := 1.0 / (1.0 + Power (a / (VLarge * b), b));
     end;
     c := a + h;

     Done := False;
     Result := 0.0;
     repeat
          r := RandomGenerator;
          x := RandomGenerator;
          s := Sqr (r) * x;
          if (r < VSmall) or (s <= 0.0) then
               Continue;

          if (r < t) then
          begin
               x := Ln (r / (1.0 - r)) / h;
               y := d * Exp (x);
               z := c * x + f * Ln ((1.0 + d) / (1.0 + y)) - aln4;
               if (s - 1.0 > z) then
               begin
                    if (s - s * z > 1.0) then
					Continue;
                    if (Ln (s) > z) then
                         Continue;
               end;
               Result := y / (1.0 + y);
          end
          else
          begin
               if 4.0 * s > Power (1.0 + 1.0 / d, f) then
                    Continue;
               Result := 1.0
          end;
          Done := True
     until Done;

     if Swap then
          Result := 1 - Result;
end;

function Random_Beta (const aa, bb: Extended): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Beta (aa, bb, MRNGRandom);
     {$ELSE}
     Result := Random_Beta (aa, bb, DelphiRandom);
     {$ENDIF}
end;

{  Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE FROM A
  T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

  DF = DEGREES OF FREEDOM OF DISTRIBUTION
    (DF >= 1)
}

function Random_T (const DF: Integer;
     RandomGenerator: TRandomGenFunction): Extended;
var
     s, c, a, f, g: Extended;
     r, x, v: Extended;
     Done: Boolean;
begin
     if (DF < 1) then
		raise EMathError.Create (rsInvalidValue);

     s := DF;
     c := -0.25 * (s + 1.0);
     a := 4.0 / Power (1.0 + 1.0 / s, c);
     f := 16.0 / a;
     if (DF > 1) then
     begin
          g := s - 1.0;
          g := Power ((s + 1.0) / g, c) * Sqrt ((s + s) / g);
     end
     else
          g := 1.0;

     Done := False;
     x := 0;
     repeat
          r := RandomGenerator;
          if (r <= 0.0) then
               Continue;
          v := RandomGenerator;
          x := (2.0 * v - 1.0) * g / r;
          v := Sqr (x);
          if (v > 5.0 - a * r) then
          begin
               if (DF >= 1) and (r * (v + 3.0) > f) then
                    Continue;
               if r > Power (1.0 + v / s, c) then
                    Continue;
          end;
          Done := True;
     until Done;
     Result := x;
end;

function Random_T (const DF: Integer): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_T (DF, MRNGRandom);
     {$ELSE}
     Result := Random_T (DF, DelphiRandom);
     {$ENDIF}
end;

{  Adapted from Fortran 77 code from the book:
 Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  N.B. An extra argument, ier, has been added to Dagpunar's routine

  SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
  VECTOR USING A CHOLESKY DECOMPOSITION.

  ARGUMENTS:
     N = NUMBER OF VARIATES IN VECTOR
    (INPUT,INTEGER >= 1)
  H(J) = J'TH ELEMENT OF VECTOR OF MEANS
    (INPUT,REAL)
  X(J) = J'TH ELEMENT OF DELIVERED VECTOR
    (OUTPUT,REAL)

 D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
     (INPUT,REAL)
 F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
   DECOMPOSITION OF VARIANCE MATRIX (J <= I)
     (OUTPUT,REAL)

    ier = 1 if the input covariance matrix is not +ve definite
    = 0 otherwise
}

procedure Random_MVNorm (const h, d: TAMFloatVector;
     var f, x: TAMFloatVector; var ier: Integer;
     RandomGenerator: TRandomGenFunction);
var
     i, j, m, n, n2, fn: Integer;
     y, v: Extended;
begin
     n := Length (h);
     if (n = 0) then
          raise EMathError.Create (rsVectorEmpty);

     fn := n * (n + 1) div 2;
     if (Length (D) < fn) then
          raise EMathError.Create (rsVectorTooSmall);

     SetLength (f, fn);

     ier := 0;

     n2 := 2 * n;
     if (d [0] <= 0.0) then
     begin
          ier := 1;
          Exit;
     end;

     f [0] := Sqrt (d [0]);
     y := 1.0 / f [0];

     for j := 2 to n do
          f [j - 1] := d [j * (j - 1) div 2] * y;

     for i := 2 to n do
     begin
          v := d [i * (i - 1) div 2 + i - 1];
          for m := 1 to i - 1 do
               v := v - Sqr (f [(m - 1) * (n2 - m) div 2 + i - 1]);

          if (v <= 0.0) then
          begin
               ier := 1;
               Exit;
          end;

          v := Sqrt (v);
          y := 1.0 / v;
          F [(i - 1) * (n2 - i) div 2 + i - 1] := v;

          for j := i + 1 to n do
          begin
               v := d [j * (j - 1) div 2 + i - 1];
               for m := 1 to i - 1 do
               begin
                    v := v - f [(m - 1) * (n2 - m) div 2 + i - 1]
                         * f [(m - 1) * (n2 - m) div 2 + j - 1]
               end;
               f [(i - 1) * (n2 - i) div 2 + j - 1] := v * y;
          end;
     end;

     X := Copy (H, 0, n);
     for j := 1 to n do
     begin
          y := Random_Normal (RandomGenerator);
          for i := j to n do
               x [i - 1] := x [i - 1] + f [(j - 1) * (n2 - j) div 2 + i - 1] * y;
     end;
end;

procedure Random_MVNorm (const h, d: TAMFloatVector;
     var f, x: TAMFloatVector; var ier: Integer);
begin
     {$IFDEF UseMRNG}
     Random_MVNorm (h, d, f, x, ier, MRNGRandom);
     {$ELSE}
     Random_MVNorm (h, d, f, x, ier, DelphiRandom);
     {$ENDIF}
end;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY] FROM
  A REPARAMETERISED GENERALISED INVERSE GAUSSIAN (GIG) DISTRIBUTION
  WITH DENSITY PROPORTIONAL TO  GIG**(H-1) * EXP(-0.5*B*(GIG+1/GIG))
  USING A RATIO METHOD.

  h = PARAMETER OF DISTRIBUTION (0 <= REAL)
  b = PARAMETER OF DISTRIBUTION (0 < REAL)
}

function Random_Inv_Gauss (const h, b: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
var
     ym, xm, r, w, r1, r2, x: Extended;
     a, c, d, e: Extended;
     Done: Boolean;
begin
     if (h < 0.0) or (b <= 0.0) then
          raise EMathError.Create (rsInvalidValue);

     if (h > 0.25 * b * Sqrt (VLarge)) then
          raise EMathError.Create (rsInvalidValue);

     e := Sqr (b);
     d := h + 1.0;
     ym := (-d + Sqrt (Sqr (d) + e)) / b;
     if (ym < VSmall) then
          raise EMathError.Create (rsInvalidValue);

     d := h - 1.0;
     xm := (d + Sqrt (Sqr (d) + e)) / b;
     if (xm < VSmall) then
          raise EMathError.Create (rsInvalidValue);

     d := 0.5 * d;
     e := -0.25 * b;
     r := xm + 1.0 / xm;
     w := xm * ym;
     a := Power (w, -0.5 * h) * Sqrt (xm / ym) * Exp (-e * (r - ym - 1.0 / ym));
     if (a < vsmall) then
          raise EMathError.Create (rsInvalidValue);

     c := -d * Ln (xm) - e * r;

     Done := False;
     x := 0.0;
     repeat
          r1 := RandomGenerator;
          if (r1 > 0.0) then
          begin
               r2 := RandomGenerator;
               x := a * r2 / r1;
               if (x > 0.0) then
               begin
                    if (Ln (r1) < d * Ln (x) + e * (x + 1.0 / x) + c) then
                         Done := True
               end;
          end
     until Done;
     Result := x;
end;

function Random_Inv_Gauss (const h, b: Extended): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Inv_Gauss (h, b, MRNGRandom);
     {$ELSE}
     Result := Random_Inv_Gauss (h, b, DelphiRandom);
     {$ENDIF}
end;

{ Logarithm to base e of the gamma function.

  Accurate to about 1.e-14.
  Programmer: Alan Miller

  Latest revision of Fortran 77 version - 28 February 1988
}

function lngamma (const x: Extended): Extended;
const
     a1 = -4.166666666554424E-02;
     a2 = 2.430554511376954E-03;
     a3 = -7.685928044064347E-04;
     a4 = 5.660478426014386E-04;
     lnrt2pi = 9.189385332046727E-1;
     pi = 3.141592653589793E0;
var
     temp, arg, product: Extended;
     reflect: Boolean;
begin
     //  lngamma is not defined if x = 0 or a negative integer.
     if (x = 0.0) or ((x < 0.0) and (Abs (x - Int (x)) < VSmall)) then
          raise EMathError.Create (rsInvalidValue);

     // If x < 0, use the reflection formula:
     //        gamma(x) * gamma(1-x) = pi * cosec(pi.x)

     reflect := x < 0.0;
     if reflect then
          arg := 1.0 - x
     else
          arg := x;

     // Increase the argument, if necessary, to make it > 10.

     product := 1.0;
     while (arg <= 10.0) do
     begin
          product := product * arg;
          arg := arg + 1.0;
     end;

     // Use a polynomial approximation to Stirling's formula.
     // N.B. The real Stirling's formula is used here, not the simpler, but less
     //     accurate formula given by De Moivre in a letter to Stirling, which
     //     is the one usually quoted.

     arg := arg - 0.5;
     temp := 1.0 / Sqr (arg);
     Result := lnrt2pi + arg * (Ln (arg) - 1.0 +
          (((a4 * temp + a3) * temp + a2) * temp + a1) * temp) - Ln (product);

     if reflect then
     begin
          temp := Sin (pi * x);
          Result := Ln (pi / temp) - Result;
     end;
end;

{     Calculate a binomial probability
}

function bin_prob (const n: Int64; const p: Extended; const r: Int64): Extended;
begin
     Result := Exp (lngamma (n + 1.0) - lngamma (r + 1.0) - lngamma (n - r + 1.0)
          + r * Ln (p) + (n - r) * Ln (1.0 - p));
end;

{  FUNCTION GENERATES A RANDOM BINOMIAL VARIATE USING C.D.Kemp's method.
  This algorithm is suitable when many random variates are required
  with the SAME parameter values for n & p.

 P = BERNOULLI SUCCESS PROBABILITY
    (0 <= REAL <= 1)
 N = NUMBER OF BERNOULLI TRIALS
    (1 <= INTEGER)

  Reference: Kemp, C.D. (1986). `A modal method for generating binomial
     variables', Commun. Statist. - Theor. Meth. 15(3), 805-813.
}

function Random_Binomial1 (const n: Integer; const p: Extended;
     RandomGenerator: TRandomGenFunction): Int64;
var
     ru, rd: Int64;
     r0: Int64;
     u, pd, pu: Real;
     odds_ratio, p_r: Real;
begin
     r0 := Trunc ((n + 1) * p);
     p_r := bin_prob (n, p, r0);
     if p < 1 then
          odds_ratio := p / (1.0 - p)
     else
          odds_ratio := VLarge;

     u := RandomGenerator;
     u := u - p_r;
     if (u < 0.0) then
     begin
          Result := r0;
          Exit;
     end;

     pu := p_r;
     ru := r0;
     pd := p_r;
     rd := r0;

     repeat
          Dec (rd);
          if (rd >= 0) then
          begin
               pd := pd * (rd + 1.0) / (odds_ratio * (n - rd));
               u := u - pd;
               if (u < 0.0) then
               begin
                    Result := rd;
                    Exit;
               end;
          end;

          Inc (ru);
          if (ru <= n) then
          begin
               pu := pu * (n - ru + 1.0) * odds_ratio / ru;
               u := u - pu;
               if (u < 0.0) then
               begin
                    Result := ru;
                    Exit;
               end;
          end;
     until False;
end;

function Random_Binomial1 (const n: Integer; const p: Extended): Int64;
begin
     {$IFDEF UseMRNG}
     Result := Random_Binomial1 (n, p, MRNGRandom);
     {$ELSE}
     Result := Random_Binomial1 (n, p, DelphiRandom);
     {$ENDIF}
end;
{**********************************************************************
  Translated to Fortran 90 by Alan Miller from:
       RANLIB

  Library of Fortran Routines for Random Number Generation

       Compiled and Written by:

        Barry W. Brown
         James Lovato

    Department of Biomathematics, Box 237
    The University of Texas, M.D. Anderson Cancer Center
    1515 Holcombe Boulevard
    Houston, TX      77030

  This work was supported by grant CA-16672 from the National Cancer Institute.

     GENerate BINomial random deviate

       Function

  Generates a single random deviate from a binomial
  distribution whose number of trials is N and whose
  probability of an event in each trial is P.

       Arguments

  N  --> The number of trials in the binomial distribution
     from which a random deviate is to be generated.
       INTEGER N

  P  --> The probability of an event in each trial of the
     binomial distribution from which a random deviate
     is to be generated.
       REAL P

!     FIRST --> Set FIRST = .TRUE. for the first call to perform initialization
!               the set FIRST = .FALSE. for further calls using the same pair
!               of parameter values (N, P).
!                              LOGICAL FIRST

!     random_binomial2 <-- A random deviate yielding the number of events
!                from N independent trials, each of which has
!                a probability of event P.
!                              INTEGER random_binomial

!                              Method

!     This is algorithm BTPE from:

!         Kachitvichyanukul, V. and Schmeiser, B. W.
!         Binomial Random Variate Generation.
!         Communications of the ACM, 31, 2 (February, 1988) 216.

!**********************************************************************
}

function Random_Binomial2 (const n: Int64; const pp: Extended;
     RandomGenerator: TRandomGenFunction): Int64;
var
     alv, amaxp, f, f1, f2, u, v, w, w2, x, x1, x2, ynorm, z, z2: Extended;
     i: Integer;
     ix, ix1, k, mp: Int64;
     m: Int64;
     p, q, xnp, ffm, fm, xnpq, p1, xm, xl, xr, c, al, xll,
          xlr, p2, p3, p4, qn, r, g: Extended;
     Done: Boolean;
begin
     // SETUP
     p := Min (pp, 1.0 - pp);
     q := 1.0 - p;
     xnp := n * p;

     if (xnp > 30.0) then
     begin
          ffm := xnp + p;
          m := Trunc (ffm);
          fm := m;
          xnpq := xnp * q;
          p1 := INT (2.195 * Sqrt (xnpq) - 4.6 * q) + 0.5;
          xm := fm + 0.5;
          xl := xm - p1;
          xr := xm + p1;
          c := 0.134 + 20.5 / (15.3 + fm);
          al := (ffm - xl) / (ffm - xl * p);
          xll := al * (1.0 + 0.5 * al);
          al := (xr - ffm) / (xr * q);
          xlr := al * (1.0 + 0.5 * al);
          p2 := p1 * (1.0 + c + c);
          p3 := p2 + c / xll;
          p4 := p3 + c / xlr;

          // GENERATE VARIATE, Binomial mean at least 30.

          ix := 0;
          repeat;
               u := RandomGenerator; //20
               u := u * p4;
               v := RandomGenerator;

               // TRIANGULAR REGION
               if (u <= p1) then
               begin
                    ix := Trunc (xm - p1 * v + u);
                    Break;
               end;

               // PARALLELOGRAM REGION
               if (u <= p2) then
               begin
                    x := xl + (u - p1) / c;
                    v := v * c + 1.0 - Abs (xm - x) / p1;
                    if (v > 1.0) or (v <= 0) then
                         Continue;
                    ix := Trunc (x);
               end
               else
               begin
                    // LEFT TAIL
                    if (u <= p3) then
                    begin
                         ix := Trunc (xl + Ln (v) / xll);
                         if (ix < 0) then
                              Continue;
                         v := v * (u - p2) * xll
                    end
                         // RIGHT TAIL
                    else
                    begin
                         ix := Trunc (xr - Ln (v) / xlr);
                         if (ix > n) then
                              Continue;
                         v := v * (u - p3) * xlr;
                    end;
               end;

               // DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST

               k := Abs (ix - m);
               if (k <= 20) or (k >= xnpq / 2 - 1) then
               begin
                    // EXPLICIT EVALUATION
                    f := 1.0;
                    r := p / q;
                    g := (n + 1) * r;
                    if (m < ix) then
                    begin
                         mp := m + 1;
                         for i := mp to ix do
                              f := f * (g / i - r);
                    end
                    else if (m > ix) then
                    begin
                         ix1 := ix + 1;
                         for i := ix1 to m do
                              f := f / (g / i - r);
                    end;

                    if (v > f) then
                         Continue
                    else
                         Break
               end;

               // SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X))

               amaxp := (k / xnpq) * ((k * (k / 3.0 + 0.625)
                    + 0.1666666666666) / xnpq + 0.5);
               ynorm := -Sqr (k) / (2.0 * xnpq);
               alv := Ln (v);
               if (alv < ynorm - amaxp) then
                    Break;
               if (alv > ynorm + amaxp) then
                    Continue;
               // STIRLING'S (actually de Moivre's) FORMULA TO MACHINE ACCURACY FOR
               //  THE FINAL ACCEPTANCE/REJECTION TEST

               x1 := ix + 1;
               f1 := fm + 1.0;
               z := n + 1 - fm;
               w := n - ix + 1.0;
               z2 := Sqr (z);
               x2 := Sqr (x1);
               f2 := Sqr (f1);
               w2 := Sqr (w);
               if (alv - (xm * Ln (f1 / x1) + (n - m + 0.5) * Ln (z / w)
                    + (ix - m) * Ln (w * p / (x1 * q))
                    + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / f2) / f2) / f2) / f2) / f1 / 166320.0
                    + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / z2) / z2) / z2) / z2) / z / 166320.0
                    + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / x2) / x2) / x2) / x2) / x1 / 166320.0
                    + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / w2) / w2) / w2) / w2) / w / 166320.0)
                    > 0.0) then
               begin
                    Continue;
               end
               else
                    Break;
          until False;
     end
     else
     begin
          // INVERSE CDF LOGIC FOR MEAN LESS THAN 30
          qn := Power (q, n);
          r := p / q;
          g := r * (n + 1);

          Done := False;
          repeat
               ix := 0;
               f := qn;
               u := RandomGenerator;
               while (u >= f) do
               begin
                    if (ix > 110) then
                         Done := False
                    else
                    begin
                         Done := True;
                         u := u - f;
                         ix := ix + 1;
                         f := f * (g / ix - r);
                    end;
               end;
          until Done;
     end;

     if (pp > 0.5) then
          ix := n - ix;

     Result := ix;
end;

function Random_Binomial2 (const n: Int64; const pp: Extended): Int64;
begin
     {$IFDEF UseMRNG}
     Result := Random_Binomial2 (n, pp, MRNGRandom);
     {$ELSE}
     Result := Random_Binomial2 (n, pp, DelphiRandom);
     {$ENDIF}
end;

{ Adapted from Fortran 77 code from the book:
  Dagpunar, J. 'Principles of random variate generation'
  Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  FUNCTION GENERATES A RANDOM NEGATIVE BINOMIAL VARIATE USING UNSTORED
  INVERSION AND/OR THE REPRODUCTIVE PROPERTY.

 SK = NUMBER OF FAILURES REQUIRED (Dagpunar's words!)
    = the `power' parameter of the negative binomial
    (0 < REAL)
 P = BERNOULLI SUCCESS PROBABILITY
    (0 < REAL < 1)

  THE PARAMETER H IS SET SO THAT UNSTORED INVERSION ONLY IS USED WHEN P <= H,
  OTHERWISE A COMBINATION OF UNSTORED INVERSION AND
  THE REPRODUCTIVE PROPERTY IS USED.
}

function Random_Neg_Binomial (const sk, p: Extended;
     RandomGenerator: TRandomGenFunction): Int64;
const
     h = 0.7;
var
     q, x, st, uln, v, r, s, y, g: Extended;
     k, n: Int64;
     i: Integer;
begin
     if (sk <= 0.0) or (p <= 0.0) or (p >= 1.0) then
          raise EMathError.Create (rsInvalidValue);

     q := 1.0 - p;
     x := 0.0;
     st := sk;
     if (p > h) then
     begin
          v := 1.0 / Ln (p);
          k := Trunc (st);
          for i := 1 to k do
          begin
               repeat
                    r := RandomGenerator;
               until r > 0.0;
               n := Trunc (v * Ln (r));
               x := x + n;
          end;
          st := st - k;
     end;

     s := 0.0;
     uln := -Ln (VSmall);
     if (st > -uln / Ln (q)) then
          raise EMathError.Create (rsInvalidValue);

     y := Power (q, st);
     g := st;
     r := RandomGenerator;
     while (y <= r) do
     begin
          r := r - y;
          s := s + 1.0;
          y := y * p * g / s;
          g := g + 1.0;
     end;

     Result := Trunc (x + s + 0.5)
end;

function Random_Neg_Binomial (const sk, p: Extended): Int64;
begin
     {$IFDEF UseMRNG}
     Result := Random_Neg_Binomial (sk, p, MRNGRandom);
     {$ELSE}
     Result := Random_Neg_Binomial (sk, p, DelphiRandom);
     {$ENDIF}
end;

//     Gaussian integration of exp(k.cosx) from a to b.

procedure integral (const a, b: Extended; var res: Extended;
     const dk: Extended);
var
     xmid, range, x1, x2: Extended;
     X, W: array [1..3] of Extended;
     I: Integer;
begin
     X [1] := 0.238619186083197;
     X [2] := 0.661209386466265;
     X [3] := 0.932469514203152;
     W [1] := 0.467913934572691;
     W [2] := 0.360761573048139;
     W [3] := 0.171324492379170;

     xmid := (a + b) / 2.0;
     range := (b - a) / 2.0;

     res := 0.0;
     for I := 1 to 3 do
     begin
          x1 := xmid + X [I] * range;
          x2 := xmid - X [I] * range;
          res := res + W [I] * (Exp (dk * Cos (x1)) + Exp (dk * Cos (x2)))
     end;

     res := res * range;
end;

{     Algorithm VMD from:
  Dagpunar, J.S. (1990) `Sampling from the von Mises distribution via a
  comparison of random numbers', J. of Appl. Statist., 17, 165-168.

  Fortran 90 code by Alan Miller
  CSIRO Division of Mathematical & Information Sciences

  Arguments:
  k (real)        parameter of the von Mises distribution.
}

function Random_von_Mises (const k: Extended;
     RandomGenerator: TRandomGenFunction): Extended;
var
     j, n, jj: Integer;
     nk: Integer;
     p: array [1..20] of Extended;
     theta: array [0..20] of Extended;
     xx, sump, r, th, lambda, rlast: Extended;
     dk: Extended;
begin
     if (k < 0.0) then
          raise EMathError.Create (rsInvalidValue);

     nk := Trunc (k + k + 1.0);
     if (nk > 20) then
          raise EMathError.Create (rsInvalidValue);

     dk := k;
     theta [0] := 0.0;
     if (k > 0.5) then
     begin
          // Set up array p of probabilities.
          sump := 0.0;
          for j := 1 to nk do
          begin
               if (j < nk) then
                    theta [j] := ArcCos (1.0 - j / k)
               else
                    theta [nk] := Pi;

               // Numerical integration of e^[k.cos(x)] from theta(j-1) to theta(j)
               Integral (theta [j - 1], theta [j], p [j], dk);
               sump := sump + p [j];
          end;
          for j := 1 to nk do
               p [j] := p [j] / sump;
     end
     else
     begin
          p [1] := 1.0;
          theta [1] := Pi;
     end;

     r := RandomGenerator;
     jj := nk;
     for j := 1 to nk do
     begin
          r := r - p [j];
          if (r < 0.0) then
          begin
               jj := j;
               Break;
          end;
     end;
     r := -r / p [jj];

     repeat
          th := theta [jj - 1] + r * (theta [jj] - theta [j - 1]);
          lambda := k - jj + 1.0 - k * Cos (th);
          n := 1;
          rlast := lambda;

          repeat
               r := RandomGenerator;
               if r <= rlast then
               begin
                    Inc (n);
                    rlast := r;
               end;
          until (r > rlast);

          if not Odd (n) then
               r := RandomGenerator
     until Odd (n);

     th := Abs (th);
     xx := (r - rlast) / (1.0 - rlast) - 0.5;
     if xx < 0 then
          Result := -1 * th
     else
          Result := th;
end;

function Random_von_Mises (const k: Extended): Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_von_Mises (k, MRNGRandom);
     {$ELSE}
     Result := Random_von_Mises (k, DelphiRandom);
     {$ENDIF}
end;

{      Generate a random deviate from the standard Cauchy distribution
}

function Random_Cauchy (RandomGenerator: TRandomGenFunction): Extended;
var
     V1, V2: Extended;
begin
     repeat
          V1 := 2.0 * (RandomGenerator - 0.5);
          V2 := 2.0 * (RandomGenerator - 0.5);
     until (Abs (V2) > VSmall) and (Sqr (V1) + Sqr (V2) < 1.0);
     Result := V1 / V2;
end;

function Random_Cauchy: Extended;
begin
     {$IFDEF UseMRNG}
     Result := Random_Cauchy (MRNGRandom);
     {$ELSE}
     Result := Random_Cauchy (DelphiRandom);
     {$ENDIF}
end;

{**********************************************************************
  Translated to Fortran 90 by Alan Miller from:
        RANLIB

  Library of Fortran Routines for Random Number Generation

     Compiled and Written by:

      Barry W. Brown
       James Lovato

      Department of Biomathematics, Box 237
      The University of Texas, M.D. Anderson Cancer Center
      1515 Holcombe Boulevard
      Houston, TX      77030

  This work was supported by grant CA-16672 from the National Cancer Institute.

     GENerate POIsson random deviate

         Function

  Generates a single random deviate from a Poisson distribution with mean mu.

         Arguments

  mu --> The mean of the Poisson distribution from which
     a random deviate is to be generated.
       REAL mu

       Method

  For details see:

    Ahrens, J.H. and Dieter, U.
    Computer Generation of Poisson Deviates
    From Modified Normal Distributions.
    ACM Trans. Math. Software, 8, 2
    (June 1982),163-179

  TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
  COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
}

function Random_Poisson (var mu: Extended;
     RandomGenerator: TRandomGenFunction): Int64;
const
     a0 = -0.5;
     a1 = 0.3333333;
     a2 = -0.2500068;
     a3 = 0.2000118;
     a4 = -0.1661269;
     a5 = 0.1421878;
     a6 = -0.1384794;
     a7 = 0.1250060;

var
     b1, b2, c, c0, c1, c2, c3, del, difmuk, e, fk, fx, fy, g,
          omega, px, py, t, u, v, x, xx: Extended;
     s, d, p, q, p0: Extended;
     j, k, kflag: Integer;
     l, m: Integer;
     pp: array [1..35] of Extended;
     fact: array [1..10] of Extended;
     FirstK0: Boolean;
begin
     u := 0;
     e := 0;
     fk := 0;
     difmuk := 0;

     fact [1] := 1; // Factorial 0
     fact [2] := 1;
     fact [3] := 2;
     fact [4] := 6;
     fact [5] := 24;
     fact [6] := 120;
     fact [7] := 720;
     fact [8] := 5040;
     fact [9] := 40320;
     fact [10] := 362880; // Factorial 9

     Result := 0;

     if (mu > 10.0) then
     begin
          // C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)

          FirstK0 := False;

          s := Sqrt (mu);
          d := 6.0 * Sqr (mu);

          // THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
          // PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
          //   IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .

          l := Trunc (mu - 1.1484);

          // STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE

          g := mu + s * Random_Normal (RandomGenerator);
          if (g > 0.0) then
          begin
               Result := Trunc (g);

               // STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH

               if (Result >= l) then
                    Exit;

               // STEP S. SQUEEZE ACCEPTANCE - SAMPLE U

               fk := Result;
               difmuk := mu - fk;
               u := RandomGenerator;
               if (d * u >= Sqr (difmuk) * difmuk) then
                    Exit;
          end;

          // STEP P. PREPARATIONS FOR STEPS Q AND H.
          // (RECALCULATIONS OF PARAMETERS IF NECESSARY)
          // .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
          // THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
          // APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
          // C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.

          omega := 0.3989423 / s;
          b1 := 0.4166667E-1 / mu;
          b2 := 0.3 * Sqr (b1);
          c3 := 0.1428571 * b1 * b2;
          c2 := b2 - 15.0 * c3;
          c1 := b1 - 6.0 * b2 + 45.0 * c3;
          c0 := 1.0 - b1 + 3.0 * b2 - 15.0 * c3;
          c := 0.1069 / mu;

          kflag := -1;

          if (g >= 0.0) then
          begin

               // 'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)
               kflag := 0;
               FirstK0 := True;
          end;

          // STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
          // DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
          // (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)

          repeat // 50
               if not FirstK0 then
               begin
                    repeat // 50
                         e := Random_Exponential (RandomGenerator);
                         u := RandomGenerator;
                         u := u + u - 1.0;
                         if u < 0 then
                              t := 1.8 - abs (e)
                         else
                              t := 1.8 + abs (e);
                    until t > -0.6744;

                    Result := Trunc (mu + s * t);
                    fk := Result;
                    difmuk := mu - fk;

                    // 'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)

                    kflag := 1
               end
               else
                    FirstK0 := False;

               // STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
               // CASE ival < 10 USES FACTORIALS FROM TABLE FACT

               if (Result < 10) then // 70
               begin
                    px := -mu;
                    py := Power (mu, Result) / fact [Result + 1];
               end
               else
               begin
                    // CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
                    //  A0-A7 FOR ACCURACY WHEN ADVISABLE
                    //  .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)

                    del := 0.8333333E-1 / fk; // 80
                    del := del - 4.8 * Sqr (del) * del;
                    v := difmuk / fk;
                    if Abs (v) > 0.25 then
                         px := fk * Ln (1.0 + v) - difmuk - del
                    else
                         px := fk * Sqr (v) * (((((((a7 * v + a6) * v + a5) * v + a4)
                              * v + a3) * v + a2) * v + a1) * v + a0) - del;
                    py := 0.3989423 / Sqrt (fk);
               end;
               x := (0.5 - difmuk) / s; // 110
               xx := Sqr (x);
               fx := -0.5 * xx;
               fy := omega * (((c3 * xx + c2) * xx + c1) * xx + c0);
               if (kflag <= 0) then
               begin
                    // STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)

                    if (fy - u * fy <= py * Exp (px - fx)) then // 40
                         Exit;
               end
               else
               begin
                    // STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)

                    if (c * Abs (u) <= py * Exp (px + e) - fy * Exp (fx + e)) then // 60
                         Exit;
               end;
          until False;
     end
     else
     begin

          // C A S E  B.    mu < 10
          // START NEW TABLE AND CALCULATE P0 IF NECESSARY

          m := MAX (1, Trunc (mu));
          l := 0;
          p := Exp (-mu);
          q := p;
          p0 := p;

          //  STEP U. UNIFORM SAMPLE FOR INVERSION METHOD

          repeat
               u := RandomGenerator;
               Result := 0;
               if (u <= p0) then
                    Exit;

               // STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
               // PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
               // (0.458=PP(9) FOR MU=10)

               if l <> 0 then
               begin
                    j := 1;
                    if (u > 0.458) then
                         j := MIN (l, m);
                    for k := j to l do
                         if (u <= pp [k]) then
                         begin
                              Result := K;
                              Exit;
                         end;

                    if l = 35 then
                         Continue;
               end;

               // STEP C. CREATION OF NEW POISSON PROBABILITIES P
               // AND THEIR CUMULATIVES Q=PP(K)

               l := l + 1; // 150
               for k := l to 35 do
               begin
                    p := p * mu / k;
                    q := q + p;
                    pp [k] := q;
                    if (u <= q) then
                    begin
                         Result := K;
                         Exit;
                    end;
               end;
               l := 35;
          until False
     end;
end;

function Random_Poisson (var mu: Extended): Int64;
begin
     {$IFDEF UseMRNG}
     Result := Random_Poisson (mu, MRNGRandom);
     {$ELSE}
     Result := Random_Poisson (mu, DelphiRandom);
     {$ENDIF}
end;

end.

