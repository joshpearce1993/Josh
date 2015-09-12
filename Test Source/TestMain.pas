unit TestMain;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 18.5 }
{$DEFINE UseXPMan}
{$IFEND}
{$ENDIF}

uses
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF UseXPMan}
     XPMan,
{$ENDIF}
     StdCtrls, ESBPCSLabel, ExtCtrls, ESBPCSPanel, ESBPCSSpinEdit,
     ESBPCSDescStats, TeEngine, Series, TeeProcs, Chart, ComCtrls, ESBPCSEdit,
     ESBPCSNumEdit, ESBPCSUrlLabel, ESBPCSUICtrl, ESBPCSGroupCtrls;

{$DEFINE UseMRNG}

type
     TForm1 = class (TForm)
          TypeRG: TESBRadioGroup;
          NoSE: TESBPosSpinEdit;
          ESBPCSLabel1: TESBPCSLabel;
          TestBtn: TButton;
          PC: TPageControl;
          Details: TTabSheet;
          Memo1: TMemo;
          Graph: TTabSheet;
          Chart1: TChart;
          Series1: TPointSeries;
          Shape: TESBPosFloatEdit;
          ShapeLbl: TESBPCSLabel;
          ALbl: TESBPCSLabel;
          A: TESBPosFloatEdit;
          B: TESBPosFloatEdit;
          BLbl: TESBPCSLabel;
          DF: TESBPosEdit;
          DFLbl: TESBPCSLabel;
          RG: TESBRadioGroup;
          NLbl: TESBPCSLabel;
          NValue: TESBPosEdit;
          PLbl: TESBPCSLabel;
          PValue: TESBPosFloatEdit;
          TabSheet1: TTabSheet;
          Chart2: TChart;
          Series2: TBarSeries;
          ESBUrlLabel1: TESBUrlLabel;
          StdDevLbl: TESBPCSLabel;
          StdDev: TESBPosFloatEdit;
          ESBUICtrl1: TESBUICtrl;
{$IFDEF XPMan}
          XPMan1: TXPManifest;
{$ENDIF}
          procedure TestBtnClick (Sender: TObject);
          procedure FormCreate (Sender: TObject);
          procedure TypeRGClick (Sender: TObject);
     private
          { Private declarations }
     public
          { Public declarations }
          UseXMean: Boolean;
     end;

var
     Form1: TForm1;

implementation

{$R *.DFM}
uses
     ESBPCSGlobals,
     ESBPCSMsgs,
     ESBPCSStatistics, ESBPCSVector, ESBPCSConvert,
     ESBPCS_RS_Math, ESBPCS_RS_DescStats,
     AMRandom, MRNG;

{$IFNDEF UseMRNG}

function MRNGRandom: Extended;
begin
     Result := 1;
end;
{$ENDIF}

procedure TForm1.TestBtnClick (Sender: TObject);
var
     I, N: Integer;
     X: TESBFloatVector;
     FMax: Extended;
     FMean: Extended;
     FMedian: Extended;
     FMin: Extended;
     FSortedValues: TESBFloatVector;
     FStdDev: Extended;
     FSum: Extended;
     FSumSq, SQ2: Extended;
     FVariance: Extended;
     FSkew: Extended;
     FKurtosis: Extended;
     FQtr1, FQtr3: Extended;
     FIQR: Extended;
     XMean, XStdDev: Extended;
     Diff: Extended;
     S: string;

     procedure GetStats;
     begin
          FSortedValues := Vector2FloatVector (X);
          QSortVector (FSortedValues);
          FSum := SumVector (X);
          FSumSq := SumSqVector (X);
          FMean := FSum / N;
          SQ2 := SumSqDiffVector (X, FMean);
          if N > 1 then
               FVariance := SQ2 / (N - 1)
          else
               FVariance := 0;
          FStdDev := Sqrt (FVariance);
          FMedian := GetMedian (FSortedValues);
          FMin := FSortedValues [0];
          FMax := FSortedValues [N - 1];
          if N > 3 then
          begin
               FKurtosis := GetSampleKurtosis (X, FMean, FStdDev)
          end
          else
               FKurtosis := 0;
          if N > 2 then
          begin
               FSkew := GetSampleSkew (X, FMean, FStdDev)
          end
          else
               FSkew := 0;
          GetQuartiles (FSortedValues, FQtr1, FQtr3);
          FIQR := FQtr3 - FQtr1;

          with Memo1.Lines do
          begin
               if N <= 1 then
                    Add (rsVectorIsEmpty)
               else
               begin
                    Add ('Calc. Mean:' + #9 + Float2EStr (FMean, 4));
                    if UseXMean then
                    begin
                         S := 'Expected Mean:' + #9 + Float2EStr (XMean, 4);
                         if XMean <> 0 then
                         begin
                              Diff := abs (FMean - XMean) * 100.0 / XMean;
                              S := S + #9 + Float2EStr (Diff, 4) + '%';
                         end;
                         Add (S);
                    end;
                    Add ('');
                    Add ('Calc. Std Dev:' + #9 + Float2EStr (FStdDev, 4));
                    if UseXMean then
                    begin
                         S := 'Expected Std Dev:' + #9 + Float2EStr (XStdDev, 4);
                         if XStdDev <> 0 then
                         begin
                              Diff := abs (FStdDev - XStdDev) * 100.0 / XStdDev;
                              S := S + #9 + Float2EStr (Diff, 4) + '%';
                         end;
                         Add (S);
                    end;
                    Add ('');
                    Add (rsDSNoOfValues + #9 + Int2EStr (Length (X)));
                    Add (rsDSMaximum + #9 + Float2EStr (FMax, 4));
                    Add (rsDSMinimum + #9 + Float2EStr (FMin, 4));
                    Add (rsDSRange + #9 + Float2EStr (FMax - FMin, 4));
                    Add (rsDSSum + #9 + Float2EStr (FSum, 4));
                    Add (rsDSSumSq + #9 + Float2EStr (FSumSq, 4));
                    Add (rsDSMedian + #9 + Float2EStr (FMedian, 4));
                    Add (rsDSVariance + #9 + Float2EStr (FVariance, 4));
                    if (N < 4) then
                         Add (rsDSNEKurtosis)
                    else
                         Add (rsDSKurtosis + #9 + Float2EStr (FKurtosis, 4));
                    if (N < 3) then
                         Add (rsDSNESkew)
                    else
                         Add (rsDSSkew + #9 + Float2EStr (FSkew, 4));
                    Add (rsDSQtr1 + #9 + Float2EStr (FQtr1, 4));
                    Add (rsDSQtr3 + #9 + Float2EStr (FQtr3, 4));
               end;
          end;
     end;

     procedure UpdateChart;
     var
          I: Integer;
     begin
          for I := 0 to N - 1 do
               Series1.AddY (X [I]);
     end;

     procedure UpdateHistogram;
     var
          I, J: Integer;
          NoClasses: Integer;
          ClassSize: Extended;
          GroupedData: TESBLWordVector;
          S: string;
     begin
          if N <= 50 then
               NoClasses := 5
          else if N <= 100 then
               NoClasses := 7
          else if N <= 200 then
               NoClasses := 9
          else if N <= 500 then
               NoClasses := 10
          else if N <= 1000 then
               NoClasses := 11
          else if N <= 5000 then
               NoClasses := 13
          else if N <= 20000 then
               NoClasses := 15
          else if N <= 50000 then
               NoClasses := 17
          else
               NoClasses := 19;

          SetLength (GroupedData, NoClasses);
          ClassSize := (FMax - FMin) / NoClasses;

          for I := 0 to NoClasses - 1 do
               GroupedData [I] := 0;

          for I := 0 to N - 1 do
          begin
               if (X [I] >= FMin) and (X [I] <= FMax) then
               begin
                    J := Trunc ((X [I] - FMin) / ClassSize + ESBTolerance);
                    if J <= NoClasses - 1 then
                         Inc (GroupedData [J]);
               end;
          end;

          for I := 0 to NoClasses - 1 do
          begin
               S := Float2EStr (FMin + I * ClassSize, 2) + ' - < '
                    + Float2EStr (FMin + (I + 1) * ClassSize, 2);
               Series2.AddBar (GroupedData [I], S, clTeeColor);
          end;
     end;
var
     fA, fB, fP: Extended;
     fDF, fN: Integer;
     Problem: Boolean;
begin
     N := NoSE.Value;
     SetLength (X, N);
     Memo1.Clear;
     Series1.Clear;
     Series2.Clear;

     Problem := False;
     case TypeRG.ItemIndex of
          0:
               begin
                    XMean := Shape.AsFloat;
                    XStdDev := StdDev.AsFloat;
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Normal (XMean, XStdDev, DelphiRandom);
                              1: X [I] := Random_Normal (XMean, XStdDev, MRNGRandom);
                         end;
                    end;
               end;
          1:
               begin
                    XMean := Shape.AsFloat;
                    XStdDev := Sqrt (Xmean);
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Gamma (XMean, DelphiRandom);
                              1: X [I] := Random_Gamma (XMean, MRNGRandom);
                         end;
                    end;
               end;
          2:
               begin
                    XMean := 1.0;
                    XStdDev := 1.0;
                    UseXMean := True;
                    for I := 0 to N - 1 do
                         case RG.ItemIndex of
                              0: X [I] := Random_Exponential (DelphiRandom);
                              1: X [I] := Random_Exponential (MRNGRandom);
                         end;
               end;
          3:
               begin
                    fA := A.AsFloat;
                    fB := B.AsFloat;
                    UseXMean := True;
                    try
                         XMean := fA / (fA + fB);
                         XStdDev := Sqrt (fA * fB / (fA + fB + 1.0)) / (fA + FB);
                         for I := 0 to N - 1 do
                         begin
                              case RG.ItemIndex of
                                   0: X [I] := Random_Beta (fA, fB, DelphiRandom);
                                   1: X [I] := Random_Beta (fA, fB, MRNGRandom);
                              end;
                         end;
                    except
                         WarningMsg ('Invalid Beta');
                         Problem := True;
                    end;
               end;
          4:
               begin
                    fDF := DF.AsInteger;
                    XMean := 0.0;
                    if fDF > 2.0 then
                         XStdDev := Sqrt (fDF / (fDF - 2.0))
                    else
                         XStdDev := 0.0;
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_t (fDF, DelphiRandom);
                              1: X [I] := Random_t (fDF, MRNGRandom);
                         end;
                    end;
               end;
          5:
               begin
                    fA := A.AsFloat;
                    fB := B.AsFloat;
                    UseXMean := False;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Inv_Gauss (fA, fB, DelphiRandom);
                              1: X [I] := Random_Inv_Gauss (fA, fB, MRNGRandom);
                         end;
                    end;
               end;
          6:
               begin
                    XMean := Shape.AsFloat;
                    XStdDev := Sqrt (Xmean);
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Poisson (XMean, DelphiRandom);
                              1: X [I] := Random_Poisson (XMean, MRNGRandom);
                         end;
                    end;
               end;
          7:
               begin
                    fN := NValue.AsInteger;
                    fP := PValue.AsFloat;
                    UseXMean := True;
                    XMean := fN * fP;
                    XStdDev := Sqrt (XMean * (1.0 - fP));
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Binomial1 (fN, fP, DelphiRandom);
                              1: X [I] := Random_Binomial1 (fN, fP, MRNGRandom);
                         end;
                    end;
               end;
          8:
               begin
                    fN := NValue.AsInteger;
                    fP := PValue.AsFloat;
                    UseXMean := True;
                    XMean := fN * fP;
                    XStdDev := Sqrt (XMean * (1.0 - fP));
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Binomial2 (fN, fP, DelphiRandom);
                              1: X [I] := Random_Binomial2 (fN, fP, MRNGRandom);
                         end;
                    end;
               end;
          9:
               begin
                    fN := NValue.AsInteger;
                    fP := PValue.AsFloat;
                    UseXMean := True;
                    XMean := fN * fP / (1 - fP);
                    XStdDev := Sqrt (XMean / (1.0 - fP));
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_neg_Binomial (fN, fP, DelphiRandom);
                              1: X [I] := Random_neg_Binomial (fN, fP, MRNGRandom);
                         end;
                    end;
               end;
          10:
               begin
                    XMean := Shape.AsFloat;
                    UseXMean := False;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_von_Mises (XMean, DelphiRandom);
                              1: X [I] := Random_von_Mises (XMean, MRNGRandom);
                         end;
                    end;
               end;
          11:
               begin
                    XMean := 0;
                    XStdDev := 1;
                    UseXMean := False;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Cauchy (DelphiRandom);
                              1: X [I] := Random_Cauchy (MRNGRandom);
                         end;
                    end;
               end;
          12:
               begin
                    fA := Shape.AsFloat;
                    UseXMean := False;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_Weibull (fA, DelphiRandom);
                              1: X [I] := Random_Weibull (fA, MRNGRandom);
                         end;
                    end;
               end;
          13:
               begin
                    fDF := DF.AsInteger;
                    XMean := fDF;
                    XStdDev := Sqrt (2 * fDF);
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_ChiSq (fDF, DelphiRandom);
                              1: X [I] := Random_ChiSq (fDF, MRNGRandom);
                         end;
                    end;
               end;
          14:
               begin
                    XMean := Shape.AsFloat;
                    XStdDev := StdDev.AsFloat;
                    UseXMean := True;
                    for I := 0 to N - 1 do
                    begin
                         case RG.ItemIndex of
                              0: X [I] := Random_LogNormal (XMean, XStdDev, DelphiRandom);
                              1: X [I] := Random_LogNormal (XMean, XStdDev, MRNGRandom);
                         end;
                    end;
               end;
     else
          begin
               WarningMsg ('No test yet!');
               Problem := True;
          end;
     end;

     if not Problem then
     begin
          GetStats;
          UpdateChart;
          UpdateHistogram;
     end;
end;

procedure TForm1.FormCreate (Sender: TObject);
begin
{$IFDEF XPMan}
     XPMan1 := TXPManifest.Create (Self);
{$ENDIF}

{$IFDEF UseMRNG}
     RG.Visible := True;
{$ENDIF}

     Caption := Caption + ' v' + FloatToStr (Version);
     Memo1.Clear;
     PC.ActivePage := Details;
     TypeRGClick (Sender);
     TestBtn.Click;
end;

procedure TForm1.TypeRGClick (Sender: TObject);
begin
     Randomize;
     MRandSeed (GetTickCount);

     ShapeLbl.Visible := False;
     Shape.Visible := False;
     ALbl.Visible := False;
     A.Visible := False;
     BLbl.Visible := False;
     B.Visible := False;
     DFLbl.Visible := False;
     DF.Visible := False;
     NLbl.Visible := False;
     NValue.Visible := False;
     PLbl.Visible := False;
     PValue.Visible := False;
     StdDevLbl.Visible := False;
     StdDev.Visible := False;
     case TypeRG.ItemIndex of
          0, 14:
               begin
                    ShapeLbl.Caption := 'Mean:';
                    ShapeLbl.Visible := True;
                    Shape.Visible := True;
                    StdDevLbl.Visible := True;
                    StdDev.Visible := True;
               end;
          1:
               begin
                    ShapeLbl.Caption := 'Shape:';
                    ShapeLbl.Visible := True;
                    Shape.Visible := True;
               end;
          3, 5:
               begin
                    ALbl.Caption := 'A:';
                    BLbl.Caption := 'B:';
                    ALbl.Visible := True;
                    A.Visible := True;
                    BLbl.Visible := True;
                    B.Visible := True;
               end;
          4, 13:
               begin
                    DFLbl.Visible := True;
                    DF.Visible := True;
               end;
          6:
               begin
                    ShapeLbl.Caption := 'Mean:';
                    ShapeLbl.Visible := True;
                    Shape.Visible := True;
               end;
          7, 8, 9:
               begin
                    NLbl.Visible := True;
                    NValue.Visible := True;
                    PLbl.Visible := True;
                    PValue.Visible := True;
               end;
          10, 12:
               begin
                    ShapeLbl.Caption := 'k:';
                    ShapeLbl.Visible := True;
                    Shape.Visible := True;
               end;
     end;
end;

end.

