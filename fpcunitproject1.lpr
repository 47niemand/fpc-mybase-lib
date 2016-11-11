program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  fpcunittestrunner,
  lazcontrols,
  testcasebaseinterface,
  testcaseforbaseentry,
  testcaseforbasemap,
  testcaseforbasemapconcurency,
  testcaseforbasevalue,
  testcaseforbvalueconcurency,
  testcaseforlistsandsets,
  TestCaseNeuronNet;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
