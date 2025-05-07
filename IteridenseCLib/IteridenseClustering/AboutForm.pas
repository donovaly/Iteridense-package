unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  LCLIntf;

type

  { TAboutFormF }

  TAboutFormF = class(TForm)
    OKBB: TBitBtn;
    ReadmeLinkL: TLabel;
    NameL: TLabel;
    DescriptionTextST: TStaticText;
    CSVInfoST: TStaticText;
    CSVInfoL: TLabel;
    VersionNumberL: TLabel;
    UsageTextST: TStaticText;
    SourceCodeTextL: TLabel;
    GitHubLinkL: TLabel;
    procedure CSVInfoLClick(Sender: TObject);
    procedure GitHubLinkLClick(Sender: TObject);
    procedure ReadmeLinkLClick(Sender: TObject);
  private

  public

  end;

var
  AboutFormF: TAboutFormF;

implementation

{$R *.lfm}

procedure TAboutFormF.ReadmeLinkLClick(Sender: TObject);
begin
  OpenURL('https://codeberg.org/Soloof/Iteridense/raw/branch/main/Paper/Iteridense-clustering.pdf');
end;

procedure TAboutFormF.GitHubLinkLClick(Sender: TObject);
begin
  OpenURL('https://github.com/donovaly/Iteridense-package/tree/main/IteridenseCLib/IteridenseClustering');
end;

procedure TAboutFormF.CSVInfoLClick(Sender: TObject);
begin
  OpenURL('https://github.com/donovaly/Iteridense-package/blob/main/IteridenseCLib/IteridenseClustering/README.md');
end;


end.

