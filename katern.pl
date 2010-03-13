#!/usr/bin/perl
use strict;
use warnings;

use PDF::API2;
use PDF::API2::Content;
use PDF::API2::Util;

use Getopt::Long;
use Pod::Usage;

use POSIX;

use List::Util qw[min max];

use constant cm => 2.54/72;
use constant {
  UP => 0,
  DOWN => 1
};
use constant {
  TOP => 0,
  SPINE => 1
};
use constant {
  LEFT => 0,
  RIGHT => 1
};

# read the command line options
my $scalePercent = 100;
my $outputFilename = "-";
my $sheetsPerSignature = -1; 
my $leadPages = 0;
my $medium = "a4";
my $debug = 0;
my $verbose = 0;
my $help = 0;
GetOptions(
  's=i' => \$scalePercent,
  'o=s' => \$outputFilename,
  'b=i' => \$sheetsPerSignature,
  'l=i' => \$leadPages,
  'm=s' => \$medium,
  'd' => \$debug,
  'v' => \$verbose,
  'h' => \$help
) or pod2usage(1);                                                      #print usage and exit with a 1
if ($help) {
  pod2usage(0)
};                                                                      #exit with a 0 and print the usage if -h is passed

# creating input pdf object
my $inPdf;
if (my $inputFilename = shift) {
  $inPdf = PDF::API2->open($inputFilename);
} else {
  $inPdf = PDF::API2->openScalar(join('',<>));
}

# calculate the parameters
my $scale = $scalePercent/100;

my %sizes = getPaperSizes();
$medium = lc($medium);
if ( ! grep {/$medium/} keys(%sizes) ) {
  print STDERR ("Error: invalid medium size\n");
  pod2usage(1); 
}
my $outXMediaSize;
my $outYMediaSize;
($outXMediaSize, $outYMediaSize) = @{$sizes{$medium}};

my $inPages = $leadPages + $inPdf->pages;
(my $incllx, my $inclly, my $incurx, my $incury) = $inPdf->openpage(1)->get_cropbox;
(my $inmllx, my $inmlly, my $inmurx, my $inmury) = $inPdf->openpage(1)->get_mediabox;
if ($scale == 0) {                                                      #forced 2-up scaling
  $scale = min($outXMediaSize/($incury - $inclly), $outYMediaSize/($incurx - $incllx));   
}
my $inXCropSize = $scale * ($incurx - $incllx);
my $inYCropSize = $scale * ($incury - $inclly);
my $inXMediaSize = $scale * ($inmurx - $inmllx);
my $inYMediaSize = $scale * ($inmury - $inmlly);

#TODO implement modeled algorithm for deciding on page orientation and
#TODO implement final rotation in function of landscape 

my $topfoldsPortrait = floor(log($outYMediaSize / $inYCropSize)/log(2));
my $spinefoldsPortrait = floor(log($outXMediaSize / $inXCropSize)/log(2));
my $topfoldsLandscape = floor(log($outXMediaSize / $inYCropSize)/log(2));
my $spinefoldsLandscape = floor(log($outYMediaSize / $inXCropSize)/log(2));
my $topfolds;
my $spinefolds;
if ($topfoldsPortrait + $spinefoldsPortrait < $topfoldsLandscape + $spinefoldsLandscape) {
  $topfolds = $topfoldsLandscape;
  $spinefolds = $spinefoldsLandscape;
  #my $switch = $outXMediaSize;
  #$outXMediaSize = $outYMediaSize;
  #$outYMediaSize = $outXMediaSize;
  ($outXMediaSize, $outYMediaSize) = ($outYMediaSize, $outXMediaSize);
} else {
  $topfolds = $topfoldsPortrait;
  $spinefolds = $spinefoldsPortrait;
}
my $folds = $topfolds + $spinefolds;
my $matrixXSize = 2**$spinefolds;
my $matrixYSize = 2**$topfolds;
my $cellXSize = $outXMediaSize / $matrixXSize;
my $cellYSize = $outYMediaSize / $matrixYSize;
my $inPagesPerOutPage = $matrixXSize * $matrixYSize;

my $signatures;
my $inPagesPerSignature;
if ( $sheetsPerSignature == 0 ) {                                       #user requests single signature
  $signatures = 1;
  $sheetsPerSignature = ceil($inPages / $inPagesPerOutPage / 2);
  $inPagesPerSignature = 2 * $sheetsPerSignature * $inPagesPerOutPage;
} elsif ( $sheetsPerSignature > 0 ) {                                  #user-defined value
  $inPagesPerSignature = 2 * $sheetsPerSignature * $inPagesPerOutPage;
  $signatures = ceil($inPages / $inPagesPerSignature);
} else {
  $inPagesPerSignature = 24;                                            #default: final fold spans 8 sheets
  $signatures = ceil($inPages / $inPagesPerSignature);
  $sheetsPerSignature = ceil($inPagesPerSignature / 2 / $inPagesPerOutPage);
}
my $layers = $inPagesPerSignature / 2;
my $outPages = $signatures * $sheetsPerSignature * 2;

my $Yoffset = ($cellYSize - $inYCropSize) / 2;                          #should be positive
my $Xoffset = $scale * ($inmllx - $incllx);                             #should be zero


if ($debug || $verbose)  {
  printf STDERR ("Input pages%s: %d\n",$leadPages ? " (inc. empty lead pages)" : "",$inPages);
  printf STDERR ("outXMediaSize: %.2fcm\n",$outXMediaSize*cm);
  printf STDERR ("outYMediaSize: %.2fcm\n",$outYMediaSize*cm);
  printf STDERR ("spinefolds: %d\n",$spinefolds);
  printf STDERR ("topfolds: %d\n",$topfolds);
  printf STDERR ("signatures: %d\n",$signatures);
  printf STDERR ("pages per signature: %d\n",$inPagesPerSignature);
  printf STDERR ("sheets per signature: %d\n",$sheetsPerSignature);
  printf STDERR ("scale: %d\n",100*$scale);
  printf STDERR ("Yoffset: %.2fcm\n",$Yoffset*cm);
  printf STDERR ("Xoffset: %.2fcm\n",$Xoffset*cm);
}

# setup the model as a closed book
my @model;
my $nr;
if ($debug)  {
  printf STDERR ("%6s%6s%6s%6s%6s%6s%6s%6s%6s\n","sig","signr","nr","layer","face","x","y","rot","align");
}
for ($nr=1; $nr<=$inPages; $nr++) {                                     #model the closed book lying face down
    $model[$nr] = {
      SIG   =>  int(($nr - 1) / $inPagesPerSignature) + 1,             #sequence number of the signature
      SIGNR =>  ($nr - 1) % $inPagesPerSignature + 1,                  #page number within the signature 
      FACE  =>  $nr % 2 == 1 ? DOWN : UP,                               #odd pages start face down, even pages face up
      LAYER =>  int((($nr - 1) % $inPagesPerSignature ) / 2) + 1,      #page 1 and 2 are layer 1 and so on...
      X     =>  1,                                                      #all pages start bottom left in the matrix
      Y     =>  1,
      ROT   =>  0,
      ALIGN =>  RIGHT                                                  
    };
    if ($debug)  {
      printf STDERR ("%6d%6d%6d%6d%6s%6d%6d%6d\n",
            $nr,
            $model[$nr]->{SIG},
            $model[$nr]->{SIGNR},
            $model[$nr]->{LAYER},
            $model[$nr]->{FACE} == DOWN ? "DOWN" : "UP",
            $model[$nr]->{X},
            $model[$nr]->{Y},
            $model[$nr]->{ROT},
            $model[$nr]->{ALIGN} == LEFT ? "LEFT" : "RIGHT"
          );
    }
}
if ($debug)  {
  printf STDERR ("\n");
}

# calculate the page positions by virtually unfolding the model
my $foldDirection = SPINE;
my $maxX = 1;
my $maxY = 1;
my $fold;
for ($fold=1; $fold<=$folds; $fold++) {                                 #model unfolding the signatures, top-right-top-right-etc...
    if ($debug)  {
      printf STDERR ("Starting from %d layers and folding to the %s.\n",$layers,$foldDirection == SPINE ? "right" : "top");
      printf STDERR ("%6s%6s%6s%6s%6s%6s%6s\n","nr","layer","face","x","y","rot","align");
    }
    $maxX = $foldDirection == SPINE ? 2 * $maxX : $maxX;
    $maxY = $foldDirection == TOP ? 2 * $maxY : $maxY;
    for ($nr=1; $nr<=$inPages; $nr++) {
        if ($model[$nr]->{LAYER} > $layers / 2) {                       #the top half of the folded sheets are unfolded
            $model[$nr]->{FACE} = $model[$nr]->{FACE} == DOWN ? UP : DOWN;
            $model[$nr]->{LAYER} = $layers - $model[$nr]->{LAYER} + 1;
            $model[$nr]->{X} = $foldDirection == SPINE ? $maxX - $model[$nr]->{X} + 1: $model[$nr]->{X};
            $model[$nr]->{Y} = $foldDirection == TOP ? $maxY - $model[$nr]->{Y} + 1: $model[$nr]->{Y};
            $model[$nr]->{ROT} = $foldDirection == TOP ? 180 - $model[$nr]->{ROT} : $model[$nr]->{ROT};
            $model[$nr]->{ALIGN} = $foldDirection == TOP ? $model[$nr]->{ALIGN} : 1 - $model[$nr]->{ALIGN};
        }
        if ($debug)  {
          printf STDERR ("%6d%6d%6s%6d%6d%6d%6s\n",
                  $nr,
                  $model[$nr]->{LAYER},
                  $model[$nr]->{FACE} == DOWN ? "DOWN" : "UP",
                  $model[$nr]->{X},
                  $model[$nr]->{Y},
                  $model[$nr]->{ROT},
                  $model[$nr]->{ALIGN} == LEFT ? "LEFT" : "RIGHT"
                );
        }
    }
    $layers = $layers / 2;
    $foldDirection = $foldDirection == TOP ? SPINE : TOP;
    if ($debug)  {
      printf STDERR ("\n");
    }
}


#create the blank pages for the output
my $outPdf = PDF::API2->new;
my @outPage;
for ($nr=1; $nr<=$outPages; $nr++) {               
    $outPage[$nr] = $outPdf->page;
    $outPage[$nr]->mediabox($outXMediaSize, $outYMediaSize);
}

#draw a folding line on each first page of a signature (= starting fold)
for ($nr=1; $nr<=$outPages; $nr+=2*$sheetsPerSignature) {
  my $line = $outPage[$nr]->gfx;
  $line->save;
  $line->linedash((10,10));
  if ($spinefolds > $topfolds) {                                        #vertical line
    $line->move($outXMediaSize/2,0);
    $line->line($outXMediaSize/2,$outYMediaSize);
  } else {                                                              #horizontal line
    $line->move(0,$outYMediaSize/2);  
    $line->line($outXMediaSize,$outYMediaSize/2);
  }
  $line->stroke;
  $line->restore;
}

#write on each page the number of the signature and the number within the signature
my %font = (
     Helvetica => {
         Bold   => $outPdf->corefont( 'Helvetica-Bold',    -encoding => 'latin1' ),
         Roman  => $outPdf->corefont( 'Helvetica',         -encoding => 'latin1' ),
         Italic => $outPdf->corefont( 'Helvetica-Oblique', -encoding => 'latin1' ),
     },
);
for ($nr=1; $nr<=$outPages; $nr++) {
  my $text = $outPage[$nr]->text;
  $text->save;
  $text->font($font{'Helvetica'}{'Roman'}, 0.2/cm );
  $text->translate($outXMediaSize/2-1/cm, 1/cm );
  $text->text(sprintf("%d - %d",($nr - 1) / (2*$sheetsPerSignature) + 1,($nr - 1) % (2*$sheetsPerSignature) + 1));
  $text->restore;
}

# flip the pages facing down
for ($nr=1; $nr<=$inPages; $nr++) {
  if ($model[$nr]->{FACE} == DOWN) {                       
      $model[$nr]->{X} = $matrixXSize - $model[$nr]->{X} + 1;
      $model[$nr]->{ALIGN} = 1 - $model[$nr]->{ALIGN};
  }
}

# put the pages on the right positions as in the unfolded model
for ($nr=1; $nr<=$inPages; $nr++) {
    my $insertedpageXobject = $outPdf->importPageIntoForm($inPdf, $nr - $leadPages); #fetch the page as Xobject
    my $outPageNr = ($model[$nr]->{SIG} - 1) * $sheetsPerSignature * 2 + 
                    ($model[$nr]->{LAYER} - 1) * 2 +
                    ($model[$nr]->{FACE} == DOWN ? 1 : 2);
    if ($debug)  {
      printf STDERR ("Putting page %d on output page %d, ",$nr,$outPageNr);
    }
    my $x = ($model[$nr]->{X} - 1) * $cellXSize;
    my $y = ($model[$nr]->{Y} - 1) * $cellYSize;
    if ( $model[$nr]->{ROT} == 0 ) {
      if ( $model[$nr]->{ALIGN} == LEFT ) {
        $x += $Xoffset;
        $y += $Yoffset;
      } else {
        $x += $cellXSize - $Xoffset - $inXMediaSize;
        $y += $Yoffset;
      }
    } else {
      if ( $model[$nr]->{ALIGN} == LEFT ) {
        $x += $Xoffset + $inXMediaSize;
        $y += $Yoffset + $inYMediaSize;
      } else {
        $x += $cellXSize - $Xoffset;
        $y += $Yoffset + $inYMediaSize;
      }
    }
    my $insertedpageGfx = $outPage[$outPageNr]->gfx;                    #create a new graphics object
    $insertedpageGfx->save;                                             #otherwise the previous tranformation is added
    $insertedpageGfx->transform(
                 -translate  => [$x,$y],
                 -rotate     => $model[$nr]->{ROT},
                 -scale      => [$scale,$scale],
    );                                                                  #define a transformation on the graphics object
    $insertedpageGfx->formimage($insertedpageXobject, 0, 0, 1);         #drop the page Xobject in the graphics object
    $insertedpageGfx->restore;
    if ($debug)  {
      printf STDERR ("with a shift of (%d cm,%d cm) and a rotation of %d\n",$x*cm,$y*cm,$model[$nr]->{ROT});
    }
}

#$outPage->rotate(90);
if ($outputFilename =~ /^-$/) {
  print $outPdf->stringify;
} else {
  $outPdf->saveas($outputFilename);
}


__END__

=head1 NAME

Signaturize - print sheets that can be folded into bookbinding signatures

=head1 SYNOPSIS

signaturize [options] [<file>]

Options:
-s <value>  set the scale factor;
-o <file>   set the output filename
-b <value>  set the number of sheets used per signature;
-m <string> set the output medium size;
-d          turn on debug messages;
-h          print help/usage

=head1 OPTIONS

=over 8

=item B<-s>

A scale factor can be applied. If no scale factor is specified, the scale is not changed.

A special value of 0 can be entered to force an automatic scaling for 2-up printing. Combine with '-b 0' for "booklet" printing.

=item B<-o>

The output will be written to the file. If no file is specified, the output is written to the standard output console.

=item B<-b>

Signatures can be composed out of multiple sheets of paper. If no bundle amount is specified, an amount of sheets is determined that limits the final fold to be applied to 8 layers of paper.

A special value of 0 can be entered to force a single signature to be produced. . Combine with '-s 0' for "booklet" printing.

=item B<-l>

A number of empty lead pages can be added to the first signature.

=item B<-m>

The medium size of the output. If no medium size is specified, the default is 'a4'. Valid sizes are '4A', '2A', 'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', '4B', '2B', 'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'LETTER', 'BROADSHEET', 'LEDGER', 'TABLOID', 'LEGAL', 'EXECUTIVE', and '36X36'.

=item B<-d>

Debugging messages are printed to standard error console.

=item B<-v>

Be verbose. A number of parameters are printed to standard error console. 

=item B<-h>

prints information on how to run the program

=back

=head1 DESCRIPTION

B<Signaturize> processes a PDF-file into another PDF-file that can be used to produce signatures for bookbinding. A signature is a stack of papers that can be transformed into a booklet by folding the pages. A book can be composed of a series of signatures.

The output is always in portrait position, to facilitate duplex printing. The output contains numbers in the margin that help to correctly position the pages, by defining the orientation, the order within the signature and the order of subsequential signatures.

The first fold for each signature is indicated on the first page of the (unfolded) signature. If the fold is indicated by a horizontal line, the top half of the page(s) if folded over the bottom half. The next fold is made by folding the right half of the pages over the left half. This is repeated until the signature is completely folded. The the fold is indicated by a vertical line, the same folding technique is used, starting by folding the right half of the page(s) over the left half.

=cut
