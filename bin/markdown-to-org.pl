#!/usr/bin/env perl
# --------------------------------------------------------------------------
#  Simple Markdown to Org-Mode Conversion Script
# --------------------------------------------------------------------------

for $input (@ARGV) {
    my $output = $input;
    $output =~ s/.[a-z]+$/.org/;
    open (IN, $input) or die "Can't read $input: $!\n";
    my @lines = <IN>;
    close(IN);

    my %links;
    for (@lines) {
        /^\s*\[(.*)\]: (.*)/;
        $links{$1} = $2;
    }

    my @newlines;
    my $linenum = 0;
    my $codeState = 0;
    for (@lines) {
        ## Ignore all reference links, like [blah]: http://...
        if (! /^\s*\[.*\]: /) {

            ## Code Section
            if (/^    / && $newlines[$linenum - 1] =~ /^\s*$/ && $codeState == 0) {
                $newlines[$linenum++] = "#+BEGIN_SRC \n";
                $codeState = 1;
            }
            elsif (!/^    / && !/^\s*$/ && $codeState == 1) {
                $newlines[$linenum - 1] = "#+END_SRC \n";
                $newlines[$linenum] = "\n";
                $linenum+=2;
                $codeState = 0;
            }

            if ($codeState == 1) {
                s/^  //;
            }

            ## If a header has dashes underneath it, we convert
            ## that to a '#' header style and ignore the dashed line:
            if (/^---+/ && $newlines[$linenum - 1] !~ /^\s*$/) {
                $newlines[$linenum - 1] = '## ' . $newlines[$linenum - 1];
            }
            elsif (/^===+/ && $newlines[$linenum - 1] !~ /^\s*$/) {
                $newlines[$linenum - 1] = '# ' . $newlines[$linenum - 1];
            }
            else {
                $newlines[$linenum] = $_;
                $linenum++;
            }
        }
    }
    push @newlines, "#+END_SRC \n" if ($codeState == 1);

    for (@newlines) {
        ## Convert the links to the org-mode style:
        s/\[(.*?)\]\[(.*?)\]/\[\[$links{$2}\]\[$1\]\]/g;

        ## Convert the code style:
        s/`(.*?)`/=$1=/g;

        ## Convert the italics:
        s/\s\*([^\*]+?)\*(\s*)/ \/$1\/$2/g;
        s/^\*([^\*]+?)\*(\s*)/\/$1\/$2/g;     # Line Start

        ## Convert the bold:
        s/\s\*\*([^\*]+?)\*\*(\s*)/ \*$1\*$2/g;
        s/^\*\*([^\*]+?)\*\*(\s*)/\*$1\*$2/g;  # Line Start

        s/^# /* /;     # Converting headers
        s/^## /* /;
        s/^### /** /;
        s/^#### /*** /;
        s/^##### /**** /;
        s/^###### /***** /;

    }
    # print "Converting " . $#lines . " lines from $input to $output\n";
    print @newlines;
}
