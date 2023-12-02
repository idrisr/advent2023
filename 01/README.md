--- day 1: trebuchet?! ---

something is wrong with global snow production, and you've been selected to take
a look. the elves have even given you a map; on it, they've used stars to mark
the top fifty locations that are likely to be having problems.

you've been doing this long enough to know that to restore snow operations, you
need to check all fifty stars by december 25th.

collect stars by solving puzzles. two puzzles will be made available on each day
in the advent calendar; the second puzzle is unlocked when you complete the
first. each puzzle grants one star. good luck!

you try to ask why they can't just use a weather machine ("not powerful enough")
and where they're even sending you ("the sky") and why your map looks mostly
blank ("you sure ask a lot of questions") and hang on did you just say the sky
("of course, where do you think snow comes from") when you realize that the
elves are already loading you into a trebuchet ("please hold still, we need to
strap you in").

as they're making the final adjustments, they discover that their calibration
document (your puzzle input) has been amended by a very young elf who was
apparently just excited to show off her art skills. consequently, the elves are
having trouble reading the values on the document.

the newly-improved calibration document consists of lines of text; each line
originally contained a specific calibration value that the elves now need to
recover.

on each line
    the calibration value can be found
    by combining
        first digit and the
        last digit
        (in that order)
        to form a single two-digit number.

for example:

1abc2        12
pqr3stu8vwx  38
a1b2c3d4e5f  15
treb7uchet   77
            ---
            142

consider your entire calibration document. what is the sum of all of the
calibration values?
