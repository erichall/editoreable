# TODO
-[X]  handle when rows expands to multiple rows, can no longer use 21px as height... :~>

-[X]  handle selections for empty rows

-[X]  remove fetching row width and chars directly from the dom cus it messes up when removing chars.. we should fetch it from the buffer instead and keep a fixed char-width as a style maybe??

-[X]   Remove active-line and just get-it from cursor y pos?

-[X]   it seems to be impossible to rely on pre-line word wrapping since you are not able to place the cursor on the invisible \n char that is not shown when wrapping.. I guess we have to roll back to manually dealing with rows, I guess we are better prepared this time

-[ ] fix the toolbar

-[X] fix the gutter with row numbers

-[X] fix copy

-[X] fix cut

-[X] fix paste

-[X] fix so when you copy you don't type in c or v

-[X] hide the active row if selection is present

-[X] disable caret flashing while typing?

-[X] fix when right clicking on selection so it still active

-[X] add all different keys

-[X] refactor select rows

-[X] fix scrolling when clicking on a row

-[X] fix relative mouse cords for rows numbers and textarea, its brooooken

-[X] Think of something better to separate cursor movement from the rest of the jazz, and especially something that does not involve coordinates and offsetting 
with a dam gutter......

-[X] add scrolling when cursor moves

-[X] fix bug when copy empty rows, the \n seems to no follow

-[X] fix when cut whole row, the row should not collapse

-[X] fix undo

-[X] fix delete selections on backspace

-[X] fix double click and triple click to select stuff

-[] fix cursor movement when cut, should end up on last on that row i guess

-[X] refactor into new cursor handling

-[X] fix bug when pressing enter

-[ ] fix adjust fontsize

-[ ] fix e2e tests

-[X] fix row/line indicator for the cursor position

-[ ] lose the coupling between editor width and char-width somehow?

-[ ] fix callbacks and necessary props

