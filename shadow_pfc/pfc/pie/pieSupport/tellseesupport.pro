% Copyright PDC

namespace pfc\pie

implement tellSeeSupport

class facts
    seeing_name : (string) determ.
    telling_name : string := "".
    seeing : inputStream := erroneous.
    telling : outputStream := erroneous.
    oldStream : outputStream := erroneous.
    streamChanged : boolean := false.

clauses
    hasSeeing() :-
        not(isErroneous(seeing)).

    hasTelling() :-
        not(isErroneous(telling)).

clauses
    tell(Filename) :-
        if not(isErroneous(telling)) then
            stdio::outputStream := oldStream,
            telling:close()
        end if,
        oldStream := stdio::outputStream,
        streamChanged := true,
        telling := outputStream_file::create(Filename),
        stdio::outputStream := telling,
        telling_name := Filename.

clauses
    told() :-
        telling_name := "",
        if hasTelling() then
            stdio::outputStream := oldStream,
            telling:close(),
            telling := erroneous
        end if.

clauses
    see(Filename) :-
        if not(isErroneous(seeing)) then
            seeing:close()
        end if,
        seeing := inputStream_file::openFile(Filename),
        stdio::inputStream := seeing,
        retractall(seeing_name(_)),
        assert(seeing_name(Filename)).

clauses
    seeingP(Filename) :-
        seeing_name(Filename).

clauses
    seen() :-
        retractAll(seeing_name(_)),
        if not(isErroneous(seeing)) then
            seeing:close(),
            seeing := erroneous
        end if.

clauses
    handle_eof() :-
        seeing_name(_),
        seeing:endOfStream().

clauses
    closeSeeing_ifPresent() :-
        if hasSeeing() then
            Seeing = seeing,
            Seeing:close(),
            seeing := erroneous
        end if.

clauses
    closeTelling_ifPresent() :-
        if hasTelling() then
            Telling = telling,
            Telling:close(),
            telling := erroneous
        end if.

end implement tellSeeSupport
