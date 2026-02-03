% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement outputStream_processEvents

delegate
    interface outputStream to stream

facts
    stream : outputStream [constant].

clauses
    new(Stream) :-
        stream := Stream.

clauses
    write(...) :-
        stream:write(...),
        _IsSuccessful = vpi::processEvents().

clauses
    writef(FormatString, ...) :-
        stream:writef(FormatString, ...),
        _IsSuccessful = vpi::processEvents().

clauses
    nl() :-
        stream:nl(),
        _IsSuccessful = vpi::processEvents().

end implement outputStream_processEvents
