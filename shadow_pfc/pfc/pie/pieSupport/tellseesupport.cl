% Copyright PDC

namespace pfc\pie

class tellSeeSupport

predicates
    tell : (string).
    told : ().

properties
    telling_name : string (o).
    seeing : inputStream.
    telling : outputStream.
    oldStream : outputStream.
    streamChanged : boolean (o).

predicates
    hasSeeing : () determ.
    hasTelling : () determ.

predicates
    see : (string).
    seeingP : (string Filename [out]) determ.
    seen : ().

predicates
    handle_eof : () determ.

predicates
    closeSeeing_ifPresent : ().
    closeTelling_ifPresent : ().

end class tellSeeSupport
