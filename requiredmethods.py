class Requireable:
    __requiredCallsPerClassId = dict()

    @staticmethod
    def __classIdFromClass(cls):
        clsId = cls.__qualname__
        return clsId

    @staticmethod
    def __classIdFromMethod(clsFn):
        callPath = clsFn.__qualname__
        clsId = ".".join(callPath.split(".")[:-1])
        return clsId

    @staticmethod
    def __getRequiredCallsFor(clsId):
        reqsPerId = Requireable.__requiredCallsPerClassId
        if clsId not in reqsPerId:
            reqsPerId[clsId] = set()
        return reqsPerId[clsId]

    @staticmethod
    def __markIsRequiredFor(clsFn, clsId):
        callsSet = Requireable.__getRequiredCallsFor(clsId)
        callsSet.add(clsFn)

    def __new__(cls, *args, **kwargs):
        selfObj = super(Requireable, cls).__new__(cls, *args, **kwargs)
        selfObj.__copyRequiredCalls()
        return selfObj

    def __copyRequiredCalls(selfObj):
        selfObj.__requiredCalls = set()
        for currCls in selfObj.__class__.__mro__:
            currClsId = selfObj.__classIdFromClass(currCls)
            currReqFns = selfObj.__getRequiredCallsFor(currClsId)
            selfObj.__requiredCalls.update(currReqFns)
            currCls = super(currCls, selfObj)

    def __markRequiredCalled(selfObj, clsFn):
        selfObj.__requiredCalls.remove(clsFn)

    def __checkRequiredFnsCalledBefore(selfObj, clsFn):
        if len(selfObj.__requiredCalls) > 0:
            fnCalled = clsFn.__name__
            fnsNotCalled = "', '".join(fn.__name__ for fn in selfObj.__requiredCalls)
            raise RuntimeError("Ensured function '{}' was called before required functions '{}'".format(fnCalled, fnsNotCalled))


def _markIsRequired(clsFn):
    clsId = Requireable._Requireable__classIdFromMethod(clsFn)
    Requireable._Requireable__markIsRequiredFor(clsFn, clsId)

def _markRequiredCalled(selfObj, clsFn):
    selfObj._Requireable__markRequiredCalled(clsFn)

def _checkRequiredFnsCalledBefore(selfObj, clsFn):
    selfObj._Requireable__checkRequiredFnsCalledBefore(clsFn)

# decorator that marks a function as "required" for ensured calls
def requiredCall(clsFn):
    _markIsRequired(clsFn)
    # TODO: is there a way to ensure self is defined/ignore if it isn't?
    def trackedFn(self, *args, **kwargs):
        if isinstance(self, Requireable):
            _markRequiredCalled(self, clsFn)
        return clsFn(self, *args, **kwargs)
    return trackedFn

# decorator that checks all required functions have been called
def ensuredCall(clsFn):
    def ensuredFn(self, *args, **kwargs):
        if isinstance(self, Requireable):
            _checkRequiredFnsCalledBefore(self, clsFn)
        return clsFn(self, *args, **kwargs)
    return ensuredFn

if __name__ == "__main__":
    class A(Requireable):
        @requiredCall
        def __init__(self):
            pass

        @requiredCall
        def runBefore(self):
            pass

        @ensuredCall
        def fn(self):
            pass

    class C:
        pass
    class B(A, C):
        def __init__(self):
            super().__init__()
    b = B()
    b.runBefore()
    b.fn()
